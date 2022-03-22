{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Bril.Structure.SSA (ssa, ssa') where

import           Bril.Lang.AST
import           Bril.Structure.CFG
import           Control.Lens
import           Data.Foldable
import           Data.Hashable
import           Data.Maybe
import           Data.Tree
import           Data.Tuple
import           GHC.Generics
import           Util.Graph
import           Util.Misc
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.Text           as T

-- | all the variables ever assigned to in
--   the given list of instructions
variables :: [Instruction] -> M.HashMap Ident Type
variables = M.fromList . (fn =<<)
  where
    fn (Value _ (Just (Assignment d t))) = [(d, t)]
    fn _                                 = []

-- | all the phi instructions in the given list of instructions
--   as a set of the 4-tuple of destination, type, variable, label
phis :: [Instruction] -> S.HashSet (Ident, Type, Ident, Ident)
phis = S.fromList . (fn =<<)
  where
    fn (Value (Phi ls) (Just (Assignment d t))) = (\(v, l) -> (d, t, v, l)) <$> ls
    fn _                                        = []

-- | represents a phi node for a variable
--   which contains the variable, the
--   destination, and the list of variables
--   and labels
data PhiNode = PhiNode Ident Ident [(Ident, Ident)]
             deriving (Show, Eq, Generic, Hashable)

-- | returns a map from blocks to the variables
--   that need to have a phi node at the header
--   of the block
addPhi :: MultiMap Ident Ident           -- the successor map
       -> M.HashMap Ident [Instruction]  -- blocks and instructions
       -> MultiMap Ident Ident           -- domination frontier for each block
       -> MultiMap Ident PhiNode         -- return the phi nodes for each block
addPhi succs instrs front = M.union result $ S.empty <$ instrs
  where
    result       = finalise <$> transpose (M.mapWithKey (phi S.empty) varbs)
    varbs        = transpose $ M.keysSet . variables <$> instrs
    finalise vs  = S.map (\s -> PhiNode s s []) vs
    -- go over the given blocks and return the
    -- blocks for which v needs to have a phi node
    phi x v blocks
      | null blocks = x
      | otherwise   = phi phis v (S.difference phis x)
      where
        phis = S.union x $ unions $ S.map (front M.!) blocks

-- | defines a variable stack with an internal counter
--   which is incremented whenever a new variable name
--   is generates
data VarStack = VarStack
              { _variable :: Ident
              , _stack    :: [Ident]
              , _counter  :: Int
              }
              deriving (Show, Eq)

-- | make lenses for the stack
makeLenses ''VarStack

-- | create an empty stack
empty :: Ident -> VarStack
empty i = VarStack i [] 0

-- | create a singleton stack
singleton :: Ident -> VarStack
singleton i = VarStack i [i] 0

-- | whether a variable is declared in this stack
declared :: VarStack -> Bool
declared (VarStack _ [] _) = False
declared _                 = True

-- | generate a new value for the
--   stack and push it to the top,
--   incrementing the counter
new :: VarStack -> VarStack
new (VarStack (Ident i) xs c) = VarStack (Ident i) (n : xs) $ c + 1
  where
    n = Ident . T.pack $ T.unpack i ++ "." ++ show c

-- | given the current stack, give a new variable name
curr :: VarStack -> Ident
curr (VarStack i [] _)      = i
curr (VarStack _ (x : _) _) = x

-- | rename an instruction with the given
--   variable stack and possibly updte the stack
renamePhi :: M.HashMap Ident VarStack -> PhiNode -> (M.HashMap Ident VarStack, PhiNode)
renamePhi vars (PhiNode v _ xs) = (vars', PhiNode v v' xs)
  where
    vars' = M.adjust new v vars
    v' = curr $ vars' M.! v

-- | rename an instruction with the given
--   variable stack and possibly updte the stack
renameInstr :: M.HashMap Ident VarStack -> Instruction -> (M.HashMap Ident VarStack, Instruction)
renameInstr vars instr = case instr' of
                           Value x (Just (Assignment d t)) -> upd x d t
                           _                               -> (vars, instr')
  where
    instr'    = mapArgs (\v -> curr $ vars M.! v) instr
    upd x d t = let vs = M.adjust new d vars in
                let d' = curr $ vs M.! d in
                (vs, Value x . Just $ Assignment d' t)

-- | take the CFG of a function, the added phi nodes, and
--   the list of instructions and rename the variables
--   recursively into SSA form
rename :: MultiMap Ident Ident                                                              -- the successors map
       -> M.HashMap Ident [Instruction]                                                     -- blocks map
       -> MultiMap Ident PhiNode                                                            -- phi nodes
       -> M.HashMap Ident VarStack                                                          -- variable stack
       -> Tree Ident                                                                        -- current node in the dom tree
       -> (M.HashMap Ident [Instruction], MultiMap Ident PhiNode, M.HashMap Ident VarStack) -- return the updated instrs, phi nodes, and stack
rename succs instrs phis stack (Node b cs) = fin $ foldl' rec (instrs', phis'', stack'') cs
  where
    -- update the stack with phi nodes in this block
    fn (s, ps) p    = _2 %~ flip S.insert ps $ renamePhi s p
    (stack', ps)    = foldl' fn (stack, S.empty) $ phis M.! b
    -- update the stack with the instrs in this block
    gn (s, is) i    = _2 %~ (\x -> is ++ [x]) $ renameInstr s i
    (stack'', is)   = foldl' gn (stack', []) $ instrs M.! b
    -- update the instrs and phi nodes for the current block
    instrs'         = M.insert b is instrs
    phis'           = M.insert b ps phis
    -- update the phi nodes in this block's successors
    hn (PhiNode v u xs)
      | declared $ stack'' M.! v = PhiNode v u ((curr $ stack'' M.! v, b) : xs)
      | otherwise                = PhiNode v u xs
    phis''          = foldl' (flip (M.adjust $ S.map hn)) phis' $ succs M.! b
    -- recurse on the children nodes
    rec (i, p, s) n = rename succs i p s n
    -- return the original stack with updated counter
    pop i s         = counter .~ (s ^. counter) $ (stack M.! i)
    fin (i, p, s)   = (i, p, M.mapWithKey pop s)

-- | combine the phi nodes with the instruction blocks
combinePhi :: M.HashMap Ident Type
           -> M.HashMap Ident [Instruction]
           -> MultiMap Ident PhiNode
           -> M.HashMap Ident [Instruction]
combinePhi types instrs phis = M.mapWithKey append instrs
  where
    instr (PhiNode v u vs)  = Value (Phi vs) (Just (Assignment u $ types M.! v))
    append v is             = after (instr <$> S.toList (phis M.! v)) is
    -- append the phi instruction after the label
    after ps (Label l : xs) = Label l : ps ++ xs
    after ps xs             = ps ++ xs

-- | for the phi nodes of each block, try to merge
--   them together
--
--   converts things like
--
--   ```
--   x: int = phi .here y;
--   z: int = phi .here x .there z;
--   ```
--
--   into
--
--   x: int = phi .here y;
--   z: int = phi .here y .there z;
--   ```
mergePhi :: [Instruction] -> [Instruction]
mergePhi = (^. _2) . foldl' merge (M.empty, [])
  where
    -- go over this phi instruction and prepare to merge
    merge (phis, instrs) (Value (Phi ls) (Just (Assignment d t))) = (phis', instrs ++ [instr])
      where
        ls'       = fn <$> ls
        fn (v, l) = if M.member v phis && M.member l (phis M.! v)
                    then ((phis M.! v) M.! l, l) else (v, l)
        phis'     = M.insert d (M.fromList $ swap <$> ls') phis
        instr     = Value (Phi ls') (Just (Assignment d t))
    merge (phis, instrs) instr = (phis, instrs ++ [instr])

-- | convert the given function into SSA from
ssa :: Function -> Function
ssa fn = finstrs .~ ilist $ fn
  where
    -- if the function has args add a new block
    ablock       = arg <$> fn ^. fargs
    arg x        = case x of Argument d t -> Value (Id d) (Just (Assignment d t))
    -- create the CFG and frontier
    cfg          = mkCFG $ finstrs %~ (ablock ++) $ fn
    front        = dominationFrontier cfg
    -- get the phi nodes to be added to each block
    phis         = addPhi succs instrs front
    succs        = cfg ^. successors . edges
    instrs       = cfg ^. instructions
    tree         = dominationTree cfg
    -- create empty stacks for all variables (including func args)
    vars         = M.unions $ variables <$> M.elems instrs
    stack       = M.mapWithKey (\x _ -> empty x) vars
    -- perform renaming of the blocks recursively
    (i, p, _)    = rename succs instrs phis stack tree
    -- create the list of instructions
    ilist        = (combinePhi vars i p M.!) =<< cfg ^. blocks

-- | remove the phi nodes by adding relevant copy instructions
--   in place of the phi nodes
removePhi :: M.HashMap Ident [Instruction] -> M.HashMap Ident [Instruction]
removePhi instrs = foldl' modify instrs' ps
  where
    -- filter all the phi instructions from this
    notPhi (Value (Phi _) _) = False
    notPhi _                 = True
    instrs'                  = filter notPhi <$> instrs
    -- extract all the phi instruction from this
    ps                       = unions $ phis <$> M.elems instrs
    -- insert the copy instruction in the given block
    modify m (d, t, v, l)    = M.adjust (insert (copy d t v)) l m
    copy d t v               = Value (Id v) (Just (Assignment d t))
    -- insert an instruction before a terminating instr
    insert i ls              = case listToMaybe $ reverse ls of
                                 Just li | terminator li -> init ls ++ [i, li]
                                 _                       -> ls ++ [i]

-- | convert the given function out of SSA form
ssa' :: Function -> Function
ssa' fn = finstrs .~ is $ fn
  where
    cfg = mkCFG fn
    im  = removePhi $ cfg ^. instructions
    is  = (im M.!) =<< (cfg ^. blocks)
