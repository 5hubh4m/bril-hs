{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bril.Structure.SSA (ssa, ssa') where

import           Bril.Lang.AST
import           Bril.Structure.CFG
import           Data.Bifunctor
import           Data.Foldable
import           Data.Hashable
import           Data.Tree
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

-- | all the phi instructions in
--   the given list of instructions
phis :: [Instruction] -> S.HashSet (Ident, Type, Ident, Ident)
phis = S.fromList . (fn =<<)
  where
    fn (Value (Phi ls) (Just (Assignment d t))) = (\(x, y) -> (d, t, x, y)) <$> ls
    fn _                                        = []

-- | represents a phi node for a variable
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
              { variable :: Ident
              , stack    :: [Ident]
              , counter  :: Int
              }
              deriving (Show, Eq)

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
curr (VarStack i []      _) = i
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
    fn (s, ps) p = second (`S.insert` ps) $ renamePhi s p
    (stack', ps) = foldl' fn (stack, S.empty) $ phis M.! b
    -- update the stack with the instrs in this block
    gn (s, is) i = second ((is ++) . (: [])) $ renameInstr s i
    (stack'', is) = foldl' gn (stack', []) $ instrs M.! b
    -- update the instrs and phi nodes for the current block
    instrs' = M.insert b is instrs
    phis' = M.insert b ps phis
    -- update the phi nodes in this block's successors
    hn (PhiNode v u xs)
      | declared $ stack'' M.! v = PhiNode v u ((curr $ stack'' M.! v, b) : xs)
      | otherwise                = PhiNode v u xs
    phis'' = foldl' (flip (M.adjust $ S.map hn)) phis' $ succs M.! b
    -- recurse on the children nodes
    rec (i, p, s) n = rename succs i p s n
    -- return the original stack with updated counter
    pop s = (stack M.! variable s) { counter = counter s }
    fin (i, p, s) = (i, p, pop <$> s)

-- | combine the phi nodes with the instruction blocks
combinePhi :: M.HashMap Ident Type -> M.HashMap Ident [Instruction] -> MultiMap Ident PhiNode -> M.HashMap Ident [Instruction]
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
mergePhi :: MultiMap Ident PhiNode -> MultiMap Ident PhiNode
mergePhi phis = merge <$> phis
  where
    merge ps     = foldl' (\ps p -> S.map (adjust p) ps) ps $ singles ps
    singles ps   = M.elems . M.mapMaybeWithKey extract $ S.toMap ps
    -- extract the phi nodes with a single label
    extract (PhiNode _ v' [(i, l)]) _  = Just (v', i, l)
    extract _                       _  = Nothing
    -- for the given phi node, replace by the above
    adjust (v', i, l) (PhiNode u v ls) = PhiNode u v $ fn <$> ls
      where
        fn x = if x == (v', l) then (i, l) else x

-- | convert the given function into SSA from
ssa :: Function -> Function
ssa fn = Function n a t ilist
  where
    -- if the function has args add a new block
    fn'          = Function n a t $ ablock ++ x
    ls           = allLabels fn
    ablock       = [Label $ freshLabel ls | not $ null a] ++ (arg <$> a)
    arg a        = let (d, t) = case a of Argument d t -> (d, t)
                   in Value (Id d) (Just (Assignment d t))
    -- create the CFG and frontier
    gh           = cfg fn'
    front        = dominationFrontier gh
    -- get the phi nodes to be added to each block
    phis         = addPhi succs instrs front
    succs        = edges $ successors gh
    instrs       = instructions gh
    tree         = dominationTree gh
    ilist        = (combinePhi vars i (mergePhi p) M.!) =<< blocks gh
    -- create empty stacks for all variables (including func args)
    args         = M.fromList $ (\(Argument d t) -> (d, t)) <$> a
    vars         = M.unions $ variables <$> M.elems instrs
    vstack       = M.mapWithKey (\x _ -> empty x) vars
    astack       = M.mapWithKey (\x _ -> singleton x) args
    stack        = M.union astack vstack
    -- perform renaming of the blocks recursively
    (i, p, _)    = rename succs instrs phis stack tree
    (n, a, t, x) = case fn of Function n a t i -> (n, a, t, i)

-- | remove the phi nodes by adding relevant copy instructions
--   in place of the phi nodes
removePhi :: M.HashMap Ident [Instruction] -> M.HashMap Ident [Instruction]
removePhi instrs = foldl' fn instrs' ps
  where
    -- filter all the phi instructions from this
    notPhi (Value (Phi _) _) = False
    notPhi _                 = True
    instrs'                  = filter notPhi <$> instrs
    -- extract all the phi instruction from this
    ps                       = unions $ phis <$> M.elems instrs
    -- inster the copy instruction in the given block
    fn m (d, t, v, l)        = M.adjust (adj d t v) l m
    adj d t v is             = insert (copy d t v) is
    copy d t v               = Value (Id v) (Just (Assignment d t))
    -- insert an instruction before a terminating instr
    insert i []              = [i]
    insert i ls              = let li = last ls in if terminator li
                               then init ls ++ [i, li] else ls ++ [i]

-- | convert the given function out of SSA form
ssa' :: Function -> Function
ssa' fn = Function n a t ilist
  where
    gh      = cfg fn
    instrs  = instructions gh
    -- remove the phi nodes
    instrs' = removePhi instrs
    ilist   = (instrs' M.!) =<< blocks gh
    (n, a, t) = case fn of Function n a t _ -> (n, a, t)
