module Bril.Optim.Loop (licm) where

import           Bril.Lang.AST
import           Bril.Optim.DataFlow
import           Bril.Structure.CFG
import           Bril.Structure.Loop
import           Bril.Structure.SSA
import           Control.Lens
import           Data.Foldable
import           Data.Ord
import           Data.List
import           Util.Misc
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import Debug.Trace

printV s v = trace (s ++ show v) v

-- | whether this instruction should be hoisted
shouldHoist :: Instruction -> Bool
shouldHoist (Value (BinOp Div _ _) (Just _))  = False
shouldHoist (Value (BinOp FDiv _ _) (Just _)) = False
shouldHoist (Value Call {} _)                 = False
shouldHoist (Value Alloc {} _)                = False
shouldHoist (Value Phi {} _)                  = False
shouldHoist (Value _ (Just _))                = True
shouldHoist _                                 = False
{-# INLINABLE shouldHoist #-}

-- | whether an instruction is a loop invariant given the loop body
--   the block in which each variable is defined, and a set of
--   currently known invariants
isInvariant :: S.HashSet Ident -> M.HashMap Ident Ident -> S.HashSet Ident -> Instruction -> Bool
isInvariant body defs known instr = trivially instr || shouldHoist instr && all invariant (args instr)
  where
    -- instruction is trivially invariant because it's defined outside the body
    trivially   = maybe False (\(Assignment d _) -> invariant d) . assignment
    -- an identifier is invariant because it's defined outside or it's in the invariant set
    invariant a = S.member a known || not (S.member (defs M.! a) body)
{-# INLINABLE isInvariant #-}

-- | find loop invariant instructions
loopInvariants :: CFG -> Loop -> M.HashMap Ident Ident -> [Instruction]
loopInvariants cfg loop defs = rec [] loopInstrs
  where
    -- get all the instructions in the loop
    blocks       = loop ^. body
    instrs b     = (cfg ^. instructions) M.! b
    loopInstrs   = foldl' (\s b -> s ++ instrs b) [] blocks
    rec invs rem = if null invs' then invs else rec (invs ++ invs') rem'
      where
       -- get all the destination in invariant instructions
       dest s        = maybe s (\(Assignment d _) -> S.insert d s) . assignment
       invars        = foldl' dest S.empty invs
       -- get all the invariant instructions and remaining
       (invs', rem') = partition (isInvariant blocks defs invars) rem
{-# INLINABLE loopInvariants #-}

-- | move the given invariant instructions for the loop
moveInvariants :: CFG -> Loop -> [Instruction] -> CFG
moveInvariants cfg loop invars = instructions .~ instrs'' $ cfg
  where
    instrs   = cfg ^. instructions
    blocks   = loop ^. body
    preh     = loop ^. preheader
    -- | filtering non-invariant instructions
    notinv i = not $ S.member i $ S.fromList invars
    instrs'  = foldl' (flip (M.adjust (filter notinv))) instrs blocks
    -- add the invariant instructions
    instrs'' = M.adjust (`insertAfter` invars) preh instrs'
{-# INLINABLE moveInvariants #-}

-- | this instruction can be hoisted to the preheader if
--   either it dominates all loop exits or it's dead after
--   the loop
canHoist :: CFG -> Loop -> M.HashMap Ident Ident -> M.HashMap Ident LiveVariables -> Instruction -> Bool
canHoist cfg loop defs live = maybe False hoist . assignment
  where
    blocks                 = loop ^. body
    -- exit blocks for this loop
    after                  = foldl' S.union S.empty $ loop ^. exits
    -- return the set of live variables for this block
    living b               = case live M.! b of LiveVariables x -> x
    -- we can hoist if the variable is dead after the loop
    hoist (Assignment d _) = not $ any (S.member d . living) after
{-# INLINABLE canHoist #-}

-- | loop invariant code motion for a function in SSA form
licm :: Function -> Function
licm fn = finstrs .~ instrs $ fn
  where
    cfg       = mkCFG fn
    -- sort loops by size in decreasing order
    -- i.e bigger/outer loops first
    loops     = sortOn (Down . length . (^. body)) $ mkLoops cfg
    -- get the result of live variable analysis
    (b, _)    = invoke cfg :: (M.HashMap Ident LiveVariables, M.HashMap Ident LiveVariables)
    -- apply licm to all loops
    cfg'      = foldl' licm' cfg loops
    instrs    = ((cfg' ^. instructions) M.!) =<< (cfg' ^. blocks)
    licm' c l = moveInvariants c l invs
      where
        invs = loopInvariants c l defs
        defs = definitions c
{-# INLINABLE licm #-}
