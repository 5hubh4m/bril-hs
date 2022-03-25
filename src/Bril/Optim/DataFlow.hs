{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bril.Optim.DataFlow where

import           Bril.Lang.AST
import           Bril.Structure.CFG
import           Control.Lens
import           Data.Foldable
import           Data.Hashable
import           GHC.Generics        hiding (to)
import           Util.Graph
import           Util.Misc
import qualified Data.HashSet        as S
import qualified Data.HashMap.Strict as M

-- | specification of a data flow analysis
class DataFlowAnalysis a where
  -- | starting value of the analysis
  start :: a

  -- | merge function for the analysis
  merge :: Foldable t => t a -> a

  -- | transfer function for the analysis
  transfer :: [Instruction] -> a -> a

  -- | begin the analysis and return the result
  invoke :: CFG -> (M.HashMap Ident a, M.HashMap Ident a)

-- | define a type for live variable analysis
newtype LiveVariables = LiveVariables (S.HashSet Ident)
                      deriving (Show, Eq, Generic, Hashable)

-- | define functions for live variable analysis
instance DataFlowAnalysis LiveVariables where
  -- | init value is just an empty set
  start = LiveVariables S.empty
  {-# INLINABLE start #-}

  -- | merge the live variables
  merge = LiveVariables . foldl' (\s (LiveVariables xs) -> S.union s xs) S.empty
  {-# INLINABLE merge #-}

  -- | take a basic block and find the live variables in it  
  transfer is (LiveVariables a) = LiveVariables $ foldr' move a is
    where
      move instr live = maybe live' dest $ assignment instr
        where
          dest (Assignment d _) = S.delete d live'
          live'                 = S.union live . S.fromList $ args instr
  {-# INLINABLE transfer #-}

  -- | perform a live variable analysis on a CFG
  invoke cfg    = dataFlowAnalysis cfg False blocks init init
    where
      init   = start <$ S.toMap blocks
      blocks = cfg ^. successors . vertices
  {-# INLINABLE invoke #-}

-- | perform a data flow analysis on a CFG
dataFlowAnalysis :: (DataFlowAnalysis a, Eq a, Hashable a)
                 => CFG
                 -> Bool
                 -> S.HashSet Ident
                 -> M.HashMap Ident a
                 -> M.HashMap Ident a
                 -> (M.HashMap Ident a, M.HashMap Ident a)
dataFlowAnalysis cfg forward works input output = case unconsSet works of
  Nothing | forward       -> (input, output)
          | otherwise     -> (output, input)
  Just (block, remaining) -> if op == (output M.! block)
                             then dataFlowAnalysis cfg forward remaining input output
                             else dataFlowAnalysis cfg forward works' input' output'
    where
      succs   = cfg ^. successors . edges
      preds   = cfg ^. predecessors . edges
      instrs  = cfg ^. instructions
      -- get the successors and predecessors of the current block
      next    = if forward then succs M.! block else preds M.! block
      prev    = if forward then preds M.! block else succs M.! block
      -- | find the input and output for this block
      ip      = merge $ S.map (input M.!) prev
      op      = transfer (instrs M.! block) ip
      -- | update the list and result maps
      works'  = S.union next works
      input'  = M.insert block ip input
      output' = M.insert block op output
{-# SPECIALIZE dataFlowAnalysis :: CFG -> Bool -> S.HashSet Ident -> M.HashMap Ident LiveVariables -> M.HashMap Ident LiveVariables -> (M.HashMap Ident LiveVariables, M.HashMap Ident LiveVariables) #-}
{-# INLINABLE dataFlowAnalysis #-}
