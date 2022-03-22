{-# LANGUAGE TemplateHaskell #-}

module Bril.Structure.Spec where

import           Bril.Lang.AST
import           Bril.Structure.CFG
import           Control.Lens
import           Data.Foldable
import           Data.Hashable
import           Data.Tree
import           Test.QuickCheck
import           Util.Graph
import           Util.Misc
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.Text           as T

-- | define an arbitrary instance for identifiers
--   which are characters from 'a' to 'z' or 'A' to 'Z'
instance Arbitrary Ident where
  arbitrary = do ident <- arbitraryPrintableChar
                 return . Ident . T.pack $ [ident]

-- | define an arbitrary instance for CFGs
--   with empty instructions and randomly
--   generated edges
instance Arbitrary CFG where
  arbitrary = do blocks <- suchThat (distinct <$> orderedList) (not . null)
                 graph  <- M.fromList <$> mapM (edges blocks) blocks
                 return $ CFG (head blocks) blocks (Graph (S.fromList blocks) graph) M.empty
    where
      -- takes the list of blocks and shuffles it and
      -- chooses at most 2 blocks as successors
      edges bs b   = do num   <- frequency [(1, return 0), (6, return 1), (3, return 2)]
                        succs <- shuffle bs
                        return (b, S.fromList $ take num succs)

-- | defines the property that dominators of a block b
--   are all the the blocks that must occur in all
--   paths from entry to that block
prop_dominatorsDefn :: CFG -> Bool
prop_dominatorsDefn cfg = and . M.mapWithKey (\v ds -> ds == doms v) $ dominators cfg
  where
    start   = cfg ^. entry
    succs   = cfg ^. successors
    -- this find the dominators of a given block by
    -- taking the intersection of all the common blockss
    -- in all acyclic paths from the entry to that block
    doms v  = intersections . S.map S.fromList $ paths succs v start
{-# INLINABLE prop_dominatorsDefn #-}

-- | defines the property that the dominatees of a
--   block b are it's recursive children in a
--   dominator tree
prop_dominationTreeDefn :: CFG -> Bool
prop_dominationTreeDefn cfg = snd . prop $ dominationTree cfg
  where
    doms             = transpose $ dominators cfg
    -- this finds the recursive children of the given block
    -- of the tree along with keeping track of whether
    -- the tree property is true
    prop (Node r ts) = (ds, doms M.! r == ds && and bs)
      where
        (ss, bs) = unzip $ prop <$> ts
        ds       = S.insert r $ unions ss
{-# INLINABLE prop_dominationTreeDefn #-}

-- | defines the property that the domination frontier
--   of a block are the blocks that are one edge away
--   from being strictly dominated by that block
prop_dominationFrontierDefn :: CFG -> Bool
prop_dominationFrontierDefn cfg = and $ M.mapWithKey props front
  where
    doms       = transpose $ dominators cfg
    front      = dominationFrontier cfg
    preds      = cfg ^. predecessors . edges
    strict v   = S.delete v $ doms M.! v
    -- this property says that domination frontier
    -- of v is not strictly dominated by v
    prop  v fs = null . S.intersection fs $ strict v
    -- this property says that domination frontier
    -- of v is one edge away from blocks v dominates
    prop' v    = not . any (null . S.intersection (doms M.! v) . (preds M.!))
    -- conbine both the properties together
    props v fs = prop v fs && prop v fs
{-# INLINABLE prop_dominationFrontierDefn #-}

-- let quick check define our tests for us
return []
checkStructure = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 10000 })
