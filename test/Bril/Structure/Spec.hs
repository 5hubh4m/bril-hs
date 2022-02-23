{-# LANGUAGE TemplateHaskell #-}

module Bril.Structure.Spec where

import           Bril.Lang.AST
import           Bril.Structure.CFG
import           Data.Bifunctor
import           Data.Foldable
import           Data.Hashable
import           Data.Tree
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.Text           as T

-- | define the maximum size of a randomly generated CFG
maxCFGBlocks :: Int
maxCFGBlocks = 20

-- | define an arbitrary instance for identifiers
--   which are characters from 'a' to 'z' or 'A' to 'Z'
instance Arbitrary Ident where
  arbitrary = do ident <- oneof [chooseEnum ('A', 'Z'), chooseEnum ('a', 'z')]
                 return . Ident . T.pack $ [ident]

-- | define an arbitrary instance for CFGs
--   with empty instructions and randomly
--   generated edges
instance Arbitrary CFG where
  arbitrary = do blocks <- resize maxCFGBlocks $ listOf1 arbitrary
                 let en = head blocks
                 let gs = M.fromList <$> mapM (edges blocks) blocks
                 graph <- suchThat gs $ reaches en
                 return $ CFG en (S.fromList blocks) M.empty graph
    where
      -- takes an entry node and a graph and returns whether
      -- all vertices in the graph are reachable from the entry
      reaches en g = all (\x -> reachable g x en) $ M.keys g
      -- takes the list of blocks and shuffles it and
      -- chooses at most 2 blocks as successors 
      edges bs b   = do n     <- chooseInt (0, 2)
                        succs <- shuffle bs
                        return (b, S.fromList $ take n succs)

-- | navigate all acyclic paths from b to a in the the graph
--   and apply the given operations to obtain a final result
search :: (Eq a, Hashable a, Eq c, Hashable c) => (a -> b) -> (a -> b -> c) -> (S.HashSet c -> b) -> M.HashMap a (S.HashSet a) -> a -> a -> b
search final transform collect graph a b = fn S.empty a b
  where
    fn seen a b
      | a == b    = final a
      | otherwise = collect $ S.map (transform b . fn seen' a) next
      where
        seen' = S.insert b seen
        next  = S.difference (graph M.! b) seen'

-- | whether the node a is reachable from the node b in the given graph
reachable :: (Eq a, Hashable a) => M.HashMap a (S.HashSet a) -> a -> a -> Bool
reachable = search (const True) (const id) or

-- | get all acyclic paths from b to a in the given graph
paths :: (Eq a, Hashable a) => M.HashMap a (S.HashSet a) -> a -> a -> S.HashSet [a]
paths = search (\x -> S.singleton [x]) (\x -> S.map ([x] ++)) unions

-- | defines the property that dominators of a block b
--   are all the the blocks that must occur in all
--   paths from entry to that block
prop_dominatorsDefn :: CFG -> Bool
prop_dominatorsDefn cfg = and $ M.mapWithKey (\v ds -> ds == doms v) $ dominators cfg
  where
    start = entry cfg
    succs   = successors cfg
    -- this find the dominators of a given node by
    -- taking the intersection of all the common nodes
    -- in all acyclic paths from the entry to that node
    doms v  = intersections . S.map S.fromList $ paths succs v start

-- | defines the property that the dominators of a
--   block b are it's recursive children in a
--   dominator tree
prop_dominationTreeDefn :: CFG -> Bool
prop_dominationTreeDefn cfg = snd $ prop tree
  where
    doms             = invert $ dominators cfg
    tree             = dominationTree cfg
    -- this finds the recursive children of the given node
    -- of the tree along with keeping track of whether
    -- the tree property is true
    prop (Node r ts) = (ds, doms M.! r == ds && and bs)
      where
        (ss, bs) = unzip $ prop <$> ts
        ds       = S.insert r $ unions ss

-- | defines the property that the domination frontier
--   of a block are the blocks that are one edge away
--   from being dominated by that block
prop_dominationFrontierDefn :: CFG -> Bool
prop_dominationFrontierDefn cfg = and $ M.mapWithKey props front
  where
    doms       = invert $ dominators cfg
    front      = dominationFrontier cfg
    preds      = prececessors cfg
    strict v   = S.delete v $ doms M.! v
    -- this property says that domination frontier
    -- of v is not strictly dominated by v
    prop  v fs = null . S.intersection fs $ strict v
    -- this property says that domination frontier
    -- of v is one edge away from nodes v dominates
    prop' v = not . any (null . S.intersection (doms M.! v) . (preds M.!))
    -- conbine both the properties together
    props v fs = prop v fs && prop v fs

-- let quick check define all the tests for us
return []
check = $quickCheckAll
