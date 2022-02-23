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

-- | define an arbitrary instance for identifiers
--   which are characters from 'a' to 'z' or 'A' to 'Z'
instance Arbitrary Ident where
  arbitrary = do ident <- arbitraryPrintableChar
                 return . Ident . T.pack $ [ident]

-- | removes repeating elements a list
distinct :: Eq a => [a] -> [a]
distinct []  = []
distinct [x] = [x]
distinct (x : y : xs)
  | x == y    = distinct (x : xs)
  | otherwise = x : distinct (y : xs)
{-# SPECIALIZE distinct :: [Ident] -> [Ident] #-}
{-# INLINABLE distinct #-}

-- | define an arbitrary instance for CFGs
--   with empty instructions and randomly
--   generated edges
instance Arbitrary CFG where
  arbitrary = do blocks <- suchThat (distinct <$> orderedList) (not . null)
                 graph  <- M.fromList <$> mapM (edges blocks) blocks
                 return $ CFG (head blocks) (S.fromList blocks) M.empty graph
    where
      -- takes the list of blocks and shuffles it and
      -- chooses at most 2 blocks as successors
      edges bs b   = do num   <- frequency [(1, return 0), (6, return 1), (3, return 2)]
                        succs <- shuffle bs
                        return (b, S.fromList $ take num succs)

-- | navigate all acyclic paths from b to a in the the graph
--   and apply the given operations to obtain a final result
search :: (Eq a, Hashable a, Eq c, Hashable c) => (a -> b) -> (a -> b -> c) -> (S.HashSet c -> b) -> M.HashMap a (S.HashSet a) -> a -> a -> b
search final transform collect graph = fn S.empty
  where
    fn seen a b
      | a == b    = final a
      | otherwise = collect $ S.map (transform b . fn seen' a) next
      where
        seen' = S.insert b seen
        next  = S.difference (graph M.! b) seen'
{-# INLINABLE search #-}

-- | whether the node a is reachable from the node b in the given graph
reachable :: (Eq a, Hashable a) => M.HashMap a (S.HashSet a) -> a -> a -> Bool
reachable = search (const True) (const id) or
{-# SPECIALIZE reachable :: M.HashMap Ident (S.HashSet Ident) -> Ident -> Ident -> Bool #-}
{-# INLINABLE reachable #-}

-- | get all acyclic paths from b to a in the given graph
paths :: (Eq a, Hashable a) => M.HashMap a (S.HashSet a) -> a -> a -> S.HashSet [a]
paths = search (\x -> S.singleton [x]) (\x -> S.map ([x] ++)) unions
{-# SPECIALIZE paths :: M.HashMap Ident (S.HashSet Ident) -> Ident -> Ident -> S.HashSet [Ident] #-}
{-# INLINABLE paths #-}

-- | defines the property that dominators of a block b
--   are all the the blocks that must occur in all
--   paths from entry to that block
prop_dominatorsDefn :: CFG -> Bool
prop_dominatorsDefn cfg = and . M.mapWithKey (\v ds -> ds == doms v) $ dominators cfg
  where
    start   = entry cfg
    succs   = successors cfg
    -- this find the dominators of a given node by
    -- taking the intersection of all the common nodes
    -- in all acyclic paths from the entry to that node
    -- with the additional criteria that each node
    -- dominates itself by default
    doms v  = intersections . S.map S.fromList $ paths succs v start
{-# INLINABLE prop_dominatorsDefn #-}

-- | defines the property that the dominatees of a
--   block b are it's recursive children in a
--   dominator tree
prop_dominationTreeDefn :: CFG -> Bool
prop_dominationTreeDefn cfg = snd . prop $ dominationTree cfg
  where
    doms             = invert $ dominators cfg
    -- this finds the recursive children of the given node
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
    doms       = invert $ dominators cfg
    front      = dominationFrontier cfg
    preds      = prececessors cfg
    strict v   = S.delete v $ doms M.! v
    -- this property says that domination frontier
    -- of v is not strictly dominated by v
    prop  v fs = null . S.intersection fs $ strict v
    -- this property says that domination frontier
    -- of v is one edge away from nodes v dominates
    prop' v    = not . any (null . S.intersection (doms M.! v) . (preds M.!))
    -- conbine both the properties together
    props v fs = prop v fs && prop v fs
{-# INLINABLE prop_dominationFrontierDefn #-}

-- let quick check define our tests for us
return []
checkStructure = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 10000 })
