{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Util.Graph where

import           Bril.Lang.AST
import           Control.Lens
import           Data.Foldable
import           Data.Hashable
import           GHC.Generics
import           Util.Misc
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

-- | define a graph data type as a multimap
--   with the invariant that only the given
--   vertices are ever in the edges map and
--   and all vertices have a key in the edges
--   map
data Graph a = Graph
             { _vertices :: S.HashSet a
             , _edges    :: MultiMap a a
             }
             deriving (Show, Eq, Generic, Hashable)

-- | make lenses for graph
makeLenses ''Graph

-- create a graph from the given matrix
mkGraph :: (Eq a, Hashable a) => MultiMap a a -> Graph a
mkGraph m = Graph vs $ M.union m $ S.empty <$ S.toMap vs
  where
    vs = S.union (M.keysSet m) $ unions $ M.elems m

-- | create a list of graph vertices in post-order traversal
--   starting from the given vertex
postorder :: (Eq a, Hashable a) => Graph a -> a -> [a]
postorder graph root = fn S.empty root ^. _2
  where
    fn seen root = if S.member root seen then (seen, [])
                   else _2 %~ (++ [root]) $ foldl' upd (seen', []) next
      where
        children     = (graph ^. edges) M.! root
        next         = S.toList $ S.difference children seen
        seen'        = S.insert root seen
        upd (s, l) n = (_2 %~ (l ++)) $ fn s n
{-# SPECIALIZE postorder :: Graph Ident -> Ident -> [Ident] #-}
{-# INLINEABLE postorder #-}

-- | takes in a directed graph and reverses the edges
invert :: (Eq a, Hashable a) => Graph a -> Graph a
invert = edges %~ (\es -> M.union (transpose es) $ S.empty <$ es)
{-# SPECIALIZE invert :: Graph Ident -> Graph Ident #-}
{-# INLINEABLE invert #-}

-- | navigate all acyclic paths from b to a in the the graph
--   and apply the given operations to obtain a final result
search :: (Eq a, Hashable a, Eq c, Hashable c)
       => (a -> b) -> (a -> b -> c) -> (S.HashSet c -> b)
       -> (Graph a -> a -> a -> b)
search final transform collect graph = fn S.empty
  where
    fn seen a b
      | a == b    = final a
      | otherwise = collect $ S.map (transform b . fn seen' a) next
      where
        seen' = S.insert b seen
        next  = S.difference ((graph ^. edges) M.! b) seen'
{-# INLINABLE search #-}

-- | whether the node a is reachable from the node b in the given graph
reachable :: (Eq a, Hashable a) => Graph a -> a -> a -> Bool
reachable = search (const True) (const id) or
{-# SPECIALIZE reachable :: Graph Ident -> Ident -> Ident -> Bool #-}
{-# INLINABLE reachable #-}

-- | get all acyclic paths from b to a in the given graph
paths :: (Eq a, Hashable a) => Graph a -> a -> a -> S.HashSet [a]
paths = search (\x -> S.singleton [x]) (\x -> S.map ([x] ++)) unions
{-# SPECIALIZE paths :: Graph Ident -> Ident -> Ident -> S.HashSet [Ident] #-}
{-# INLINABLE paths #-}
