module Util.Misc where

import           Bril.Lang.AST
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

-- | a multimap can contain multiple values
--   for the same key
type MultiMap a b = M.HashMap a (S.HashSet b)

-- | remove an element from a set
unconsSet :: (Eq a, Hashable a) => S.HashSet a -> Maybe (a, S.HashSet a)
unconsSet s
  | S.null s  = Nothing
  | otherwise = let h = head $ S.toList s in Just (h, S.delete h s)
{-# SPECIALIZE unconsSet :: S.HashSet Ident -> Maybe (Ident, S.HashSet Ident) #-}
{-# INLINABLE unconsSet #-}

-- | find the intersection of all the sets in the given container
intersections :: (Foldable t, Eq a, Hashable a) => t (S.HashSet a) -> S.HashSet a
intersections x = if null x then S.empty else foldl1 S.intersection x
{-# SPECIALIZE intersections :: S.HashSet (S.HashSet Ident) -> S.HashSet Ident #-}
{-# SPECIALIZE intersections :: [S.HashSet Ident] -> S.HashSet Ident #-}
{-# INLINABLE intersections #-}

-- | find the union of all the sets in the given container
unions :: (Foldable t, Eq a, Hashable a) => t (S.HashSet a) -> S.HashSet a
unions = foldl' S.union S.empty
{-# SPECIALIZE unions :: S.HashSet (S.HashSet Ident) -> S.HashSet Ident #-}
{-# SPECIALIZE unions :: [S.HashSet Ident] -> S.HashSet Ident #-}
{-# INLINABLE unions #-}

-- | takes in a multimap from a to b and then returns a multimap from b to a
transpose :: (Eq a, Hashable a, Eq b, Hashable b) => MultiMap a b -> MultiMap b a
transpose map = M.mapWithKey (const . keys) $ S.toMap values
  where
    keys v = M.keysSet $ M.filter (S.member v) map
    values = unions $ M.elems map
{-# SPECIALIZE transpose :: MultiMap Ident Ident -> MultiMap Ident Ident #-}
{-# INLINEABLE transpose #-}

-- | removes repeating elements a list
distinct :: Eq a => [a] -> [a]
distinct []  = []
distinct [x] = [x]
distinct (x : y : xs)
  | x == y    = distinct (x : xs)
  | otherwise = x : distinct (y : xs)
{-# SPECIALIZE distinct :: [Ident] -> [Ident] #-}
{-# INLINABLE distinct #-}

-- | zip with the list with it's index
indexed :: [a] -> [(Int, a)]
indexed = zip [0..]
{-# INLINABLE indexed #-}
