{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Bril.Structure.Loop where

import           Bril.Lang.AST
import           Bril.Structure.CFG
import           Control.Lens
import           Data.Hashable
import           Data.Maybe
import           Data.List
import           GHC.Generics
import           Util.Graph
import           Util.Misc
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

-- | this type encapsulates a natural loop
data Loop = Loop
          { _preheader :: Ident                -- preheader for the loop
          , _header    :: Ident                -- header for the loop
          , _tail      :: Ident                -- tail for the loop
          , _body      :: S.HashSet Ident      -- blocks in the loop body
          , _exits     :: MultiMap Ident Ident -- blocks which have exits from the loop
          }
          deriving (Show, Eq, Generic, Hashable)

-- | make lenses for loop
makeLenses ''Loop

-- | find all the backedges in a CFG
backedges :: CFG -> S.HashSet (Ident, Ident)
backedges cfg = M.foldlWithKey' ac S.empty $ M.mapWithKey be succs
  where
    preds     = cfg ^. predecessors . edges
    succs     = cfg ^. successors . edges
    doms      = dominators cfg
    -- backedges are edges to your dominator
    be v ss   = S.intersection ss $ doms M.! v
    ac s v bs = S.union s $ S.map (,v) bs
{-# INLINABLE backedges #-}

-- | identify the body of the natural loop corresponsing 
--   to the given backedge
loop :: CFG -> (Ident, Ident) -> Maybe Loop
loop cfg (header, tail) = if length pre == 1 && all verify body then
                          Just loop else Nothing
  where
    succs    = cfg ^. successors . edges
    preds    = cfg ^. predecessors . edges
    body     = rec (S.singleton tail) $ S.singleton header
    loop     = Loop (head pre) header tail body exits
    -- find loop exits
    exit     = (`S.difference` body)
    exits    = exit <$> M.intersection succs (S.toMap body)
    isExit b = not . null $ S.difference (succs M.! b) body
    -- whether the body indeed forms a natural loop
    verify v = (v == header) || S.isSubsetOf (preds M.! v) body
    -- whether the body has a single preheader
    pre      = S.toList $ S.difference (preds M.! header) body
    -- recursively update body
    rec s b = case unconsSet s of
                Nothing                    -> b
                Just (h, t) | S.member h b -> rec t b
                            | otherwise    -> rec (S.union t $ preds M.! h) $ S.insert h b
{-# INLINABLE loop #-}

-- | identify all loops in a function
mkLoops :: CFG -> [Loop]
mkLoops cfg = foldl' fn [] (backedges cfg)
  where
    -- fold all the loops into a set
    fn s e = maybe s (: s) $ loop cfg e
{-# INLINABLE mkLoops #-}
