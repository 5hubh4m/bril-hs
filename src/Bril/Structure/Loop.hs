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
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

-- | this type encapsulates a natural loop
data Loop = Loop
          { _preheader :: Ident
          , _header    :: Ident
          , _tail      :: Ident
          , _body      :: S.HashSet Ident
          }
          deriving (Show, Eq, Generic, Hashable)

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

-- | identify the body of the natural loop corresponsing 
--   to the given backedge
loop :: CFG -> (Ident, Ident) -> Maybe Loop
loop cfg (header, tail) = if length pre == 1 && all verify body
                          then Just lp else Nothing
  where
    preds    = cfg ^. predecessors . edges
    body     = rec (S.singleton tail) $ S.singleton header
    lp       = Loop (head pre) header tail body
    -- whether the body indeed forms a natural loop
    verify v = (v == header) || S.isSubsetOf (preds M.! v) body
    -- whether the body has a single preheader
    pre      = S.toList $ preds M.! header
    -- recursively update body
    rec s b
      | null s       = b
      | S.member h b = rec t b
      | otherwise    = rec s' b'
      where
        h  = head $ S.toList s
        t  = S.delete h s
        s' = S.union t $ preds M.! h
        b' = S.insert h b

-- | identify all loops in a function
mkLoop :: Function -> S.HashSet Loop
mkLoop fn = foldl' ac S.empty be
  where
    cfg    = mkCFG fn
    be     = backedges cfg
    ac s x = case loop cfg x of
               Just l  -> S.insert l s
               Nothing -> s
