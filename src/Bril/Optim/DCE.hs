module Bril.Optim.DCE (tdce) where

import           Bril.Lang.AST
import           Control.Lens
import qualified Data.HashSet  as S

-- | whether an instruction is globally dead
dead :: S.HashSet Ident -> Instruction -> Bool
dead _    Label {}                          = False
dead _    (Effect Nop)                      = True
dead _    (Effect _)                        = False
dead _    (Value Call {} _)                 = False
dead _    (Value Alloc {} _)                = False
dead _    (Value _ Nothing)                 = True
dead used (Value _ (Just (Assignment d _))) = not $ S.member d used
{-# INLINABLE dead #-}

-- | trivial dead code elimination on an entire function
tdce :: Function -> Function
tdce fn = if fn == fn' then fn else tdce fn'
  where
    fn'  = finstrs %~ filter (not . dead used) $ fn
    used = S.fromList $ args =<< fn ^. finstrs
{-# INLINABLE tdce #-}
