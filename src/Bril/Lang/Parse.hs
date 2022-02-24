{-# LANGUAGE OverloadedStrings #-}

module Bril.Lang.Parse where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Scientific
import qualified Bril.Lang.AST       as Bril
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

-- | convert an identifier from a JSON
instance FromJSON Bril.Ident where
  parseJSON = withText "Identifier" $ return . Bril.Ident

-- | convert an identifier to a JSON
instance ToJSON Bril.Ident where
  toJSON (Bril.Ident i) = String i

-- | convert a literal from a JSON
instance FromJSON Bril.Literal where
  parseJSON (Bool b)   = return $ Bril.Bool b
  parseJSON (Number n) = return $ case floatingOrInteger n of
                                    Left f -> Bril.Float f
                                    Right i -> Bril.Int i
  parseJSON _ = fail "parsing value failed."

-- | convert a literal to a JSON
instance ToJSON Bril.Literal where
  toJSON (Bril.Bool b)  = Bool b
  toJSON (Bril.Int i)   = Number $ fromInteger i
  toJSON (Bril.Float f) = Number $ fromFloatDigits f

-- | convert a type from a JSON
instance FromJSON Bril.Type where
  parseJSON (Object v)       = Bril.Pointer <$> v .: "ptr"
  parseJSON (String "int")   = return Bril.Integer
  parseJSON (String "bool")  = return Bril.Boolean
  parseJSON (String "float") = return Bril.Floating
  parseJSON _                = fail "parsing type failed."

-- | convert a type to a JSON
instance ToJSON Bril.Type where
  toJSON Bril.Integer     = String "int"
  toJSON Bril.Boolean     = String "bool"
  toJSON Bril.Floating    = String "float"
  toJSON (Bril.Pointer t) = object ["ptr" .= t]

-- | convert a program from a JSON
instance FromJSON Bril.Program where
  parseJSON = withObject "Program" $ \v -> Bril.Program <$> v .: "functions"

-- | convert a program to a JSON
instance ToJSON Bril.Program where
  toJSON (Bril.Program fs) = object ["functions" .= fs]

-- | convert an argument from a JSON
instance FromJSON Bril.Argument where
  parseJSON = withObject "Argument" $ \v -> Bril.Argument <$> v .: "name" <*> v .: "type"

-- | convert an argument to a JSON
instance ToJSON Bril.Argument where
  toJSON (Bril.Argument n t) = object ["name" .= n, "type" .= t]

-- | convert a function from a JSON
instance FromJSON Bril.Function where
  parseJSON = withObject "Function" $ \v -> Bril.Function
                                              <$> v .:  "name"
                                              <*> v .:! "args"   .!= []
                                              <*> v .:! "type"
                                              <*> v .:! "instrs" .!= []

-- | convert a function to a JSON
instance ToJSON Bril.Function where
  toJSON (Bril.Function n as t is) = object $ ["name" .= n, "instrs" .= is] ++ args ++ typ
    where
      typ  = concat . maybeToList $ (\t ->["type" .= t]) <$> t
      args = ["args" .= as | not $ null as]

-- | convert an instruction from a JSON
instance FromJSON Bril.Instruction where
  parseJSON = withObject "Instruction" $ \v ->
    if M.member "label" v then Bril.Label <$> v .: "label"
    else do op <- (v .:  "op") :: Parser T.Text
            dt <-  v .:! "dest"
            tp <-  v .:! "type"
            vl <-  v .:! "value"
            as <-  v .:! "args"   .!= []
            fs <-  v .:! "funcs"  .!= []
            ls <-  v .:! "labels" .!= []
            let func   = head fs
            let first  = head as
            let true   = head ls
            let second = head $ tail as
            let false  = head $ tail ls
            let assgn  = Bril.Assignment <$> dt <*> tp
            case op of
              "nop"                                      -> return . Bril.Effect $ Bril.Nop
              "ret"                                      -> return . Bril.Effect $ Bril.Ret $ listToMaybe as
              "commit"                                   -> return . Bril.Effect $ Bril.Commit
              "speculate"                                -> return . Bril.Effect $ Bril.Speculate
              "br"     | not (null as) && length ls >= 2 -> return . Bril.Effect $ Bril.Br first true false
              "jmp"    | not $ null ls                   -> return . Bril.Effect $ Bril.Jmp true
              "print"  | not $ null as                   -> return . Bril.Effect $ Bril.Print first
              "store"  | length as >= 2                  -> return . Bril.Effect $ Bril.Store first second
              "free"   | not $ null as                   -> return . Bril.Effect $ Bril.Free first
              "guard"  | not (null as) && not (null ls)  -> return . Bril.Effect $ Bril.Guard first true
              "const"  | isJust vl                       -> return $ Bril.Value (Bril.Const $ fromJust vl) assgn
              "id"     | not $ null as                   -> return $ Bril.Value (Bril.Id first) assgn
              "alloc"  | not $ null as                   -> return $ Bril.Value (Bril.Alloc first) assgn
              "load"   | not $ null as                   -> return $ Bril.Value (Bril.Load first) assgn
              "call"   | not $ null fs                   -> return $ Bril.Value (Bril.Call func as) assgn
              "add"    | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.Add first second) assgn
              "sub"    | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.Sub first second) assgn
              "mul"    | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.Mul first second) assgn
              "div"    | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.Div first second) assgn
              "fadd"   | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.FAdd first second) assgn
              "fsub"   | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.FSub first second) assgn
              "fmul"   | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.FMul first second) assgn
              "fdiv"   | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.FDiv first second) assgn
              "lt"     | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.LT first second) assgn
              "gt"     | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.GT first second) assgn
              "le"     | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.LE first second) assgn
              "ge"     | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.GE first second) assgn
              "eq"     | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.EQ first second) assgn
              "flt"    | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.FLT first second) assgn
              "fgt"    | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.FGT first second) assgn
              "fle"    | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.FLE first second) assgn
              "fge"    | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.FGE first second) assgn
              "feq"    | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.FEQ first second) assgn
              "ptradd" | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.PtrAdd first second) assgn
              "and"    | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.And first second) assgn
              "or"     | length as >= 2                  -> return $ Bril.Value (Bril.BinOp Bril.Or first second) assgn
              "not"    | not $ null as                   -> return $ Bril.Value (Bril.UnOp Bril.Not first) assgn
              "phi"    | length as == length ls          -> return $ Bril.Value (Bril.Phi $ zip as ls) assgn
              _                                          -> fail $ "parsing instruction failed for op " ++ show op

-- | convert an instruction to a JSON
instance ToJSON Bril.Instruction where
  toJSON (Bril.Label l) = object ["label" .= l]
  toJSON instr          = object $ ["op" .= op] ++ assgn ++ args ++ funcs ++ labels ++ value
    where
      args   = let as = Bril.args instr in ["args" .= as | not $ null as]
      labels = let ls = Bril.labels instr in ["labels" .= ls | not $ null ls]
      funcs  = let fs = Bril.funcs instr in ["funcs" .= fs | not $ null fs]
      value  = concat . maybeToList $ (\v -> ["value" .= v]) <$> Bril.literal instr
      assgn  = concat . maybeToList $ (\(Bril.Assignment d t) -> ["dest" .= t, "type" .= t]) <$> Bril.assignment instr
      op     = case instr of
                 Bril.Effect i  -> Bril.op i
                 Bril.Value i _ -> Bril.op i
                 _              -> error "should never reach here"
