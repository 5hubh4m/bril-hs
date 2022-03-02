{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bril.Lang.AST where

import           Data.Bifunctor
import           Data.Hashable
import           GHC.Generics
import           Prelude       hiding (LT, GT, EQ)
import qualified Data.Text     as T

-- | the type of an identifier is a Text
newtype Ident = Ident
              { unIdent :: T.Text
              }
              deriving (Eq, Ord, Generic, Hashable)

-- | define a show instance for identifiers
instance Show Ident where
  show (Ident i) = T.unpack i

-- | a literal is either an integer or a float or a boolean
data Literal = Bool Bool
             | Int Integer
             | Float Double
             deriving (Show, Eq, Generic, Hashable)

-- | a type is either a integer or a boolean
--   or a pointer to a type
data Type = Integer
          | Boolean
          | Floating
          | Pointer Type
          deriving (Show, Eq, Generic, Hashable)

-- | a program is a list of functions
newtype Program = Program [Function]
                deriving (Show, Eq, Generic, Hashable)

-- | an argument is an identifier and it's type
data Argument = Argument Ident Type
              deriving (Show, Eq, Generic, Hashable)

-- | a function has a name, arguments, optionally
--   a type if it returns a value and a list of
--   instructions
data Function = Function Ident [Argument] (Maybe Type) [Instruction]
              deriving (Show, Eq, Generic, Hashable)

-- | defines all possible operations
--   possible in Bril
data Op = Add
        | Mul
        | Sub
        | Div
        | FAdd
        | FMul
        | FSub
        | FDiv
        | LT
        | GT
        | LE
        | GE
        | EQ
        | FLT
        | FGT
        | FLE
        | FGE
        | FEQ
        | And
        | Or
        | Not
        | PtrAdd
        deriving (Show, Eq, Generic, Hashable)

-- | an assignment is a destination along with
--   it's type
data Assignment = Assignment Ident Type
                deriving (Show, Eq, Generic, Hashable)

-- | this is an instruction that assigns to a variable
data ValueInstruction = Alloc Ident
                      | BinOp Op Ident Ident
                      | Call Ident [Ident]
                      | Const Literal
                      | Id Ident
                      | Load Ident
                      | Phi [(Ident, Ident)]
                      | UnOp Op Ident
                      deriving (Show, Eq, Generic, Hashable)

-- | this is an instruction that can perform side-effects
data EffectInstruction = Br Ident Ident Ident
                       | Commit
                       | Free Ident
                       | Guard Ident Ident
                       | Jmp Ident
                       | Nop
                       | Print Ident
                       | Ret (Maybe Ident)
                       | Speculate
                       | Store Ident Ident
                       deriving (Show, Eq, Generic, Hashable)

-- | defines all possible instructions in Bril
data Instruction = Label Ident
                 | Effect EffectInstruction
                 | Value ValueInstruction (Maybe Assignment)
                 deriving (Show, Eq, Generic, Hashable)

-- | given an instruction return it's list of arguments
args :: Instruction -> [Ident]
args (Effect (Br x _ _))     = [x]
args (Effect (Free x))       = [x]
args (Effect (Guard x _))    = [x]
args (Effect (Print x))      = [x]
args (Effect (Ret (Just x))) = [x]
args (Effect (Store x y))    = [x, y]
args (Value (Alloc x) _)     = [x]
args (Value (BinOp _ x y) _) = [x, y]
args (Value (Call _ xs) _)   = xs
args (Value (Id x) _)        = [x]
args (Value (Load x) _)      = [x]
args (Value (Phi xs) _)      = fst <$> xs
args (Value (UnOp _ x) _)    = [x]
args _                       = []

-- | given an instruction return it's list of labels
labels :: Instruction -> [Ident]
labels (Label x)            = [x]
labels (Effect (Br _ x y))  = [x, y]
labels (Effect (Guard _ x)) = [x]
labels (Effect (Jmp x))     = [x]
labels (Value (Phi xs) _)   = snd <$> xs
labels _                    = []

-- | given an instruction return it's list of funcs
funcs :: Instruction -> [Ident]
funcs (Value (Call x _) _) = [x]
funcs _                    = []

-- | given an instruction return the value it contains
literal :: Instruction -> Maybe Literal
literal (Value (Const v) _) = Just v
literal _                   = Nothing

-- | given an instruction return it's assignment
assignment :: Instruction -> Maybe Assignment
assignment (Value _ a) = a
assignment _           = Nothing

-- | change the arguments of an instruction by
--   applying the given function
mapArgs :: (Ident -> Ident) -> Instruction -> Instruction
mapArgs f (Effect (Br cond tl fl)) = Effect $ Br (f cond) tl fl
mapArgs f (Effect (Free x))        = Effect $ Free (f x)
mapArgs f (Effect (Guard x l))     = Effect $ Guard (f x) l
mapArgs f (Effect (Print x))       = Effect $ Print (f x)
mapArgs f (Effect (Ret x))         = Effect $ Ret (f <$> x)
mapArgs f (Effect (Store x y))     = Effect $ Store (f x) (f y)
mapArgs f (Value (Alloc x) a)      = Value (Alloc (f x)) a
mapArgs f (Value (BinOp o x y) a)  = Value (BinOp o (f x) (f y)) a
mapArgs f (Value (Call fn as) a)   = Value (Call fn (f <$> as)) a
mapArgs f (Value (Id x) a)         = Value (Id (f x)) a
mapArgs f (Value (Load y) a)       = Value (Load (f y)) a
mapArgs f (Value (UnOp o x) a)     = Value (UnOp o (f x)) a
mapArgs f (Value (Phi xs) a)       = Value (Phi $ first f <$> xs) a
mapArgs _ x                        = x

-- | change the destination of an instruction by
--   applying the given function
mapDest :: (Ident -> Ident) -> Instruction -> Instruction
mapDest f (Value x (Just (Assignment d t))) = Value x . Just $ Assignment (f d) t
mapDest _ x                                 = x

-- | defines an operation to extract an op object
--   from an instruction
class InstrOp a where
  op :: a -> Ident

-- | define an instance of InstrOp for Op values
instance InstrOp Op where
  op Add    = Ident "add"
  op Mul    = Ident "mul"
  op Sub    = Ident "sub"
  op Div    = Ident "div"
  op FAdd   = Ident "fadd"
  op FMul   = Ident "fmul"
  op FSub   = Ident "fsub"
  op FDiv   = Ident "fdiv"
  op LT     = Ident "lt"
  op GT     = Ident "gt"
  op LE     = Ident "le"
  op GE     = Ident "ge"
  op EQ     = Ident "eq"
  op FLT    = Ident "flt"
  op FGT    = Ident "fgt"
  op FLE    = Ident "fle"
  op FGE    = Ident "fge"
  op FEQ    = Ident "feq"
  op And    = Ident "and"
  op Or     = Ident "or"
  op Not    = Ident "not"
  op PtrAdd = Ident "ptradd"

-- | define an instance of InstrOp for value instructions
instance InstrOp ValueInstruction where
  op (BinOp o _ _) = op o
  op (UnOp o _)    = op o
  op Alloc {}      = Ident "alloc"
  op Call {}       = Ident "call"
  op Const {}      = Ident "const"
  op Id {}         = Ident "id"
  op Load {}       = Ident "load"
  op Phi {}        = Ident "phi"

-- | define an instance of InstrOp for effect instructions
instance InstrOp EffectInstruction where
  op Br {}     = Ident "br"
  op Commit    = Ident "commit"
  op Free {}   = Ident "free"
  op Guard {}  = Ident "guard"
  op Jmp {}    = Ident "jmp"
  op Nop       = Ident "nop"
  op Print {}  = Ident "print"
  op Ret {}    = Ident "ret"
  op Speculate = Ident "speculate"
  op Store {}  = Ident "store"

-- | defines an operation to extract whether the instruction has a side effect
class EffectOp a where
  effect :: a -> Bool

-- | define an instance of EffectOp for effect instructions
instance EffectOp EffectInstruction where
  effect Nop = False
  effect _   = True

-- | define an instance of EffectOp for value instructions
instance EffectOp ValueInstruction where
  effect Call {}  = True
  effect Alloc {} = True
  effect _        = False

-- | whether a given instruction is a terminating instruction
terminator :: Instruction -> Bool
terminator (Effect Jmp {})  = True
terminator (Effect Ret {})  = True
terminator (Effect Br {})   = True
terminator _                = False
