{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Bril.Lang.AST where

import           Control.Lens
import           Data.Hashable
import           Data.Maybe
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
data Function = Function
              { _fname   :: Ident
              , _fargs   :: [Argument]
              , _fret    :: Maybe Type
              , _finstrs :: [Instruction]
              }
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
                      | Constant Literal
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

-- | make lenses for function
makeLenses ''Function

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
args (Value (Phi xs) _)      = (^. _1) <$> xs
args (Value (UnOp _ x) _)    = [x]
args _                       = []
{-# INLINABLE args #-}

-- | given an instruction return it's list of labels
labels :: Instruction -> [Ident]
labels (Label x)            = [x]
labels (Effect (Br _ x y))  = [x, y]
labels (Effect (Guard _ x)) = [x]
labels (Effect (Jmp x))     = [x]
labels (Value (Phi xs) _)   = (^. _2) <$> xs
labels _                    = []
{-# INLINABLE labels #-}

-- | given an instruction return it's list of funcs
funcs :: Instruction -> [Ident]
funcs (Value (Call x _) _) = [x]
funcs _                    = []
{-# INLINABLE funcs #-}

-- | given an instruction return the value it contains
literal :: Instruction -> Maybe Literal
literal (Value (Constant v) _) = Just v
literal _                      = Nothing
{-# INLINABLE literal #-}

-- | given an instruction return it's assignment
assignment :: Instruction -> Maybe Assignment
assignment (Value _ a) = a
assignment _           = Nothing
{-# INLINABLE assignment #-}

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
mapArgs f (Value (Phi xs) a)       = Value (Phi $ (_1 %~ f) <$> xs) a
mapArgs _ x                        = x
{-# INLINABLE mapArgs #-}

-- | change the destination of an instruction by
--   applying the given function
mapDest :: (Ident -> Ident) -> Instruction -> Instruction
mapDest f (Value x (Just (Assignment d t))) = Value x . Just $ Assignment (f d) t
mapDest _ x                                 = x
{-# INLINABLE mapDest #-}

-- | defines an operation to extract an opName object
--   from an instruction
class InstrOp a where
  opName :: a -> Ident

-- | define an instance of InstrOp for Op values
instance InstrOp Op where
  opName Add    = Ident "add"
  opName Mul    = Ident "mul"
  opName Sub    = Ident "sub"
  opName Div    = Ident "div"
  opName FAdd   = Ident "fadd"
  opName FMul   = Ident "fmul"
  opName FSub   = Ident "fsub"
  opName FDiv   = Ident "fdiv"
  opName LT     = Ident "lt"
  opName GT     = Ident "gt"
  opName LE     = Ident "le"
  opName GE     = Ident "ge"
  opName EQ     = Ident "eq"
  opName FLT    = Ident "flt"
  opName FGT    = Ident "fgt"
  opName FLE    = Ident "fle"
  opName FGE    = Ident "fge"
  opName FEQ    = Ident "feq"
  opName And    = Ident "and"
  opName Or     = Ident "or"
  opName Not    = Ident "not"
  opName PtrAdd = Ident "ptradd"
  {-# INLINABLE opName #-}

-- | define an instance of InstrOp for value instructions
instance InstrOp ValueInstruction where
  opName (BinOp o _ _) = opName o
  opName (UnOp o _)    = opName o
  opName Alloc {}      = Ident "alloc"
  opName Call {}       = Ident "call"
  opName Constant {}   = Ident "const"
  opName Id {}         = Ident "id"
  opName Load {}       = Ident "load"
  opName Phi {}        = Ident "phi"
  {-# INLINABLE opName #-}

-- | define an instance of InstrOp for effect instructions
instance InstrOp EffectInstruction where
  opName Br {}     = Ident "br"
  opName Commit    = Ident "commit"
  opName Free {}   = Ident "free"
  opName Guard {}  = Ident "guard"
  opName Jmp {}    = Ident "jmp"
  opName Nop       = Ident "nop"
  opName Print {}  = Ident "print"
  opName Ret {}    = Ident "ret"
  opName Speculate = Ident "speculate"
  opName Store {}  = Ident "store"
  {-# INLINABLE opName #-}

-- | whether a given instruction is a terminating instruction
terminator :: Instruction -> Bool
terminator (Effect Jmp {}) = True
terminator (Effect Ret {}) = True
terminator (Effect Br {})  = True
terminator _               = False
{-# INLINABLE terminator #-}

-- | insert the given list of instructions in a given basic block
insertAfter :: [Instruction] -> [Instruction] -> [Instruction]
insertAfter block is = case listToMaybe $ reverse block of
                         Just li | terminator li -> init block ++ is ++ [li]
                         _                       -> block ++ is
