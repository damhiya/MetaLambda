module MetaLambda.Syntax where

-- identifier
data Id = Id !String !Integer deriving (Show, Eq, Ord)
data GId = GId !String !Integer deriving (Show, Eq)

-- type and local contex
data Type
  = Base
  | Bool
  | Int
  | Prod Type Type
  | List Type
  | Arr Type Type
  | BoxT LCtx Type
  deriving (Show, Eq)
type LCtx = [(Id, Type)]
type LECtx = [Id]

-- term definition
data PrimOp t
  = IntEq t t
  | IntLe t t
  | IntAdd t t
  | IntSub t t
  | IntMul t t
  | IntDiv t t
  | IntMod t t
  | IntPow t t
  | Inject t
  deriving Show

data Term
  = Var Id
  -- bool
  | BTrue
  | BFalse
  | BoolMatch Term Term Term
  -- integer
  | IntLit Integer
  -- product
  | Pair Term Term
  | ProdMatch Term Id Id Term
  -- list
  | Nil Type
  | Cons Term Term
  | ListMatch Term Term Id Id Term
  -- function
  | Lam Id Type Term
  | App Term Term
  -- box
  | Box LCtx Term
  | LetBox LECtx GId Term Term
  -- others
  | Clo GId [Term]
  | PrimOp (PrimOp Term)
  deriving Show
