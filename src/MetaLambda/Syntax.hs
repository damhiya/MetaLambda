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
  | IntLt t t
  | IntAdd t t
  | IntSub t t
  | IntMul t t
  | IntDiv t t
  | IntMod t t
  | IntPow t t
  | Inject t
  deriving (Show, Functor)

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
  | Fix Type Type Id Id Term
  | App Term Term
  -- box
  | Box LCtx Term
  | LetBox LECtx GId Term Term
  -- others
  | Clo GId [Term]
  | Let Id Term Term
  | PrimOp (PrimOp Term)
  deriving Show

data Value
  = VTrue
  | VFalse
  | VInt Integer
  | VPair Value Value
  | VNil Type
  | VCons Value Value
  | VLam Id Type Term
  | VFix Type Type Id Id Term
  | VBox LCtx Term

liftToTerm :: Value -> Term
liftToTerm = \case
  VTrue -> BTrue
  VFalse -> BFalse
  VInt n -> IntLit n
  VPair v1 v2 -> Pair (liftToTerm v1) (liftToTerm v2)
  VNil t -> Nil t
  VCons v1 v2 -> Cons (liftToTerm v1) (liftToTerm v2)
  VLam x t e -> Lam x t e
  VFix t1 t2 f x e -> Fix t1 t2 f x e
  VBox octx e -> Box octx e
