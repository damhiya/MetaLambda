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
type GCtx = [(GId, (LCtx, Type))]
type Subst = [(Id, Term)]

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
  = TVar Id
  -- bool
  | TTrue
  | TFalse
  | TBoolMatch Term Term Term
  -- integer
  | TInt Integer
  -- product
  | TPair Term Term
  | TProdMatch Term Id Id Term
  -- list
  | TNil Type
  | TCons Term Term
  | TListMatch Term Term Id Id Term
  -- function
  | TLam Id Type Term
  | TFix Type Type Id Id Term
  | TApp Term Term
  -- box
  | TBox LCtx Term
  | TLetBox GId Term Term
  | TClo GId Subst
  -- others
  | TLet Id Term Term
  | TPrimOp (PrimOp Term)
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
  deriving Show

-- helper functions
lookupId :: LCtx -> Id -> Maybe Type
lookupId ctx x = lookup x ctx

lookupGId :: GCtx -> GId -> Maybe (LCtx, Type)
lookupGId gctx u = lookup u gctx

erase :: LCtx -> LECtx
erase = map fst

lookupSubst :: Subst -> Id -> Maybe Term
lookupSubst s x = lookup x s

idSubst :: LCtx -> Subst
idSubst ctx = map (\(x,_) -> (x, TVar x)) ctx

liftToTerm :: Value -> Term
liftToTerm = \case
  VTrue -> TTrue
  VFalse -> TFalse
  VInt n -> TInt n
  VPair v1 v2 -> TPair (liftToTerm v1) (liftToTerm v2)
  VNil t -> TNil t
  VCons v1 v2 -> TCons (liftToTerm v1) (liftToTerm v2)
  VLam x t e -> TLam x t e
  VFix t1 t2 f x e -> TFix t1 t2 f x e
  VBox octx e -> TBox octx e
