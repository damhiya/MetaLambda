module Syntax.Object where

-- identifier, type, local context
newtype Id = Id String deriving (Show, Eq)

data Ty = Base | Arr Ty Ty deriving (Show, Eq)

type LCtx = [(Id, Ty)]
type LECtx = [Id]

erase :: LCtx -> LECtx
erase = map fst

-- term definition
data Tm m
  = Var Id
  | Abs Id Ty (Tm m)
  | App (Tm m) (Tm m)
  | Meta m
  deriving Show
