module Syntax where

-- identifier
data Id = Id !String !Integer deriving (Show, Eq, Ord)
data GId = GId !String !Integer deriving (Show, Eq)

-- type and local contex
data Type = Base | Arr Type Type | BoxT LCtx Type deriving (Show, Eq)
type LCtx = [(Id, Type)]
type LECtx = [Id]

-- term definition
data Term
  = Var Id
  | Lam Id Type Term
  | App Term Term
  | Box LCtx Term
  | LetBox LECtx GId Term Term
  | Clo GId [Term]
  deriving Show
