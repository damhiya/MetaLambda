module Syntax.Meta where

import qualified Syntax.Object as O

-- identifier, type, local context
newtype Id = Id String deriving (Show, Eq)
newtype GId = GId String deriving (Show, Eq)

data Ty = BoxT O.LCtx O.Ty | Arr Ty Ty deriving Show

instance Eq Ty where
  Arr a0 b0 == Arr a1 b1 = a0 == a1 && b0 == b1
  BoxT ctx0 a0 == BoxT ctx1 a1 = map snd ctx0 == map snd ctx1 && a0 == a1
  _ == _ = False

type LCtx = [(Id, Ty)]

-- term definition
data M = Inst GId [O.Tm M] deriving Show
data Tm
  = Var Id
  | Abs Id Ty Tm
  | App Tm Tm
  | Box O.LCtx (O.Tm M)
  | LetBox O.LECtx GId Tm Tm
  deriving Show
