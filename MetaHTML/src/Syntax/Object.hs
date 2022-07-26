module Syntax.Object where

import Data.Text

data Id = Id !Text !Integer deriving (Show, Eq, Ord)