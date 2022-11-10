module Util where

import           Control.Monad.Except

with :: MonadError e m => e -> Maybe a -> m a
with e Nothing  = throwError e
with e (Just x) = pure x

guardWith :: MonadError e m => e -> Bool -> m ()
guardWith e False = throwError e
guardWith e True  = pure ()
