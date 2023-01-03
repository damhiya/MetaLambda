module Data.MapList where

import           Data.Foldable
import           Data.Functor.Compose
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.Map             as M
import qualified Data.Map.Merge.Lazy  as M
import qualified Util                 as U

type MapList k a = M.Map k (NonEmpty a)

insert :: Ord k => k -> [a] -> MapList k a -> MapList k a
insert k []     m = m
insert k (x:xs) m = M.insert k (x :| xs) m

lookup :: Ord k => k -> MapList k a -> [a]
lookup k m = (toList . Compose . M.lookup k) m

consAt :: Ord k => k -> a -> MapList k a -> MapList k a
consAt k x m = M.alter (\xs -> Just (x :| (toList . Compose) xs)) k m

allKeys :: (k -> Bool) -> MapList k a -> Bool
allKeys p = M.foldrWithKey (\k _ b -> p k && b) True

zipWithExact :: Ord k => (a -> b -> c) -> MapList k a -> MapList k b -> Maybe (MapList k c)
zipWithExact f = M.mergeA missing missing matched
  where
    missing = M.traverseMissing $
      \k x -> Nothing
    matched = M.zipWithAMatched $
      \k (x :| xs) (y :| ys) -> (f x y :|) <$> U.zipWithExact f xs ys
