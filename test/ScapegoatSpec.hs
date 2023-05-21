module ScapegoatSpec where

import           Data.Foldable
import           Data.Foldable1
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import qualified Scapegoat
import           Scapegoat.Measured
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "scapegoat tests" $ do
         describe "building balanced trees" $ do
           prop "perfect builds perfect trees" $ \(xs :: NonEmpty.NonEmpty Int) ->
             Scapegoat.isPerfect $ Scapegoat.fromAscList (NonEmpty.sort $ Elem <$> xs)
         prop "sorting test" $ \(xs :: NonEmpty.NonEmpty Int) ->
           sortScapegoat xs == NonEmpty.sort xs
         prop "deletion tests" $ \(xs0 :: NonEmpty.NonEmpty (Elem Int)) n ->
           let xs = NonEmpty.nub xs0
               ys = NonEmpty.take (min n $ (length xs) - 1) xs
           in deleteAll xs ys
         prop "lookupGE tests" $  \(xs0 :: NonEmpty.NonEmpty (Elem Int)) ->
           let (xs,queries) = select xs0
               t            = Scapegoat.fromNonEmpty xs
           in all (sameAsNaiveGE xs t) queries
         prop "lookupLE tests" $  \(xs0 :: NonEmpty.NonEmpty (Elem Int)) ->
           let (xs,queries) = select xs0
               t            = Scapegoat.fromNonEmpty xs
           in all (sameAsNaiveLE  xs t) queries
         prop "lookupLE tests" $  \(xs0 :: NonEmpty.NonEmpty (Elem Int)) ->
           let (xs,queries) = select xs0
               t            = Scapegoat.fromNonEmpty xs
           in all (sameAsNaiveEQ xs t) xs


--------------------------------------------------------------------------------

newtype Elem a = Elem a
  deriving newtype (Show,Eq,Ord,Arbitrary)

instance Measured () (Elem a) where
  measure _ = ()

instance DynMeasured () (Elem a) where
  deleteFrom _ _ = ()

--------------------------------------------------------------------------------

deleteAll xs ys = toList (foldr delete' (Scapegoat.fromNonEmpty xs) ys)
                  ==
                  List.sort (toList xs List.\\ toList ys)

delete' x t = fromJust $ Scapegoat.delete x t

--------------------------------------------------------------------------------

select           :: NonEmpty.NonEmpty a -> (NonEmpty.NonEmpty a, [a])
select (x :| xs) = go (x:|[]) [] xs
  where
    go ys qs = \case
      []       -> (ys,qs)
      q:[]     -> (ys,q:qs)
      q:y:rest -> go (y NonEmpty.<| ys) (q:qs) rest

sameAsNaiveGE        :: Ord a => NonEmpty.NonEmpty a -> Scapegoat.ScapegoatTree v a -> a -> Bool
sameAsNaiveGE xs t q = Scapegoat.lookupGE q t == naive (toList xs)
  where
    naive xs' = case  [ x | x <- xs' , q <= x ] of
                  []   -> Nothing
                  xs'' -> Just $ List.minimum xs''

sameAsNaiveLE         :: Ord a => NonEmpty.NonEmpty a -> Scapegoat.ScapegoatTree v a -> a -> Bool
sameAsNaiveLE  xs t q = Scapegoat.lookupLE q t == naive (toList xs)
  where
    naive xs' = case  [ x | x <- xs' , q >= x ] of
                  []   -> Nothing
                  xs'' -> Just $ List.maximum xs''

sameAsNaiveEQ        :: Ord a => NonEmpty.NonEmpty a -> Scapegoat.ScapegoatTree v a -> a -> Bool
sameAsNaiveEQ xs t q = Scapegoat.lookup q t == naive (toList xs)
  where
    naive xs' = not . null $  [ x | x <- xs' , q == x ]

--------------------------------------------------------------------------------

-- | Sort by repeatedly inserting into a scapegoat tree
sortScapegoat :: Ord a => NonEmpty a -> NonEmpty a
sortScapegoat = fmap (\(Elem x) -> x) . toNonEmpty . Scapegoat.fromNonEmpty . fmap Elem
