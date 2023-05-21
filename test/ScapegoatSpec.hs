module ScapegoatSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Scapegoat
import           Scapegoat.Measured
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "scapegoat tests" $ do
         describe "building balanced trees" $ do
           prop "perfect builds perfect trees" $ \(xs :: NonEmpty.NonEmpty Int) ->
             Scapegoat.isPerfect $ Scapegoat.fromAscList (NonEmpty.sort $ Elem <$> xs)


--------------------------------------------------------------------------------

newtype Elem a = Elem a
  deriving newtype (Show,Eq,Ord)

instance Measured () (Elem a) where
  measure _ = ()

instance DynMeasured () (Elem a) where
  deleteFrom _ _ = ()
