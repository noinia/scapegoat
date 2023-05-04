module Scapegoat
  ( ScapegoatTree
  , fromAscList
  , fromAscListN
  , lookupGE
  , lookupLE
  , insert
  , delete
  ) where

import           Control.Applicative ((<|>))
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Prelude hiding (minimum,maximum)

--------------------------------------------------------------------------------

data ScapegoatTree v a = ScapegoatTree { size    :: {-# UNPACK #-} !Int
                                       , maxSize :: {-# UNPACK #-} !Int
                                       , tree    :: Tree v a
                                       }
                         deriving stock (Show,Eq,Functor)

instance Foldable (ScapegoatTree v) where
  foldMap f = foldMap f . tree
  length t = size t

instance Foldable1 (ScapegoatTree v) where
  foldMap1 f = foldMap1 f . tree
  minimum = minimum . tree
  maximum = maximum . tree

-- pre: Tree is a BST
data Tree v a = Leaf !a
              | Node (Tree v a) !a !v (Tree v a)
              deriving stock (Show,Eq,Functor)

instance Foldable (Tree v) where
  foldMap f = \case
    Leaf x       -> f x
    Node l _ _ r -> foldMap f l <> foldMap f r


instance Foldable1 (Tree v) where
  foldMap1 f = \case
    Leaf x       -> f x
    Node l _ _ r -> foldMap1 f l <> foldMap1 f r
  minimum = \case
    Leaf x       -> x
    Node l _ _ _ -> minimum l
  maximum = \case
    Leaf x       -> x
    Node _ _ _ r -> maximum r


--------------------------------------------------------------------------------

class Semigroup v => Measured v a | a -> v  where
  measure :: a -> v

class Measured v a => DynMeasured v a where
  {-# MINIMAL deleteFrom #-}
  insertInto :: a -> v -> v
  insertInto x v = measure x <> v

  deleteFrom :: a -> v -> v

instance Measured v a => Measured v (Tree v a) where
  measure = \case
    Leaf x       -> measure x
    Node _ _ v _ -> v


--------------------------------------------------------------------------------
-- * Queries

lookupGE   :: Ord a => a -> ScapegoatTree v a -> Maybe a
lookupGE q = go . tree
  where
    go = \case
      Leaf x | q <= x    -> Just x
             | otherwise -> Nothing
      Node l k _ r | q <= k    -> go l <|> Just (minimum r)
                   | otherwise -> go r

lookupLE :: Ord a => a -> ScapegoatTree v a -> Maybe a
lookupLE = undefined

--------------------------------------------------------------------------------

insert     :: (Ord a, DynMeasured v a) => a -> ScapegoatTree v a -> ScapegoatTree v a
insert x t = undefined

-- insert' :: Ord a => a -> Tree v a -> Tree v a
-- insert' x t = go
--   where
--     go = \case
--       Leaf x -> Node



--------------------------------------------------------------------------------
-- * Rebuilding

fromAscList   :: (Foldable1 f, Measured v a) => f a -> ScapegoatTree v a
fromAscList xs = fromAscListN (length xs) xs

fromAscListN   :: (Foldable1 f, Measured v a) => Int -> f a -> ScapegoatTree v a
fromAscListN n = ScapegoatTree n n . perfect


data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
             deriving stock (Show,Eq)

-- | Builds a perfectly balanced tree of the given size
perfect    :: (Foldable1 f, Measured v a) => f a -> Tree v a
perfect xs = case perfect' (length xs) (toNonEmpty xs) of
               (t, []) -> fst $ fromTree' t
               _       -> error "perfect: leftover elements, absurd."

-- | Transforms our Tree' into a proper Tree, returns the tree as well
-- as the rightmost element.
fromTree' :: Measured v a => Tree' a -> (Tree v a, a)
fromTree' = \case
  Leaf' x     -> (Leaf x, x)
  Node' l' r' -> let (l,k) = fromTree' l'
                     (r,m) = fromTree' r'
                 in (Node l k (measure l <> measure r) r, m)

-- | Builds a perfectly balanced tree of the given size n. Returns
-- whatever is left of the input xs.
--
-- pre: the input xs has at least s elements
--
perfect'              :: Int -- ^ the desired size n
                      -> NonEmpty a -- ^ the elements,
                      -> (Tree' a, [a])
perfect' 1 (x :| xs') = (Leaf' x, xs')
perfect' n xs         = let k         = n `div` 2
                            (l, xs')  = perfect' k       xs
                            (r, xs'') = perfect' (n-k) $ NonEmpty.fromList xs'
                                        -- by precondition, NonEmtpy.fromList is safe
                        in (Node' l r, xs'')

--------------------------------------------------------------------------------
-- * Deletions

delete     :: (Ord a, DynMeasured v a) => a -> ScapegoatTree v a -> Maybe (ScapegoatTree v a)
delete x t = case delete' x (tree t) of
               Left _                                  -> Just t
               Right Nothing                           -> Nothing
               Right (Just t') | n < maxSize t `div` 2 -> Just $ fromAscListN n t'
                               | otherwise             -> Just $ t { tree = t' }
  where
    n = pred $ size t

--
delete'   :: (Ord a, DynMeasured v a)
          => a -> Tree v a -> Either (Tree v a) -- original tree if unchanged
                                     (Maybe (Tree v a)) -- new tree, if there is one still
delete' x = go
  where
    go t = case t of
      Leaf y | x == y    -> Right Nothing
             | otherwise -> Left t
      Node l k v r | x <= k    -> case go l of
                                    Left _          -> Left t -- unchanged
                                    Right Nothing   -> Right . Just $ r
                                    Right (Just l') -> Right . Just $ Node l' k (deleteFrom x v) r
                   | otherwise -> case go r of
                                    Left _          -> Left t
                                    Right Nothing   -> Right . Just $ l
                                    Right (Just r') -> Right . Just $ Node l k (deleteFrom x v) r'
