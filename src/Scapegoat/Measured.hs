module Scapegoat.Measured
  ( Measured(..)
  , DynMeasured(..)
  ) where

--------------------------------------------------------------------------------

class Semigroup v => Measured v a | a -> v  where
  measure :: a -> v

class Measured v a => DynMeasured v a where
  {-# MINIMAL deleteFrom #-}
  insertInto     :: a -> v -> v
  insertInto x v = measure x <> v

  deleteFrom :: a -> v -> v
