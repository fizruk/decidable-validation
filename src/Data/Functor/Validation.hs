{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Functor.Validation where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Functor.Contravariant.Generic
import Data.Monoid
import Data.Proxy
import Data.Void (absurd)

newtype Validation e a s = Validation { getValidation :: a -> s -> e }

instance Contravariant (Validation e a) where
  contramap f (Validation g) = Validation (\a -> g a . f)

instance Monoid e => Divisible (Validation e a) where
  divide split (Validation f) (Validation g)
    = Validation (\a s -> let (u, v) = split s in f a u <> g a v)
  conquer = Validation (\_ _ -> mempty)

instance Monoid e => Decidable (Validation e a) where
  choose k (Validation f) (Validation g)
    = Validation (\a -> either (f a) (g a) . k)
  lose k = Validation (\_ -> absurd . k)

class Validate e a s | s -> e a where
  validate :: a -> s -> e

gvalidate :: forall e a s. (Monoid e, Deciding (Validate e a) s) => a -> s -> e
gvalidate = getValidation $ deciding (Proxy :: Proxy (Validate e a)) (Validation validate)

