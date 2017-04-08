{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Boltzmann.Species.System where

import Control.Applicative
import Data.Foldable (asum)
import Data.List (inits)
import Data.Proxy
import Data.Vector (Vector)
import GHC.Exts (Constraint)
import GHC.Prim (Any)
import GHC.TypeLits
import Numeric.Natural
import Unsafe.Coerce
import qualified Data.Vector as V

import Boltzmann.Data.Common (binomial)

newtype System_ (e :: * -> *) (d :: [(k, *)]) = System_ (Vector Any)

type family Index a d where
  Index a ('(a, _) ': _) = 0
  Index a (_ ': d) = 1 + Index a d

type family Lookup a (d :: [(k, *)]) where
  Lookup a ('(a, b) ': _) = b
  Lookup a (_ ': d) = Lookup a d

lookupSystem_
  :: forall a d e n
  .  (n ~ Index a d, KnownNat n)
  => System_ e d
  -> F e a (Lookup a d)
lookupSystem_ (System_ v) = unsafeCoerce (v V.! fromInteger (natVal @n Proxy))

system :: PreSystem e d d -> System_ e d
system (PreSystem a) = System_ (V.fromList a)

-- |
--
-- @
-- PreSystem e \'['(a1, b1), '(a2, b2), '(a3, b3)]
--   = [F e b1, F e b2, F e b3]
-- @
newtype PreSystem e d d0 = PreSystem [Any]

emptySys :: PreSystem e '[] d0
emptySys = PreSystem []

(/\)
  :: forall e (d0 :: [(k, *)]) (a :: k) b (d :: [(k, *)])
  .  (Equation e, Injection e d0)
  => e b -> PreSystem e d d0 -> PreSystem e ('(a, b) ': d) d0
(/\) eq (PreSystem sys) = PreSystem (coerceEquation (equation @a @d0 eq) : sys)
  where
    coerceEquation :: F e a b -> Any
    coerceEquation = unsafeCoerce

infixr 3 /\

class Alternative f => Sized f where
  pay :: f a -> f a

  (<.>) :: Natural -> f a -> f a
  (<.>) = duplicate

duplicate :: Alternative f => Natural -> f a -> f a
duplicate 0 _ = empty
duplicate n f = f <|> duplicate (n - 1) f

class Sized e => Equation e where
  type F e (a :: k) b
  type Injection e (d :: [(k, *)]) :: Constraint

  rhs_
    :: (Injection e d, KnownNat (Index a d), b ~ Lookup a d)
    => proxy a -> proxy' d
    -> F e a b -> e b
  equation_
    :: Injection e d
    => proxy a -> proxy' d
    -> e b -> F e a b

type Assoc a d b = (KnownNat (Index a d), Lookup a d ~ b)

rhs
  :: forall a d e b
  .  (Equation e, Injection e d, Assoc a d b)
  => F e a b -> e b
rhs = rhs_ (Proxy @a) (Proxy @d)

equation
  :: forall a d e b
  .  (Equation e, Injection e d)
  => e b -> F e a b
equation = equation_ (Proxy @a) (Proxy @d)

lookupSys
    :: forall a d e n proxy
    .  (n ~ Index a d, KnownNat n, Injection e d, Equation e)
    => System_ e d
    -> e (Lookup a d)
lookupSys = rhs @a @d . lookupSystem_ @a

newtype Inject e a = Inject (e a)
  deriving (Functor, Applicative, Alternative, Sized)

instance Equation e => Equation (Inject e) where
  type F (Inject e) a b = F e a b
  type Injection (Inject e) d = (?injection :: System_ e d, Injection e d)

  rhs_ (a :: proxy a) (d :: proxy' d) _ =
    Inject (rhs_ a d (lookupSystem_ @a @d ?injection))
  equation_ a d (Inject e) = equation_ a d e

newtype GFunction x a = GFunction (x -> x)

instance Num x => Equation (GFunction x) where
  type F (GFunction x) a b = x
  type Injection (GFunction x) d = (?gfx :: x)

  rhs_ _ _ = GFunction . const
  equation_ _ _ (GFunction f) = f ?gfx

instance Functor (GFunction x) where
  fmap f (GFunction x) = GFunction x

instance Num x => Applicative (GFunction x) where
  pure _ = GFunction (const 1)
  GFunction x1 <*> GFunction x2 = GFunction (liftA2 (*) x1 x2)

instance Num x => Alternative (GFunction x) where
  empty = GFunction (const 0)
  GFunction x1 <|> GFunction x2 = GFunction (liftA2 (+) x1 x2)

instance Num x => Sized (GFunction x) where
  pay (GFunction f) = GFunction (\x -> x * f x)

-- Quite unsafe.
applySystem
  :: forall x d
  .  Num x
  => (Injection (Inject (GFunction x)) d => System_ (Inject (GFunction x)) d)
  -> x -> Vector x -> Vector x
applySystem f x xs =
  coerceToVector (let ?injection = coerceFromVector xs ; ?gfx = x in f)
  where
    coerceToVector :: System_ (Inject (GFunction x)) d -> Vector x
    coerceFromVector :: Vector x -> System_ (GFunction x) d
    coerceToVector = unsafeCoerce
    coerceFromVector = unsafeCoerce

newtype Pointed f a
  = Pointed [f a]  -- ^ Infinite or empty lists only.

takePointed :: Int -> Pointed f a -> [f a]
takePointed n (Pointed f) = take n f

instance Functor f => Functor (Pointed f) where
  fmap f (Pointed v) = Pointed ((fmap . fmap) f v)

instance Sized f => Applicative (Pointed f) where
  pure a = Pointed (pure a : repeat empty)
  Pointed fs <*> Pointed xs = Pointed (convolute fs xs)
    where
      convolute fs xs = zipWith sumOfProducts [0 ..] ((tail . inits) xs)
      sumOfProducts k x = asum (zipWith3 (times k) [0 ..] fs (reverse x))
      times k k1 f x = binomial k k1 <.> f <*> x

instance Sized f => Alternative (Pointed f) where
  empty = Pointed []
  Pointed [] <|> ys = ys
  xs <|> Pointed [] = xs
  Pointed xs <|> Pointed ys = Pointed (zipWith (<|>) xs ys)

instance Sized f => Sized (Pointed f) where
  pay (Pointed fs) = Pointed self
    where
      self = zipWith' (<|>) (empty : self) (fmap pay fs)

zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' f (x : xs) ~(y : ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = error "pay empty"

newtype Coefficients a = Coefficients [Integer]

takeCoefficients :: Int -> Coefficients a -> [Integer]
takeCoefficients n (Coefficients cs) = take n cs

instance Functor Coefficients where
  fmap _ (Coefficients cs) = Coefficients cs

instance Applicative Coefficients where
  pure _ = Coefficients (1 : repeat 0)
  Coefficients xs <*> Coefficients ys = Coefficients (cauchy xs ys)
    where
      cauchy xs ys = tail (fmap sumOfProducts (inits ys))
      sumOfProducts = sum . zipWith (*) xs . reverse

instance Alternative Coefficients where
  empty = Coefficients (repeat 0)
  Coefficients xs <|> Coefficients ys = Coefficients (zipWith (+) xs ys)

instance Sized Coefficients where
  pay (Coefficients xs) = Coefficients (0 : xs)

newtype Wrapped f a = Wrapped (f a)
  deriving (Functor, Applicative, Alternative, Sized)

instance Sized f => Equation (Wrapped f) where
  type F (Wrapped f) a b = Wrapped f b
  type Injection (Wrapped f) d = ()

  rhs_ _ _ = id
  equation_ _ _ = id
