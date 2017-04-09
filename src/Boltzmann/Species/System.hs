{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Boltzmann.Species.System where

import Control.Applicative
import Data.Constraint.Forall
import Data.Foldable (asum)
import Data.List (inits)
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Vector (Vector)
import GHC.Exts (Constraint)
import GHC.Prim (Any)
import GHC.TypeLits
import Numeric.Natural
import Unsafe.Coerce
import qualified Numeric.AD as AD
import qualified Data.Vector as V

import Boltzmann.Data.Common (binomial)
import Boltzmann.Solver

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
    :: (Injection e d, Assoc a d b)
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

newtype GFunction x a = GFunction ((?gfx :: x) => x)

instance Num x => Equation (GFunction x) where
  type F (GFunction x) a b = x
  type Injection (GFunction x) d = (?gfx :: x)

  rhs_ _ _ = GFunction
  equation_ _ _ (GFunction f) = f

instance Functor (GFunction x) where
  fmap _ (GFunction x) = GFunction x

instance Num x => Applicative (GFunction x) where
  pure _ = GFunction 1
  GFunction xf <*> GFunction xa = GFunction (xf * xa)

instance Num x => Alternative (GFunction x) where
  empty = GFunction 0
  GFunction x1 <|> GFunction x2 = GFunction $ x1 + x2

instance Num x => Sized (GFunction x) where
  pay (GFunction x) = GFunction (?gfx * x)

data WFunctor x f a = WFunctor !((?gfx :: x) => x) ((?gfx :: x) => f a)

instance (Num x, WAlternative x f) => Equation (WFunctor x f) where
  type F (WFunctor x f) a b = (x, f b)
  type Injection (WFunctor x f) d = (?gfx :: x)

  rhs_ _ _ (x, f) = WFunctor x f
  equation_ _ _ (WFunctor x f) = (x, f)

instance Functor m => Functor (WFunctor x m) where
  fmap f (WFunctor x m) = WFunctor x (fmap f m)

instance (Num x, Applicative m) => Applicative (WFunctor x m) where
  pure a = WFunctor 1 (pure a)
  WFunctor xf f <*> WFunctor xa a = WFunctor (xf * xa) (f <*> a)

instance (Num x, WAlternative x m) => Alternative (WFunctor x m) where
  empty = WFunctor 0 (wempty @x)
  WFunctor x1 a1 <|> WFunctor x2 a2 = WFunctor (x1 + x2) (wplus (x1, a1) (x2, a2))

instance (Num x, WAlternative x m) => Sized (WFunctor x m) where
  pay (WFunctor x a) = WFunctor (?gfx * x) (wincr @x a)

class Applicative m => WAlternative x m where
  wempty :: m a
  wplus :: (x, m a) -> (x, m a) -> m a
  wincr :: m a -> m a
  wincr = id

class (x ~ F e a b) => EqualsF (e :: * -> *) x a b
instance (x ~ F e a b) => EqualsF e x a b

-- Quite unsafe.
applySystem
  :: forall e d x
  .  (ForallV (EqualsF e x), Injection e d)
  => (Injection (Inject e) d => System_ (Inject e) d)
  -> Vector x -> Vector x
applySystem f xs =
  coerceToVector (let ?injection = coerceFromVector xs in f)
  where
    coerceToVector :: System_ (Inject e) d -> Vector x
    coerceFromVector :: Vector x -> System_ e d
    coerceToVector = unsafeCoerce
    coerceFromVector = unsafeCoerce

type IGF x = Inject (GFunction x)

applySystemGF
  :: forall d x
  .  Num x
  => (Injection (IGF x) d => System_ (IGF x) d)
  -> x -> Vector x -> Vector x
applySystemGF f x = let ?gfx = x in applySystem @(GFunction x) @d f

type PGF x = Pointed (GFunction x)
type IPGF x = Inject (PGF x)

applySystemPGF
  :: forall d x
  .  Num x
  => (Injection (IPGF x) d => System_ (IPGF x) d)
  -> x -> Vector [x] -> Vector [x]
applySystemPGF f x = let ?gfx = x in applySystem @(PGF x) @d f

newtype Pointed f a = Pointed [f a]

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

instance Equation f => Equation (Pointed f) where
  type F (Pointed f) a b = [F f a b]
  type Injection (Pointed f) d = Injection f d

  rhs_ a d = Pointed . fmap (rhs_ a d)
  equation_ a d (Pointed f) = fmap (equation_ a d) f

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

type System d = forall e. (Equation e, Injection e d) => System_ e d

type family Length d where
  Length '[] = 0
  Length (_ ': d) = 1 + Length d

sizedGenerator
  :: forall a d m b n
  .  (KnownNat (Length d), Assoc a d b, WAlternative Double m)
  => System d
  -> Int
  -> Maybe Double
  -> m b
sizedGenerator sys k size' = snd (gs !! k)
  where
    zipOracle =
      V.zipWith (coerceInj . zipWith (\x (_, m) -> (x, m))) oracle
      where
        coerceInj :: ([(Double, m Any)] -> [(Double, m Any)]) -> Any -> Any
        coerceInj = unsafeCoerce
    gs = lookupSystem_ @a sys_
    sys_ :: System_ (Inject (Pointed (WFunctor Double m))) d
    sys_@(System_ v_) =
      let ?gfx = x
          ?injection = System_ (zipOracle v_)
      in sys
    (x, oracle) = solveSized @a @d sys k size'

solveSized
  :: forall a d b n
  .  (KnownNat (Length d), Assoc a d b)
  => System d
  -> Int
  -> Maybe Double
  -> (Double, Vector [Double])
solveSized sys k size' =
  fmap (shape n (k + 2) . fromJust) .
  search (solveAt @d sys k) $
  checkSize size'
  where
    n = fromInteger (natVal (Proxy @(Length d)))
    i = fromInteger (natVal (Proxy @(Index a d)))

    j = i * (k + 2) + k
    j' = i * (k + 2) + k + 1

    checkSize _ (Just ys) | V.any (< 0) ys = False
    checkSize (Just size) (Just ys) = size >= ys V.! j' / ys V.! j
    checkSize Nothing (Just _) = True
    checkSize _ Nothing = False

solveAt
  :: forall d
  .  KnownNat (Length d)
  => System d
  -> Int
  -> Double
  -> Maybe (Vector Double)
solveAt sys k x = fixedPoint defSolveArgs (phi x) x0
  where
    n = fromInteger (natVal (Proxy @(Length d)))
    x0 = V.replicate (n * (k + 2)) 0
    phi :: (AD.Mode x, AD.Scalar x ~ Double) => Double -> Vector x -> Vector x
    phi x = unshape (k + 2) . applySystemPGF @d sys (AD.auto x) . shape n (k + 2)

shape :: Int -> Int -> Vector x -> Vector [x]
shape n k v = V.generate n $ \i -> V.toList (V.slice (i * k) k v)

unshape :: Int -> Vector [x] -> Vector x
unshape k = (V.fromList . take k =<<)
