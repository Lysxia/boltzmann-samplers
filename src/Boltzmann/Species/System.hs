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
import Data.Coerce
import Data.Constraint.Forall
import Data.Foldable (asum)
import Data.List (inits)
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Type.Equality
import Data.Vector (Vector)
import GHC.Exts (Constraint)
import GHC.Prim (Any)
import GHC.TypeLits
import Numeric.Natural
import Unsafe.Coerce
import qualified Numeric.AD as AD
import qualified Data.Vector as V

import Data.TypeMap.Static
import Data.TypeMap.List (TypeList)
import Data.TypeMap.Vector (TypeVector)
import qualified Data.TypeMap.List as TL
import qualified Data.TypeMap.Vector as TV

import Boltzmann.Data.Common (binomial)
import Boltzmann.Options
import Boltzmann.Solver

-- | Heterogeneous map of combinatorial species with arbitrarily kinded indices.
newtype Species (f :: * -> *) (d :: [(k, *)]) = Species (TypeVector (MapSnd f d))

type family MapSnd f d where
  MapSnd f '[] = '[]
  MapSnd f ('(a, b) ': d) = '(a, f b) ': MapSnd f d

coerceMapLookup :: forall a f d. Lookup a (MapSnd f d) -> f (Lookup a d)
coerceMapLookup = unsafeCoerce

admitEqualIndices :: forall a f d r. (Index a d ~ Index a (MapSnd f d) => r) -> r
admitEqualIndices r =
  case unsafeCoerce (Refl :: 0 :~: 0) :: Index a d :~: Index a (MapSnd f d) of
    Refl -> r

-- | @a@ is in the domain of the association list @d@.
type Indexable a d = KnownNat (Index a d)

-- | Access a species by its index.
species
  :: forall a d f
  .  Indexable a d
  => Species f d
  -> f (Lookup a d)
species (Species v) = admitEqualIndices @a @f @d $
  coerceMapLookup @a @f @d (TV.index @a v)

-- | Size shifts.
type Pay f = forall b. f b -> f b

data family Alias (r :: [(*, *)]) (f :: * -> *)

-- | A map of transformations @f b -> f a@ indexed by the type @a@.
data FAlias r f where
  (:&) :: (f b -> f a) -> FAlias r f -> FAlias ('(a, b) ': r) f
  ANil :: FAlias '[] f

infixr 3 :&

class Aliasing r f where
  ($~) :: ('Just b ~ LookupM a r, Aliasing_ a r) => Alias r f -> f b -> f a

class Aliasing_ a r where
  ($~.) :: ('Just b ~ LookupM a r) => FAlias r f -> f b -> f a

type family LookupM a (r :: [(*, *)]) :: Maybe * where
  LookupM a ('(a, b) ': r) = 'Just b
  LookupM a (ab' ': r) = LookupM a r
  LookupM a '[] = 'Nothing

instance (LookupM a (c ': r) ~ LookupM a r, Aliasing_ a r) => Aliasing_ a (c ': r) where
  (_ :& alias) $~. f = alias $~. f

instance {-# OVERLAPPING #-} Aliasing_ a ('(a, b) ': r) where
  (g :& _) $~. f = g f

-- | See 'system'.
newtype System_ r f d = System_
  { runSystem_ :: Alias r f -> Pay f -> Species f d -> Species f d
  }

-- | Constructor of systems.
--
-- A (polynomial) function @F(x, y)@ describes the system @y = F(x, y)@.
--
-- - @'Alias' r f@: a family of isomorphisms between combinatorial species;
-- - @'Pay' f@: shift the sizes of a structure by 1 (multiplication by @x@);
-- - @'Species' f d@: a vector of combinatorial species (@y@).
system
  :: (Alias r f -> Pay f -> Species f d -> Species' f d d)
  -> System_ r f d
system sys = System_ (\r x -> toSpecies . sys r x)

toSpecies :: Species' f d d -> Species f d
toSpecies (Species' l) = Species (TL.toVector l)

-- | Intermediate representation of heterogeneous map of species,
-- tagged with the type of a whole, final map.
--
-- @
-- Species' f \'['(a1, b1), '(a2, b2), '(a3, b3)]
--   = [F f b1, F f b2, F f b3]
-- @
newtype Species' f d d0 = Species' (TypeList (MapSnd f d))

-- | Empty vector.
none :: Species' f '[] d0
none = Species' TL.empty

-- | Heterogeneous cons.
(/\) :: f b -> Species' f d d0 -> Species' f ('(a, b) ': d) d0
(/\) eq (Species' sys) = Species' (eq `TL.cons` sys)

infixr 3 /\

-- |
--
-- @
-- 'duplicate' n f = f '<|>' ... '<|>' f  -- n times
-- @
duplicate :: Alternative f => Natural -> f a -> f a
duplicate 0 _ = empty
duplicate n f = f <|> duplicate (n - 1) f

type Assoc a d b = (Indexable a d, Lookup a d ~ b)

newtype GFunction x b = GFunction x

instance Functor (GFunction x) where
  fmap _ (GFunction x) = GFunction x

instance Num x => Applicative (GFunction x) where
  pure _ = GFunction 1
  GFunction xf <*> GFunction xa = GFunction (xf * xa)

instance Num x => Alternative (GFunction x) where
  empty = GFunction 0
  GFunction x1 <|> GFunction x2 = GFunction $ x1 + x2

data instance Alias r (GFunction x) = GFAlias

instance Aliasing r (GFunction x) where
  _ $~ GFunction x = GFunction x

xGFunction :: Num x => x -> Pay (GFunction x)
xGFunction x1 (GFunction x0) = GFunction (x1 * x0)

data WFunctor x f a = WFunctor !x (f a)

instance Functor m => Functor (WFunctor x m) where
  fmap f (WFunctor x m) = WFunctor x (fmap f m)

instance (Num x, Applicative m) => Applicative (WFunctor x m) where
  pure a = WFunctor 1 (pure a)
  WFunctor xf f <*> WFunctor xa a = WFunctor (xf * xa) (f <*> a)

instance (Num x, WAlternative x m) => Alternative (WFunctor x m) where
  empty = WFunctor 0 (wempty @x)
  WFunctor x1 a1 <|> WFunctor x2 a2 = WFunctor (x1 + x2) (wplus (x1, a1) (x2, a2))

newtype instance Alias r (WFunctor x f) = WFAlias (FAlias r f)

instance Aliasing r (WFunctor x f) where
  WFAlias alias $~ WFunctor x f = WFunctor x (alias $~. f)

xWFunctor :: Num x => x -> Pay (WFunctor x f)
xWFunctor x1 (WFunctor x0 f) = WFunctor (x1 * x0) f

-- | Alternatives with weighted choice.
class Applicative m => WAlternative x m where
  wempty :: m a
  wplus :: (x, m a) -> (x, m a) -> m a

  -- | Increment size.
  wincr :: m a -> m a
  wincr = id

class Coercible x (f b) => CoerciblesF x (f :: * -> *) b
instance Coercible x (f b) => CoerciblesF x f b

-- Quite unsafe.
applySystem
  :: ForallV (CoerciblesF x f)
  => System_ r f d
  -> Alias r f -> Pay f -> Vector x -> Vector x
applySystem (System_ sys) alias x = unsafeCoerce (sys alias x)

applySystemGF
  :: Num x
  => System_ r (GFunction x) d
  -> x -> Vector x -> Vector x
applySystemGF f x = applySystem f GFAlias (xGFunction x)

applySystemPGF
  :: Num x
  => System_ r (Pointed (GFunction x)) d
  -> x -> Vector [x] -> Vector [x]
applySystemPGF f x = applySystem f (PAlias GFAlias) (xPointed (xGFunction x))

newtype Pointed f a = Pointed [f a]

takePointed :: Int -> Pointed f a -> [f a]
takePointed n (Pointed f) = take n f

instance Functor f => Functor (Pointed f) where
  fmap f (Pointed v) = Pointed ((fmap . fmap) f v)

instance Alternative f => Applicative (Pointed f) where
  pure a = Pointed (pure a : repeat empty)
  Pointed fs <*> Pointed xs = Pointed (convolute fs xs)
    where
      convolute fs xs = zipWith sumOfProducts [0 ..] ((tail . inits) xs)
      sumOfProducts k x = asum (zipWith3 (times k) [0 ..] fs (reverse x))
      times k k1 f x = duplicate (binomial k k1) f <*> x

instance Alternative f => Alternative (Pointed f) where
  empty = Pointed (repeat empty)
  -- Pointed [] <|> ys = ys
  -- xs <|> Pointed [] = xs
  Pointed xs <|> Pointed ys = Pointed (zipWith' (<|>) xs ys)

zipWith' f ~(x : xs) ~(y : ys) = f x y : zipWith' f xs ys

newtype instance Alias r (Pointed f) = PAlias (Alias r f)

instance Aliasing r f => Aliasing r (Pointed f) where
  PAlias r $~ Pointed v = Pointed (fmap (r $~) v)

xPointed :: Alternative f => Pay f -> Pay (Pointed f)
xPointed x (Pointed fs) = Pointed (scanl1 (<|>) (fmap x fs))

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

data instance Alias r Coefficients = CAlias

instance Aliasing r Coefficients where
  _ $~ Coefficients cs = Coefficients cs

xCoefficients :: Pay Coefficients
xCoefficients (Coefficients xs) = Coefficients (0 : xs)

coeffSystem
  :: forall a r d b
  .  (Assoc a d b)
  => System_ r Coefficients d -> Coefficients b
coeffSystem (System_ sys) = species @a self
  where
    self = sys CAlias xCoefficients self

pCoeffSystem
  :: forall a r d b
  .  (Assoc a d b)
  => System_ r (Pointed Coefficients) d -> Pointed Coefficients b
pCoeffSystem (System_ sys) = species @a self
  where
    self = sys (PAlias CAlias) (xPointed xCoefficients) self
    -- unsafeCoerce (V.replicate 30 (Pointed (repeat (empty :: Coefficients b))))) -- self

-- | A combinatorial system describing a family of recursive structures.
--
-- Constructed using 'system'.
type System r d = forall f. (Alternative f, Aliasing r f) => System_ r f d

type family Length d where
  Length '[] = 0
  Length (_ ': d) = 1 + Length d

-- | Create a generator for a given combinatorial species.
sizedGenerator
  :: forall a r (d :: [(k, *)]) m b
  .  (KnownNat (Length d), Assoc a d b, WAlternative Double m)
  => FAlias r m
  -> System r d
  -> Options
  -> m b
sizedGenerator alias sys opts = g
  where
    zipOracle = coerceEndo (V.zipWith (zipWith (\x (_, m) -> (x, m))) oracle)
      where
        coerceEndo
          :: (Vector [(Double, m Any)] -> Vector [(Double, m Any)])
          -> Species (Pointed (WFunctor Double m)) d
          -> Species (Pointed (WFunctor Double m)) d
        coerceEndo = unsafeCoerce
    WFunctor _ g = gs !! points opts
    Pointed gs = species @a s
    s = runSystem_ sys (PAlias (WFAlias alias)) (xPointed (xWFunctor x0)) (zipOracle s)
    (x0, oracle) = solveSized @a sys opts

solveSized
  :: forall a r d b n
  .  (KnownNat (Length d), Assoc a d b)
  => System r d
  -> Options
  -> (Double, Vector [Double])
solveSized sys opts =
  fmap (shape n (k + 2) . fromJust) .
  search (solveAt sys k) $
  checkSize (averageSize opts)
  where
    k = points opts
    n = fromInteger (natVal (Proxy @(Length d)))
    i = fromInteger (natVal (Proxy @(Index a d)))

    j = i * (k + 2) + k
    j' = i * (k + 2) + k + 1

    checkSize _ (Just ys) | V.any (\x -> x < (- (sum . fmap abs) ys / 10 ** 6)) ys = False
    checkSize (Just size) (Just ys) = size >= ys V.! j' / ys V.! j
    checkSize Nothing (Just _) = True
    checkSize _ Nothing = False

solveAt
  :: forall r d
  .  KnownNat (Length d)
  => System r d
  -> Int
  -> Double
  -> Maybe (Vector Double)
solveAt sys k x = fixedPoint defSolveArgs (phi x) x0
  where
    n = fromInteger (natVal (Proxy @(Length d)))
    x0 = V.replicate (n * (k + 2)) 0
    phi :: (AD.Mode x, AD.Scalar x ~ Double) => Double -> Vector x -> Vector x
    phi x = unshape (k + 2) . applySystemPGF sys (AD.auto x) . shape n (k + 2)

shape :: Int -> Int -> Vector x -> Vector [x]
shape n k v = V.generate n $ \i -> V.toList (V.slice (i * k) k v)

unshape :: Int -> Vector [x] -> Vector x
unshape k = (V.fromList . take k =<<)
