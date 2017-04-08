{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import Data.Proxy
import Data.Vector (Vector)
import GHC.Exts (Constraint)
import GHC.Prim (Any)
import GHC.TypeLits
import Unsafe.Coerce
import qualified Data.Vector as V

newtype System_ (e :: k -> * -> *) (d :: [(k, *)]) = System_ (Vector Any)

type family Index a d where
  Index a ('(a, _) ': _) = 0
  Index a (_ ': d) = 1 + Index a d

type family Lookup a (d :: [(k, *)]) where
  Lookup a ('(a, b) ': _) = b
  Lookup a (_ ': d) = Lookup a d

system :: PreSystem e d -> System_ e d
system (PreSystem a) = System_ (V.fromList a)

-- |
--
-- @
-- PreSystem e \'['(a1, b1), '(a2, b2), '(a3, b3)]
--   = [F e b1, F e b2, F e b3]
-- @
newtype PreSystem e d = PreSystem [Any]

emptySys :: PreSystem e '[]
emptySys = PreSystem []

(/\) :: e a b -> PreSystem e d -> PreSystem e ('(a, b) ': d)
(/\) eq (PreSystem sys) = PreSystem (unsafeCoerce eq : sys)

infixr 3 /\

class Alternative f => Species f where
  pay :: f a -> f a

  duplicate :: Int -> f a -> f a
  duplicate 0 _ = empty
  duplicate n f = f <|> duplicate (n - 1) f

class Species (F e) => Equation (e :: k -> * -> *) where
  type F e :: * -> *
  type Injection e (d :: [(k, *)]) :: Constraint

  lookupSys
    :: forall a d n proxy
    .  (n ~ Index a d, KnownNat n, Injection e d)
    => proxy a
    -> System_ e d
    -> F e (Lookup a d)
  lookupSys _ (System_ v) =
    (rhs :: e a (Lookup a d) -> F e (Lookup a d))
      (unsafeCoerce (v V.! fromInteger (natVal @n Proxy)))

  rhs :: e a b -> F e b
  equation :: Injection e d => F e b -> e a b

newtype Number x a b = Number x

instance Num x => Equation (Number x) where
  type F (Number x) = Ring x
  type Injection (Number x) d = (?injection :: (x, Vector x))

  lookupSys
    :: forall a d n proxy
    .  (n ~ Index a d, KnownNat n, Injection (Number x) d)
    => proxy a
    -> System_ (Number x) d
    -> Ring x (Lookup a d)
  lookupSys _ _ =
    (Ring . const)
      (snd ?injection V.! fromInteger (natVal @n Proxy))

  rhs (Number x) = Ring (const x)
  equation (Ring f) = (Number . f . fst) ?injection

newtype Ring x a = Ring (x -> x)

instance Functor (Ring x) where
  fmap f (Ring x) = Ring x

instance Num x => Applicative (Ring x) where
  pure _ = Ring (const 1)
  Ring x1 <*> Ring x2 = Ring (liftA2 (*) x1 x2)

instance Num x => Alternative (Ring x) where
  empty = Ring (const 0)
  Ring x1 <|> Ring x2 = Ring (liftA2 (+) x1 x2)

instance Num x => Species (Ring x) where
  pay (Ring f) = Ring (\x -> x * f x)

-- Quite unsafe.
applySystem
  :: forall x d
  .  Num x
  => (Injection (Number x) d => System_ (Number x) d)
  -> x -> Vector x -> Vector x
applySystem f x xs =
  unsafeCoerce (let ?injection = (x, xs) in f :: System_ (Number x) d)
