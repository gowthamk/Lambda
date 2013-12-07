{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}

-- Demonstrate data type promotion to datakind

module LengthIndexedList where

  data Nat = Zero | Succ Nat
  -- Nat is promoted to kind level Nat.
  -- Zero to type level 'Zero of kind Nat, and
  -- Succ of type level 'Succ of kind Nat -> Nat

  --  We define Plus as an indexed type family,
  --  which is equivalent to a type-level function
  type family Plus (a :: Nat) (b :: Nat) ::  Nat
  type instance Plus 'Zero b = b
  type instance Plus ('Succ a) b = 'Succ (Plus a b)

  data Vec :: * -> Nat -> * where
    VNil :: Vec a 'Zero
    VCons :: a -> Vec a n -> Vec a ('Succ n)

  append :: Vec a n1 -> Vec a n2 -> Vec a (Plus n1 n2)
  append VNil v2 = v2
  append (VCons x xs) v2 = VCons x (append xs v2)

  -- First example ATTAPL chapter on Dependent Types:
  -- How do we create vectors of length n?
  -- We cannot write a function with following type:
  -- init :: n -> a -> Vec a n
  -- Why? Although we know that n ranges over types of kind Nat,
  -- the two types that inhabit Nat (viz., Zero and Succ (a :: Nat)) do not
  -- have value constructors. So, they are uninhabited.
  -- Apparently, there is a solution involving proxy type - 
  -- http://stackoverflow.com/questions/10286680/how-to-create-typeclass-instances-of-a-promoted-type

  -- Heterogeneous list example
  data List a = Nil | Cons a (List a) 
  data HList :: (List *) -> * where
    HNil :: HList 'Nil
    HCons :: a -> HList as -> HList ('Cons a as)

  --data HList :: [*] -> * where
  --  HNil :: HList '[]
  --  HCons :: a -> HList as -> HList (a:as)

  l1 :: HList ('Cons String ('Cons Bool 'Nil))
  l1  = HCons "Hello" (HCons True HNil)

  -- Haskell kinds now include the kind of types of values, written
  -- ⋆, other base kinds formed from promoted datatypes, arrow kinds,
  -- and polymorphic kinds. This means that we can only promote data
  -- constructors with types analogous to one of these kinds.

  -- Kind polymorphism
  -- Due to kind polymorphism, type constructors can now take, not
  -- just types (i.e.,a::*), but other type constructors (a::AnyK) as
  -- arguments. That is, type constructos can be indexed by other type
  -- constructors. An example is present in higher-order-tyfun.hs.
  -- In the below example, a is a type constructor of any kind.
  -- Therefore, tycon Proxy :: forall X. X -> *
  data Proxy a = Froxy
  l2 :: Proxy Maybe
  l2 = Froxy
  -- If we use it in type class ...
  class Typeable a where
    typeOf :: Proxy a -> String
  instance Typeable Maybe where
    typeOf  _ = "Maybe"
