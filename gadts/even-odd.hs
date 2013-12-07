{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- Use DataKind promotion with type function for even-odd

module EvenOdd where

  data Parity = Even | Odd
  -- Parity is promoted to kind level Parity.
  -- Even & Odd to type level 'Even & 'Odd of kind Parity

  -- We define type-function opp to establish the relation that
  -- type 'Even is opposite of 'Odd, and vice-versa
  type family Opp (n :: Parity) :: Parity
  type instance Opp 'Even = 'Odd
  type instance Opp 'Odd = 'Even
  
 -- We tag natural number with the type of its parity
  data Nat :: Parity -> * where
     Zero :: Nat 'Even
     Succ :: Nat (Opp p) -> Nat p

  -- Now we (should) get free theorems.
  -- 1. Plus of two even numbers is even
  evenPlus :: Nat 'Even -> Nat 'Even -> Nat 'Even
  evenPlus Zero n2 = n2
  evenPlus (Succ (Succ n1)) n2 = Succ (Succ (evenPlus n1 n2))

  -- 2. Plus of even and odd numbers is odd
  evenOddPlus :: Nat 'Even -> Nat 'Odd -> Nat 'Odd
  evenOddPlus Zero n2 = n2
  evenOddPlus (Succ (Succ n1)) n2 = Succ (Succ (evenOddPlus n1 n2))

  -- 3. Sum of two odd numbers is even
  oddPlus :: Nat 'Odd -> Nat 'Odd -> Nat 'Even
  oddPlus (Succ n1) n2 = Succ (evenOddPlus n1 n2)
