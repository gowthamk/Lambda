{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module HigherOrderTyFun where
  
  data Nat = Zero | Succ Nat

  type family App (a :: * -> Nat) (b :: *) :: Nat
  type instance App a b = a b
