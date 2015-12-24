{-# LANGUAGE InstanceSigs #-}  

{-- Wadler's example 1 --}
{-- Demonstrates the plumbing aspect of monad --}
module Wadler1 where

  import Control.Monad
  import Control.Applicative

  data Term = Con Int | Div Term Term deriving Show

  {-- Result of evaluating a Term (or, anything in general) --}
  data Res a = Raise Exception | Return a deriving Show
  type Exception = String

  {-- Non-monadic evaluator --}
  eval :: Term -> Res Int
  eval (Con x) = Return x
  eval (Div t1 t2) = case eval t1 of
      Raise e -> Raise e
      Return v1 -> case eval t2 of
        Raise e -> Raise e
        Return v2 -> if v2 == 0 
          then Raise "Divide by zero"
          else Return (v1 `div` v2)

  {- Monadic evaluator should sweep the ugly plumbing of the above
   - code under a monad rug. We must first define such monad. -}
  instance Monad Res where
    return :: a -> Res a
    return a = Return a

    (>>=) :: Res a -> (a -> Res b) -> Res b
    (>>=) ra f = case ra of
      Raise exn {- :: Res a -} -> Raise exn {- :: Res b -}
      Return va -> f va

  {- Notice how an ordinary algebraic datatype has now become a monad. -}
  {- We now define a monadic evaluator -}
  evalM :: Term -> Res Int
  evalM (Con x) = return x
  evalM (Div t1 t2) = do
    v1 <- evalM t1
    v2 <- evalM t2
    return (v1 `div` v2)
