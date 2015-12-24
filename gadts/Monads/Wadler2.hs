{-# LANGUAGE InstanceSigs, TypeSynonymInstances #-}
{- InstanceSigs, so that we can declare types for instance functions.
 - TypeSynonymInstances, so that we can delcare type synonyms as
 - instances. -}

{-- Wadler's example 2 --}
module Wadler2 where

  import Control.Monad
  import Control.Applicative

  data Term = Con Int | Div Term Term deriving Show

  {- Consider an evaluation that counts the number of division
   - operations. Int maintains an integer state. -}
  type State = Int
  eval :: Term -> State -> (Int,State)
  eval (Con x) n = (x,n)
  eval (Div t1 t2) n = 
   let (v1,n') = eval t1 n
       (v2,n'') = eval t2 n'
   in (v1 `div` v2, n'' + 1)

  {- Let us now define a type Res such that eval :: Term -> Res Int -}
  data Res a = Res {runState :: State -> (a,State)}
  {- Note: Due to Haskell's treatment of record fields:
       *Wadler2 > :t runState
       runState :: Res a -> State -> (a, State)
   -}
  {- Passing the state around is a bit of a problem. Why not make Res
   - a monad? -}
  instance Monad Res where
    {- return acts as an identity function over states. It produces an
     - effect-free computation that returns the given value. -}
    return :: a -> Res a
    return a = Res {runState = \initState -> (a,initState)}
    {- (>>=) plumbs the state between first computation and the
     - second. -}
    (>>=) :: Res a -> (a -> Res b) -> Res b
    (>>=) ra f = Res 
      {runState = \initState ->
                    let (a,st1) = runState ra initState
                        rb = f a
                    in runState rb st1}

  {- tick is an effect-only computation. -}
  tick :: Res ()
  tick = Res {runState = \initState -> ((), initState + 1)}

  {- Now, lets define the monadic evaluator -}
  evalM :: Term -> Res Int
  evalM (Con x) = return x
  evalM (Div t1 t2) = do
    v1 <- evalM t1
    v2 <- evalM t2
    tick
    return (v1 `div` v2)
  {-  Notice that, while Wadler1.eval and Wadler2.eval are very
   -  diffent, Wadler1.evalM and Wadler2.evalM are almost the same!
   -  This is the power of monad abstraction: it abstracts away the
   -  plumbing logic to expose only the relevant computation. -}
