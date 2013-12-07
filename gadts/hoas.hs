{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HOAS where

  data Exp :: * -> * where
    Lit :: a -> Exp a
    Pair :: Exp a -> Exp b -> Exp (a,b)
    -- Recall that our primary goal is to use meta language (here,
    -- ocaml) functions to encode object language binders *)
    Abs :: (Exp a -> Exp b) -> Exp (a -> b)
    App :: Exp (a -> b) -> Exp a -> Exp b

  eval :: Exp s -> s
  eval (Lit x) = x
  eval (Pair e1 e2) = (eval e1, eval e2)
  eval (Abs f) = (\x -> eval (f (Lit x)))
  eval (App e1 e2) = (eval e1) (eval e2)

  -- The following function prompty fails typecheck.
  -- This is the upside (or downside, depending on your prespective)
  -- of existential type. matching first argument e against pattern
  -- App e1 e2 introduces an existential type a such that 
  -- e1 :: a -> s and e2 :: a. Since a only exists after we unfold
  -- e, no type can ever be unified with a.
  subst (App e1 e2) e' = App e' e2
  subst e e' = e

  newe = subst (App (Abs (\x -> x))) (Pair (Lit "x1") (Lit "x2"))
