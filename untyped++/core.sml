structure Core = 
struct
  open Ast

  exception CantTakeStep

  fun isVal t = case t of
      True => true
    | False => true
    | Zero => true
    | Succ t' => isVal t'
    | Symbol _ => true
    | Closure (_,_,_) => true
    | _ => false

  fun singleStepEval (env,t) = 
    let val _ = Control.debugPrint ("\t- " ^ layout t ^ "\n")
    in
      case t of
        Succ e => Succ (singleStepEval e)
      | Pred (Zero) => Zero
      | Pred (Succ e) => e
      | Pred e => Pred (singleStepEval e)
      | IsZero (Zero) => True
      | IsZero (Succ _) => False
      | IsZero e => IsZero (singleStepEval e)
      | Val id => Env.lookup id handle NotFound => Symbol id
      | Abs (id,exp) => Closure(id,exp,env)
      | App ((Closure(id,e,clenv)),e2) => 
          if (isVal e2)
          (* computation - beta reduce *)
          then singleStepEval (Env.bind clenv id e2) e
          (* congruence-2 *)
          else App(Abs(id,e),singleStepEval env e2)
      | App (e1,e2) => (* congruence-1 *)
          App (singleStepEval e1,e2)
      | Ite (True,e2,e3) => e2
      | Ite (False,e2,e3) => e3
      | Ite (e1,e2,e3) => Ite(singleStepEval e1,e2,e3)
      | _ => raise CantTakeStep
    end
    

  fun eval t =
    let val t' = singleStepEval t
    in
      eval t'
    end handle CantTakeStep => expToVal

  fun evaluateDecl env (Val(id,exp)) : Env.t =>
    let val rhsVal = eval env exp
    in
      Env.bind id rhsVal
    end

  fun evaluateTopDecl env td = evaluateDecl env td

  fun evaluateProgram (Prog(tds,exp)) = 
    let val env = List.foldl (fn (td,env) => evaluateTopDecl td)
          Env.emptyEnv
          tds
    in
      eval env exp
    end
end
