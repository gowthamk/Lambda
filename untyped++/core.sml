structure Core = 
struct
  open Ast

  exception CantTakeStep

  fun isVal t = case t of
      True => true
    | False => true
    | Zero => true
    | Succ t' => isVal t'
    | Var _ => true
    | Abs(_,_) => true
    | _ => false

  fun singleStepEval t = 
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
      | App ((Abs(id,e)),e2) => 
          if (isVal e2)
          (* computation - beta reduce *)
          then subst (e2,id,e)
          (* congruence-2 *)
          else App(Abs(id,e),singleStepEval e2)
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
    end handle CantTakeStep => t
end
