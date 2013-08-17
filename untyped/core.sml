structure Core = 
struct
  open Ast

  exception CantTakeStep

  fun isVal t = case t of
      Var _ => true
    | Abs(_,_) => true
    | _ => false

  fun singleStepEval t = 
    let val _ = Control.debugPrint ("\t- " ^ layout t ^ "\n")
    in
      case t of
        App ((Abs(id,e)),e2) => 
          if (isVal e2)
          (* computation - beta reduce *)
          then subst (e2,id,e)
          (* congruence-2 *)
          else App(Abs(id,e),singleStepEval e2)
      | App (e1,e2) => (* congruence-1 *)
          App (singleStepEval e1,e2)
      | _ => raise CantTakeStep
    end
    

  fun eval t = 
    let val t' = singleStepEval t
    in
      eval t'
    end handle CantTakeStep => t
end
