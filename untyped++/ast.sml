structure Ast =
struct
  type id = string

  datatype exp =  True
                | False
                | Zero
                | Succ of exp
                | Pred of exp
                | IsZero of exp
                | Var of id
                | App of exp*exp
                | Abs of id*exp
                | Ite of exp*exp*exp

  type top_level = exp

  fun layout ast = case ast of
      True => "true"
    | False => "false"
    | Zero => "0"
    | Succ e => "Succ ("^(layout e)^")"
    | Pred e => "Pred"^(layout e)^")"
    | IsZero e => "iszero ("^(layout e)^")"
    | Var id => id
    | App(e1,e2) => 
      let val s1 = layout e1
          val s2 = layout e2
      in
        "("^s1^" "^s2^")"
      end
    | Abs(id,e) =>
      let val s = layout e
      in
        "(\\"^id^"."^s^")"
      end
    | Ite(e1,e2,e3) => "if "^(layout e1)^" then "^(layout e2)^
        " else "^(layout e3)
  (*
    freeVars returns list of free variables in given term
  *)
  fun freeVars e = case e of
      Succ e' => freeVars e'
    | Pred e' => freeVars e'
    | IsZero e' => freeVars e'
    | Var id => [id]
    | App (e1,e2) => List.concat [freeVars e1,freeVars e2]
    | Abs (id,e') => List.filter (fn fv => not (fv = id)) (freeVars e')
    | Ite (e1,e2,e3) => List.concat [freeVars e1, freeVars e2, freeVars e3]
    | _ => []

  (* 
    Alpha renaming - Renames bound var id as id' such that
    1. There is no free var in e' named id'
    2. Any sub-term in e' with bound var name as id' is recursively 
       alpha-converted. This is achieved by calling capture-avoiding
       subst on e'.
  *)
  fun alphaConvert e = case e of
      Abs (id,e') => 
      let
        val fv_e' = freeVars e'
        (* Prove that this terminates *)
        fun createNewName fvs hint = 
          if List.exists (fn fv => fv = hint) fvs
          then createNewName fvs (hint^"'")
          else hint
        val id' = createNewName fv_e' (id^"'")
      in
        Abs(id',subst(Var id',id,e'))
      end
    | _ => raise Fail "No alpha-conversion for Unbound terms"

  (* Capture-avoiding substitution *)
  and subst(e1,id,e2) = case e2 of
      Succ e2' => Succ (subst(e1,id,e2'))
    | Pred e2' => Pred (subst(e1,id,e2'))
    | IsZero e2' => IsZero (subst(e1,id,e2'))
    | Var id' => if id = id' 
        then e1 else e2
    | App(e21,e22) => App(subst(e1,id,e21),subst(e1,id,e22))
    | Abs(id',e2') => if id' = id then e2 else
      let
        val fv_e1 = freeVars(e1)
      in
        if List.exists (fn fv => fv = id') fv_e1
        then subst(e1,id,alphaConvert e2)
        else Abs(id',subst(e1,id,e2'))
      end
    | Ite(e21,e22,e23) => Ite(subst(e1,id,e21),subst(e1,id,e22),
        subst(e1,id,e23))
    | _ => e2
end
