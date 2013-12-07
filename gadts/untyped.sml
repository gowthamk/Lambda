structure Untyped =
struct
  type id = string

  (*
   * Consider BNF grammar of a simple toy language
   * e ::= v | (e,e) | \v.e | e e |
   * v :: = variables 
   * We translate it to following First-Order Abstract Syntax
   *)
  datatype exp =  Var of id
                | App of exp * exp
                | Abs of id * exp

  fun freeVars e = case e of
    Var id => [id]
  | App (e1,e2) => List.concat [freeVars e1,freeVars e2]
  | Abs (id,e') => List.filter (fn fv => not (fv = id)) 
    (freeVars e')
  
  (*
   * Alpha-conversion is needed to implement substitutions.
   * Implementing alpha-conversion and capture-avoiding substitution 
   * is not trivial.
   * Rfv(e) = Rfv(e')
   *)
  fun alphaConvert e = case e of
      Abs (id,e') => 
      let
        val fv_e' = freeVars e'
        fun createNewName fvs hint = 
          if List.exists (fn fv => fv = hint) fvs
          then createNewName fvs (hint^"'")
          else hint
        val id' = createNewName fv_e' (id^"'")
        val e2 = subst(Var id',id,e')
      in
        Abs(id',e2)
      end
    | _ => raise Fail "No alpha-conversion for Unbound terms"
  (* 
   * ({(id)} ∩ Rfv(e2) = {()} /\ Rfv(res) = Rfv(e2)) \/ 
   * ({(id)} ⊆ Rfv(e2)) /\ (Rfv(res) = (Rfv(e2) - {(id)}) U Rfv(e1))
   *)
  and subst(e1,id,e2) = case e2 of
      Var id' => if id = id' 
        then e1 else e2
    | Tup(e21,e22) => Tup(subst(e1,id,e21),subst(e1,id,e22))
    | App(e21,e22) => App(subst(e1,id,e21),subst(e1,id,e22))
    | Abs(id',e2') => if id' = id then e2 else
      let
        val fv_e1 = freeVars(e1)
      in
        if List.exists (fn fv => fv = id') fv_e1
        then subst(e1,id,alphaConvert e2)
        else Abs(id',subst(e1,id,e2'))
      end
end
