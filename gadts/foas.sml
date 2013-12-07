structure ToyLang =
struct
  type id = string

  (*
   * Consider BNF grammar of a simple toy language
   * e ::= v | (e,e) | \v.e | e e |
   * v :: = variables 
   * We translate it to following First-Order Abstract Syntax
   *)
  datatype exp =  Var of id
                | Tup of exp * exp
                | App of exp * exp
                | Abs of id * exp

  fun freeVars e = case e of
    Var id => [id]
  | Tup (e1,e2) => List.concat [freeVars e1,freeVars e2]
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
      in
        Abs(id',subst(Var id',id,e'))
      end
    | _ => raise Fail "No alpha-conversion for Unbound terms"
  (* 
   * Rfv(res) => (Rfv(e2) - {(id)}) U Rfv(e1)    
   * Rfv(e2) => Rfv(res) U {(id)}
   * OR
   * Rfv(res) - Rfv(e1) = Rfv(e2) - {(id)};
   * OR
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

  (*
   * Compilers most often perform syntactic transformations like
   * A-Normalization on source code. Such transformations require
   * substituting one term for another. Certain substitutions are
   * patently incorrect. For eg, substituting a lambda term (i.e., a
   * term that is syntactically a lambda) with a tuple term is
   * obviously wrong.
   * [(x1,x2)/\x.x] ((\x.x) y) -~-> (x1,x2) y
   * With first-order abstract syntax, we cannot prevent such
   * substitutions. The following code happily typechecks.
   *)
  val newe = (fn (e:exp, e':exp) => case e of 
        App (e1,e2) => App (e',e2) 
      | _ => e) 
    (App (Abs ("x",Var "x")), Tup(Var "x1",Var "x2"));
end
