Lambda
======

Untyped lambda calculus extended with bools and Peano arithmetic.

Commit Log:
----------

* 17/08/2013 - First commit
* 19/08/2013 - Fixed lexer error. Keywords come first because:
  "The lexer resolves conflicts among rules by choosing the rule
  with the longest match, and in the case two rules match the same
  string, choosing the rule listed first in the specification."
* 21/08/2013 - Adding explicit substitutions for effeciency and as
  a prelude to adding named lambdas.

Explicit substutions
--------------------

Consider the following lambda term -

(\x.x x x)((\a.a a a a) t)

where t is a large lambda term in head normal form. Beta redex 
rule:

      e2 ==> v2
.......................
(\x.e1) e2 ==> [v2/x]e1

dictates us do the textual substitution of t for a and resultant 
term for x, which leads to size explosion. Also, substitution in
beta rule is a meta-level operation, therefore, does not capture
the intracacies like capture-avoidance. In interests of
effeciency and rigour, we represent substitutions explcitly with 
help of dynamic environment (Δ).
{Δ ∈ variables → values} is a map from variables to values that 
contains bindings under which current expression has to be evalu
ated. Notice that co-domain is set of values, which is in agreem
ent with our CBV semantics. The evaluation relation is now a ter
nary relation and our natural deduction rules make the following 
judgement:  

           Δ ⊢ e ==> e'

New bindings in Δ are created by new β redex:

     Δ ⊢ e2 ==> v2   Δ[x↦v2] ⊢ e1 ==> e1'
    ...................................... [E-β1]
               Δ ⊢ (\x.e1) e2 ==> e1'

A Substituion is carried our when a variable is evaluated:  

           Δ(x) = v
         ............. [E-VAR]
          Δ ⊢ x ==> v
	  
If we allow only closed programs, then failed lookup should be
an error in the evaluation. Otherwise, we should use different
notation to represent variables which are values. We represent 
them as symbols:

        datatype exp = ... | Symbol of id | ...

When evalutating a lambda abstraction, current environment cont
ains bindings for free variables. It is important to capture the 
lambda term in context of current environment. Why? Consider the 
following redex:

	Δ ⊢ ((\y.\x.y x)((\x.\y.y x) z)) x'

As per [E-β1] rule, the term (\y.y x) is evaluated in the extended 
environment Δ[x↦z]. Now, if we avoid capturing the current environ
ment and instead only return the lambda term, the binding of [x↦z] 
is lost forever as topmost redex is only evaluated under Δ. Another
manifestation of this problem is the case when Δ already contains a
binding for x (which is different from the current x), in which case
substitute inappropriate value for x. Therefore, we evaluate lambda 
abstractions to a term containing the abstraction, along with the 
current environment, which we call as a closure:  
         Δ ⊢ (\x.e) ==> (\x.e,Δ)
	 
We now modify [E-β1] rule to account for closures:  

     Δ ⊢ e2 ==> v2   Δ'[x↦v2] ⊢ e1 ==> e1'
    ...................................... [E-β2]
               Δ ⊢ (\x.e1,Δ') e2 ==> e1'

Before we move onto give full small-step semantics for explicit sub
stitution calculus, let us reason why substution through [E-VAR]
rule is safe by considering the two corner cases that we encountered
in implicit substiutions:
1. Nested lambda term having same bound var as enclosing lambda term:
   In this case, the algebraic laws of maps dictate that Δ(bound var)
   evaluates to the latest binding, thereby preventing substitution 
   for overridden bound var.
2. Free vars of substitution are captured by bound var of enclosing 
   lambda term: There is no concept of capture in our substitutions as
   we never substitute under lambda abstractions.
So far so good. Now let us add the congruence rules for β redex:

     Δ ⊢ e1 ==> e1'
  .................... [E-APP1]
  Δ ⊢ e1 e2 ==> e1' e2

     Δ ⊢ e2 ==> e2'
  .................... [E-APP2]
  Δ ⊢ v1 e2 ==> v1 e2'
  
As an example, consider the following term:  

   (\t.\x.\y.t x y) (\x.\y.x y) a b

After two reductions, we find ourselves applying [E-β2] rule as:  

      	  [] ⊢ (var) b ==> (sym) b   
      [t↦(\x.\y.x y, []); x↦a; y↦b] ⊢ (t x y) ==> e1'
 ..........................................................
    [] ⊢ (\y.t x y,[t↦(\x.\y.x y, []); x↦a]) b ==> e1'

Next, by applying [E-APP1] twice and [E-VAR] once, we derive e1' to be
	 
	 (\x.\y.x y, []) x y

Further reductions happen under empty env, so we have effectively lost
bindings for x and y in topmost redex. Our approach to treat substitu
tion as a single step (via [E-VAR]) of evaluation coupled with our
insistence on making evaluation only a ternary relation has landed us 
in trouble here. Notice that we wouldn't have had encountered this 
problem if we had not endeavoured to give small-step semantics for 
explicit substitution calculus, and instead went with big-step evaluation.
Indeed, when implementing an interpreter we do not evaluate in small
steps, but reduce a term till it becomes a value. So, for all practical 
purposes, we have done enough here. However, more work needs to be done
to give precise small-step semantics.

One way to get around this problem is by allowing closures for terms.
