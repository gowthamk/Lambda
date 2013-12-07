-- The main difference between conventional notion of dependent types
-- and Haskell perception is that, while conventionally dependent
-- types are defined as types indexed by terms, Haskell promotes terms
-- to type level and provides type level functions that, together,
-- lead to dependent typing.
-- In Haskell, we can only promote value constructors to types. Values 
-- cannot be promoted.

-- Consider the definition of length and elts indexed list in
-- Haskelized DML:

data Set :: * -> * where
  Empty :: Set a
  Union :: Set a -> Set a -> Set a

data List where
  Nil :: List a s Empty
  Cons :: Element a s -> List a s s1 -> List a s (Union s s1)

-- We tag List type with 1. element type, 2. Element set abstraction 
-- 3. List set abstraction.
-- type of map will be:
-- map :: (f :: Element a s1 -> Element b s2) -> (l1 :: List a (s s1)) ->
--        (l2 :: List b (s s2))
data Rmem :: Relation -> Relation where
  Base :: Rmem a
  Ind :: r -> Rmem r -> Rmem r

data Rob :: Relation -> Relation where
  Base :: Rob a
  Ind :: r -> Rmem r -> Rob r


{l1 : list (Rmem r) (a r)} -> {f : a r -> b s} ->{l2 : list (Rmem s) (b s)}

map : {l1 : 'a list} -> {f : {x:'a} -> {y:'b| s(y) = r(x)} } -> 
  {l2 : 'b list | (Rob* r)(l1) = (Rob* s)(l2)}

fold : {l1 : 'a list} -> {f : {x : 'a} -> {acc : 'b} -> 
    {z : 'b | R1(z) = (r(x) X R2(acc)) U R1(acc)}} ->
    {l2 : 'b | R2 (l2) = (Rob r)* (l1) /\ R1(l2) = (Rmem r)(l1)}

map2 : l1 -> l2 -> {l | (Rob Rfirst)*(l) = Rob*(l1)}
map2 l1 l2 = case (l1,l2) of
    ([],[]) => []
  | (x1::xs1, x2::xs2) => (x1,x2)::(map2 xs1 xs2)
  | _ => raise (Fail "map2")

forall y, exists x1,x2,  RobsRf(l,y1,y2) <=> Robs(l,x1,x2,x3,x4) /\ Rfirst (x1,x2,y1) /\ Rfirst (x3,x4,)
forall z, RobsRf(l,y) <=> Robs(l1,

Rfirst (x1,x2) = {(x1)}
Rsecond (x1,x2) = {(x2)}

Rnotk (k,x) = {(x)} - {(k)}

remove : {l1 : string list} -> {k : string} -> {l2 : string list |
          (Rob* _)(l2) = (Rob* (Rnotk k))(l1)}


List.filter : {l : 'a list} -> {f : {x:'a} -> {y: bool | } }
     -> {l2 : 'a list | (R}

Relation is a kind

r : 'a -> Relation

Rmem : Relation -> 'a -> Relation



----------------------------------------------
-- Thumb rule : for the case of OR - encode true 
-- as empty set and use intersection.
-- Or, encode true as non-empty set and use union.
----------------------------------------------
Rmemx r (x::xs) = r(x) /\ (Rmem r)(xs)

Listexists : l -> (f : {x:'a} -> {v : bool | R1(v) = R2(x)} ) ->
      {z : bool | R1(z) = (Rmemx R2)(l)}

Eg: R1(true) = {} R1(false) = {k}, and
    R2(x) = {k} - {x}

----------------------------------------------
-- Lets refine further
-- Define
  R1 R (true) = R | R1 R (false) = {}

Listexists : l -> (f : {x:'a} -> {v : bool | R1 R (v) = R2(x)} ) 
    -> {z : bool | R1 R (z) = (Rmem R2)(l)}
Listexists [] f => false
Listexists (x::xs) f => case Listexists xs f of
    true => true
  | false => f(x)

-- Then define
R2(x) = {k} - ({k} - {x})
----------------------------------------------
----------------------------------------------
-- 1. We aim for sweet-spot between inlining the higher-order function
-- and generic (but weak) typed higher-order function
-- 2. Treat these two things separately:
-- 2.1 Ease of using (instantiating) higher-order type, and
-- 2.2 Ease of type-checking the higher-order type
-- 3. Relations map patterns to relations
-- Rmem :: ('a -> 'b) relation) -> {'a list -> 'b} relation
--
-- TODO : Apply for PLMW funding :-)
--
-- first order list exists:
relation R2 r1 r2 = r1 - (r1 - r2);


listexists : l -> {k : string } -> {z : bool | R1 {k} (z) = Rmem (R2 {k}) (l)}
listexists [] k => false
listexists (x::xs) k = case listexists xs k of 
    true => true
  | false => (x=k)

