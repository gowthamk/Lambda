When you have a series of functions generating values of the same
datatype, such that the result of one computation is consumed by a
subsequent computation, then the Monad class provides an easy way to
plumb them. The plumbing logic can be abstracted out into the bind
function of the monad, and the `do` syntax makes it easy to simply
declare the sequence of computations. 

Most often, the "sequence of computations" view is very useful to
describe effectful computations on some shared state. In such case,
the notion of "state" can be concretized as a value of some type T,
and the notion of an "effect" can be concretized as a function that
accepts a value of type T (initial state) and produces a new value of
type T (final state). The type `T` (or a type built from `T`) can be
declared as Monad, and the logic of plumbing states between two
computations can be abstracted out into the bind function of the
Monad.

However, it is crucial to note that the notion of state is not a
definitional characteristic of a monad. As mentioned previously, there
is nothing more to a monad than the plumbing logic. If it so happens
that there exists a meaningful plumbing logic for your type T that is
useful in many different contexts, then you can declare your type T as
a monad with that plumbing logic. That is why so many types are monads
: List, Maybe, Either etc etc. For lists, plumbing logic is        

    (>>=) xs f = List.concat $ List.map f xs

For Maybe,  it is:

    (>>=) Nothing f = Nothing
    (>>=) (Just x) f = f x

