From oleg at okmij.org Sun Aug 31 20:40:41 2008
To: haskell@haskell.org
Subject: The initial view on typed sprintf and sscanf
Message-ID: <20080901024041.07B71AE54@Adric.metnet.fnmoc.navy.mil>
Date: Sun, 31 Aug 2008 19:40:41 -0700 (PDT)
Status: OR


We demonstrate typed sprintf and typed sscanf sharing the same
formatting specification. Our solution is surprisingly trivial: it
defines a simple embedded domain-specific language of formatting
patterns. The functions sprintf and sscanf are two interpreters of the
language, to build or parse a string according to the given
pattern. Our solution relies only on GADTs. We demonstrate that
lambda-abstractions at the type level are expressible already in the
Hindley-Milner type system; GADT with the included polymorphic
recursion help us use the abstractions.

The typed sprintf takes the formatting specification and several
arguments and returns the formatted string. The types and the number
of arguments depend on the formatting specification. Conversely, the
typed sscanf parses a string according to the formatting
specification, passing parsed data to a consumer function. Again, the
number and the types of the arguments to the consumer depend on the
formatting specification. The typed sprintf problem has been
investigated extensively: the first solution was shown by Danvy; more
solutions were proposed by Hinze and Asai. The typed scanf problem
received significantly less attention, if any. It seems that the
implementation of the typed sprintf and sscanf sharing the same
formatting specification has not been known.

Here are a few examples of the typed sprintf and sscanf

> tp1 = sprintf $ lit "Hello world"
> -- "Hello world"
> ts1 = sscanf "Hello world" (lit "Hello world")  ()
> -- Just ()

> tp2 = sprintf (lit "Hello " ^ lit "world" ^ char) '!'
> -- "Hello world!"
> ts2 = sscanf "Hello world!" (lit "Hello " ^ lit "world" ^ char) id
> -- Just '!'

> fmt3 = lit "The value of " ^ char ^ lit " is " ^ int
> tp3 = sprintf fmt3 'x' 3
> -- "The value of x is 3"
> ts3 = sscanf "The value of x is 3" fmt3 (\c i -> (c,i))
> -- Just ('x',3)

A formatting specification is built by connecting primitive
specifications (such as lit "string", int, char) with (^). We observe
that whereas 
     sprintf $ lit "Hello world"
has the type String, 
     sprintf $ lit "The value of " ^ char ^ lit " is " ^ int
has the type Char -> Int -> String. Likewise, the type of the consumer
of the values parsed by sscanf varies with the formatting specification.
The example of tp3 and ts3 demonstrates that sprintf and sscanf can
indeed use exactly the same formatting specification, which
is a first-class value.

Printf and scanf are present in many languages (e.g., C and Haskell);
in most of the cases, they are not type-safe: the type checker does
not stop the programmer from passing to printf more or fewer arguments
than required by the formatting string. OCaml supports typed sprintf
and sscanf, which relies on a particular weird typing of these
functions, resulting in formatting specifications being not first
class. Our sscanf has exactly the same interface as that in OCaml,
modulo weird typing.

The complete code of the typed sprintf and sscanf with more examples
is available at PrintScan.hs
It can be used in Haskell programs as it is; it is trivial to arrange
it in a Haskell library or Hackage package.


Our implementation is based on the observation of Hinze (Ralf
Hinze. Functional Pearl: Formatting: a class act. JFP, 13(5):935-944,
September 2003) that the formatting specification is a `type
transformer', a functor. For example, the formatting specification
'int' is associated with a functor transforming a type 't' to a type
'Int -> t'. Hinze represented functors in Curry style; the
functor associated with 'int' is then (Int ->).  However, a more
direct, Church-style representation is also possible.  Let us consider
a type "F a b" where "F" is a binary type constructor, "a" is a type
variable, and "b" is a type that may include one or more occurrences of
"a". That type is essentially a type abstraction, the type-level
lambda-function. To apply the function to an argument, we unify "a"
with the argument; the second parameter of "F" is the result of the
application. This is the standard emulation of lambda-calculus in
Prolog.

Based on this observation, we define the language of formatting
specifications as follows:

> data F a b where
>     FLit :: String -> F a a
>     FInt :: F a (Int -> a)
>     FChr :: F a (Char -> a)
>     (:^) :: F b c -> F a b -> F a c

> (^)  = (:^)
> lit  = FLit
> int  = FInt

The type of (:^) expresses the inverse functional composition of two
type-level lambdas; one may read "F" as `big lambda'.

The formatting specification can be interpreted in two different ways,
for printing and for parsing. The types of the interpreters are pleasingly
symmetric:

> intp :: F a b -> (String -> a) -> b
> intp (FLit str) k = k str
> intp FChr       k = \x -> k [x]
> intp (a :^ b)   k = intp a (\sa -> intp b (\sb -> k (sa ++ sb)))


> ints :: F a b -> String -> b -> Maybe (a,String)
> ints (FLit str) inp x = maybe Nothing (\inp' -> Just (x,inp')) $ 
>                           prefix str inp
> ints FChr (c:inp) f = Just (f c,inp)
> ints FChr "" f      = Nothing
> ints (a :^ b) inp f = maybe Nothing (\(vb,inp') -> ints b inp' vb) $ 
> 		          ints a inp f


The printing interpretation implements Asai's accumulator-less alternative to
Danvy's functional unparsing:
	http://pllab.is.ocha.ac.jp/~asai/papers/tr07-1.ps.gz
The printing interpretation of (a :^ b) is string concatenation in CPS form.

The sprintf and sscanf are then simple wrappers:

> sprintf :: F String b -> b
> sprintf fmt = intp fmt id

> sscanf :: String -> F a b -> b -> Maybe a
> sscanf inp fmt f = maybe Nothing (Just . fst) $ ints fmt inp f

The problem of the typed printf and scanf over the same formatting
specification has been posed by Chung-chieh Shan in a discussion with
Kenichi Asai and Yukiyoshi Kameyama. I'm grateful to them for
inspiration.


