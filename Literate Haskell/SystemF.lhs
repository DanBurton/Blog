
Background
=====================================================================

  As an exercise while reading through
Types and Programming Languages,
I decided to implement an interpreter
and typechecker for System F,
using HOAS (Higher-Order Abstract Syntax)
and Haskell's GADTs (Generalized Algebraic Data Types).
There were some really cute tricks
that made the implementation fairly simple,
so I decided to blog about it.

> {-# LANGUAGE GADTs, KindSignatures, FlexibleInstances, EmptyDataDecls #-}

> import Prelude hiding (succ)
> import Control.Applicative
> import Control.Monad

  I've left the module declaration out of this version
for the sake of space. See
[http://github.com/DanBurton/system-f](http://github.com/DanBurton/system-f)
for the module-ized version with few comments.


Preliminaries
=====================================================================

  I've provided a few synonyms around the Either type,
in the event that I might want to change the error handling
in the future. Basic stuff, really.

> type ErrOr a = Either String a

> good :: a -> ErrOr a
> good = Right

> err :: String-> ErrOr a
> err = Left

> isGood :: ErrOr a -> Bool
> isGood Right{} = True
> isGood Left{}  = False

  This next function is a bit of an abombination.
I promise I won't abuse it too much.

> get :: ErrOr a -> a
> get (Right x) = x
> get (Left e)  = error e


Data types
=====================================================================

  The language should provide some primitives.
Here I've provided Num primitives which correspond to
natural numbers, sort of. Primitive doesn't just have kind `*`,
it has kind `* -> *`, meaning primitives are parameterized over
some Haskell type. More on this later.

> data Primitive :: * -> * where
>   Num :: Integer -> Primitive Integer
>   Succ :: Primitive (Integer -> Integer)

  In retrospect, it is slightly confusing
that I named one of these constructors "Num".
This is a *value*, so Haskell doesn't confuse it with
the typeclass of the same name. This might cause problems
if I turned on some of the extra kind-wankery GHC provides.

---------------------------------------------------------------------

  Types are a little more interesting.
In System F, Types are also "values" in the language:
you apply type literals to type abstractions.

  There are three types that I provide here:
the Num type, the Function type, and the Type Abstraction type.
I use V as a pun for the "forall" symbol,
which represents type abstractions.
If additional primitives were added,
then this could be extended with relative ease
(e.g. CharTy, ListTy, etc).

  Notice how Types are also parameterized on Haskell types.
We'll talk about that soon... promise!

> data Type :: * -> * where
>   NumTy :: Type Integer
>   FunTy :: Type a -> Type b -> Type (a -> b)
>   VTy :: (Type a -> Type b) -> Type (V a b)
>   TyVar :: Char -> Type a

  Now, obviously I can't count,
because I said there were three types of types,
but in fact there are four. Well the fourth (TyVar)
is used purely as a hack for the purpose of printing
and equality testing. Let's check those out:

> instance Eq (Type a) where
>   (==) = eqTy (['A' .. 'Z'] ++ ['a' .. 'z'])

  Here we are about to define equality on types
based on a helper function that takes a list of Chars.
A "forall" type is represented by a Haskell function from Type to Type,
so whenever we hit one of those, we will just pull a Char out of the list
and create a TyVar to hand into that function.
By handing the same Char to two different VTys,
we will be able to see whether or not the functions are equal.
There are some underlying assumptions going on here,
primarily that you are not constructing types using strange means.

> eqTy :: [Char] -> Type a -> Type a -> Bool
> eqTy _ NumTy NumTy = True
> eqTy cs (FunTy dom rng) (FunTy dom' rng') = eqTy cs dom dom' && eqTy cs rng rng'
> eqTy (c:cs) (VTy f) (VTy f') = eqTy cs (f (TyVar c)) (f' (TyVar c))
> eqTy [] _ _ = error "Congratulations, you've used up all of the characters. Impressive."
> eqTy _ (TyVar c) (TyVar c') = c == c'
> eqTy _ _ _ = False

  The Show instance is much the same.
I prefer my type variables to be X, Y, and Z when possible,
so the list of Chars starts with those.
Whenever we need to print out a Forall'd type,
we just pick a Char from the list, and use that TyVar hack.

> instance Show (Type a) where
>   show = showTy ("XYZ" ++ ['A' .. 'W'])

> showTy :: [Char] -> (Type a) -> String
> showTy _ NumTy = "Num"
> showTy cs (FunTy dom rng) = "(" ++ showTy cs dom ++ " -> " ++ showTy cs rng ++ ")"
> showTy (c:cs) (VTy f) = "(∀ " ++ [c] ++ ". " ++ showTy cs (f (TyVar c)) ++ ")"
> showTy [] VTy{} = error "Too many nested type applications"
> showTy _ (TyVar t) = [t]

---------------------------------------------------------------------

  Here's a lonely little line of code.
This is the entire reason for the EmptyDataDecls pragma.
The Haskell type of a System F function abstraction is parameterized 
on a Haskell type (as we will soon see). But type abstractions
are not parameterized by any pre-existing Haskell type.
Instead, they are parameterized on this V type.

> data V a b

---------------------------------------------------------------------

  Now for the exciting part, terms!
GADTs really shine here. (Well, they shine for Types too).

  There are five kinds of terms in System F:
primitives, function abstractions, function applications,
type abstractions, and type applications.
Look very carefully at the type signature for each of these constructors.

  A Primitive parameterized on Haskell type `a` turns into
a Term parameterized on Haskell type `a`.

  An Abstraction requires you to declare the input Type
and to provide a function from Term to Term. Notice how
these are parameterized on types `a` and `b` in a nifty way.

  An Application requires a Term parameterized on a function
from `a` to `b`, and a Term parameterized on `a`, and creates
a term parameterized on `b`.

  Type abstractions and applications are similar to those of functions.
The only differences are the use of the V type,
and the type abstraction does not require you to specify the type of the input.
(This implementation of System F does not support bounded quantification)
Now go back and review the type signatures for constructors of Type.
Makes sense, right?

> data Term :: * -> * where
>   Prim :: Primitive a -> Term a
>   Abs :: Type a -> (Term a -> Term b) -> Term (a -> b)
>   App :: Term (a -> b) -> Term a -> Term b
>   TAbs :: (Type a -> Term b) -> Term (V a b)
>   TApp :: Term (V a b) -> Type a -> Term b
>   Unknown :: Char -> Term a

  There's also that nifty little extra constructor: Unknown.
This is a hack analogous to TyVar; we'll see how they play together later.

  Now for another cute little trick:
getting ghci to also be the repl for our little System F language.
All you have to do is provide a Show instance for Terms
that evaluates its argument and discovers its type.

> instance Show a => Show (Term a) where
>   show t = let v = get $ eval t
>                ty = get $ typeOf t
>            in show v ++ " : " ++ show ty

Using a GHC < 7.4, this Eq instance is required for what follows.
Implementing Eq on terms could be done in similar fashion to
Eq on Types, but I was too lazy to do it.

> instance Eq (Term Integer) where
>   (==) = undefined

  Here's a related trick to make our System F even more convenient,
by defining `fromInteger = num` for `Term Integer`, we can now use
numeric literals as primitive values in our System F language.
`num` is defined near the end, along with a few other
language conveniences.

> instance Num (Term Integer) where
>   fromInteger = num
>   (+) = undefined
>   (-) = undefined
>   (*) = undefined
>   abs = undefined
>   signum = undefined
>   negate = undefined


Evaluation
=====================================================================

  Now for evaluation. The code is almost insanely simple.
First, let's define `eval'`, which reduces terms as much as possible,
using a big-step approach.

> eval' :: Term a -> ErrOr (Term a)
> eval' (Prim p)   = good $ Prim p
> eval' (Abs t f)  = good $ Abs t f
> eval' (TAbs f)   = good $ TAbs f
> eval' (App f x)  = do
>   f' <- eval' f
>   res <- runApp f' <*> eval' x
>   eval' res
> eval' (TApp f x) = do
>   f' <- eval' f
>   res <- runTApp f' <*> pure x
>   eval' res

  Here for function and type applications,
we rely on helper functions `runApp` and `runTApp` respectively,
which will either hit an error, or produce an actual Haskell function.

  We'll also define a "full eval" function,
which creates an actual Haskell value out of evaluating a `Term`
(or produces an error).

> eval :: Term a -> ErrOr a
> eval t = eval' t >>= valueOf

---------------------------------------------------------------------

  Here I've only provided a way to extract an Int.
Functions are left as an exercise to the reader.

> valueOf :: Term a -> ErrOr a
> valueOf (Prim n) = fromPrim n
> valueOf _ = err "Not a value"

> fromPrim :: Primitive a -> ErrOr a
> fromPrim (Num n) = good n
> fromPrim _ = err "fromPrim failed unexpectedly"

---------------------------------------------------------------------

> runApp :: Term (a -> b) -> ErrOr (Term a -> Term b)
> runApp (Abs t f) = good f
> runApp (Prim p) = runAppPrim p
> runApp _ = err "unexpected non-abstraction used in application"

  runApp is simple. If it is a function abstraction,
just grab the Haskell function that was already provided.
If it is a primitive, then provide a primitive implementation.

> runAppPrim :: Primitive (a -> b) -> ErrOr (Term a -> Term b)
> runAppPrim Succ = good $ \(Prim (Num n)) -> num (n + 1)

  Here's something really cute about GADTs.
At first I had another case,
`runAppPrim _ = err "blah blah"`
which followed the `Succ` case.
But GHC warned me about overlapping patterns!
Since I gave this function the type `Primitive (a -> b) -> blah`,
GHC *knows* that the `Num` is not a possibility.

---------------------------------------------------------------------

  There are no primitives that are type abstractions,
so `runTApp` is even simpler than `runApp`.

> runTApp :: Term (V a b) -> ErrOr (Type a -> Term b)
> runTApp (TAbs f) = good f
> runTApp _ = err "runTApp failed unexpectedly"


Typing
=====================================================================

  Now for another fun part, type reconstruction!
Given a `Term`, we want to discover its `Type`.
But here's something really cool: the `Term` and the `Type`
have to be parameterized over *the same Haskell type*!
Basically, Haskell's type checker will prevent me from
writing my type checker incorrectly.

  I think it would actually be safe to remove the Error wrapping
around the result of this funciton, and always assume that it is correct,
because the Haskell type checker should prevent you from constructing
an ill-typed Term in the first place.

> typeOf :: Term a -> ErrOr (Type a)
> typeOf (Prim p)  = good $ primType p
> typeOf (Abs t f) = FunTy t <$> typeOf (f (genTy t))
> typeOf (TAbs f)  = good $ VTy (\x -> get $ typeOf (f x))
> typeOf (App f x) = do
>   FunTy dom rng <- typeOf f
>   t             <- typeOf x
>   if (t == dom)
>     then good rng
>     else err "function domain does not match application input"
> typeOf (TApp f x) = do
>   VTy f' <- typeOf f
>   good (f' x)
> typeOf (Unknown c) = good $ TyVar c

The types of primitives are predetermined

> primType :: Primitive a -> Type a
> primType Num{} = NumTy
> primType Succ  = FunTy NumTy NumTy

  Now even more fun! In order to determine the type of a function abstraction,
we need to be able to inspect the "body" or "result" of that function.
However, since it is represented as a Haskell function, it is opaque to us!
Or is it? All we really have to do is give it some Term, any Term,
of the correct input type, and then look at the type of the result.

  So all we have to do is, given a Type, generate a Term of the correct Type.
Can we actually do that? Well, sure! Check it out:

> genTy :: Type a -> Term a
> genTy NumTy = num 0
> genTy (FunTy dom rng) = l dom (\_ -> genTy rng)
> genTy (VTy f) = TAbs (\x -> genTy (f x))
> genTy (TyVar c) = Unknown c

  First, notice the interplay between genTy and typeOf
when it comes to Unknown and TyVar. An Unknown c has type TyVar c,
and to get a value of TyVar c, just create an Unknown c! Cute.

  More seriously, this function is a testament to the awesomeness of GADTs.
Check this out:

    [haskell]
    num 0 :: Term Integer
    l foo (\_ -> genTy bar) :: Term (Foo -> Bar)

(l = Abs, see below)
genTy cannot possibly be well-typed, because these two expressions
have entirely different (and entirely concrete, non-polymorphic) types!

  ... and yet it is, and this is the real magic of parameterizing *both*
Types and Terms on Haskell types. Since NumTy is parameterized on `Integer`,
that means that the result of `genTy NumTy` must be a `Term Integer'.
But since `FunTy foo bar` is parameterized on `Foo -> Bar`,
that means that the result of `genTy (FunTy foo bar)` must be a `Term (Foo -> Bar)`.
So genTy, which would otherwise be impossible to type,
is, in fact, well typed! All thanks to (quite natural) use of GADTs and
Haskell's sexy types.

tl;dr - parametric polymorphism + GADTs = awesomesauce


Language primitives
=====================================================================

  These are just a few "primitives".
Here I use the term "primitive" to mean "you should actually write
System F expressions using these". Although with the Num typeclass hack,
`num` should be unnecessary.

> num = Prim . Num
> succ = Prim Succ

> v = TAbs
> l = Abs

> app = App
> tapp = TApp


Basic testing functions
=====================================================================

  Let's play around, defining a couple functions in System F.
You'll notice how verbose it is to perform function and type applications.
Brownie points to you if you write some Template Haskell quasi-quoting
that can prettify this. (Contribute it to the github repo linked at the top!)

A simple function that simply applies the primitive succ to its input

> succ' = l NumTy (\x -> app succ x)

The identity function. Given a type and an input of that type,
reproduce the input.

> id' = v (\t -> l t (\x -> x))

The const function. Given two types, and two inputs of those types,
reproduce the first.

> const' = v (\t1 -> v (\t2 -> l t1 (\x -> l t2 (\_ -> x))))

Given a function from X -> X, produce the function
that performs the original function twice on its given input.

> twice = v (\t -> l (FunTy t t) (\f -> l t (\x -> (app f (app f x)))))

Use the twice function on itself!

> fourTimes = v (\t -> app (tapp twice (FunTy t t)) (tapp twice t))

Example usage:

    [ghci]
    app (app (tapp twice NumTy) succ) 0

I wish this could be written in a more System F style:

    [haskell]
    [| twice NumTy succ 0 |]

----------------------------------------------------------------------

Here's a cool thing to check out. Go into ghci and try out the following:

    [ghci]
    :type const'
    typeOf const'

Cool, huh? The inferred Haskell type really captures a lot of the meaning.
