Let me take you on a journey. It is a long journey,
and this long journey is not for everyone. This is the journey
to understanding Monads. If you are not prepared to wholeheartedly
embark on this journey, then stop reading now. Yes, now. Just leave.
I need your undivided attention, and a nontrivial amount of your
time in order to take you on this journey. It doesn't have to be
travelled all at once, but it does have to be embarked upon with
the intent of emerging with a confident understanding about Monads.
I need you to trust me. This is going to be wordy. This is *not*
the bare minimum you need to know about Monads. Are you ready to
take the plunge?

So, you want to learn the ways of the Monad.
Perhaps you've heard that monads are burritos,
or space suits, or monowhoozits in the category of endowhatzits.
Perhaps you've heard that monads are secretly hiding inside
of the languages you use every day. Perhaps you've heard
that monads are semicolons, or sugar cookies, or superpowers.
I want you to forget all that.

If you want to come on this Monadic journey with me,
the first thing you must do is forget. Forget everything
you thought you knew about monads. Go ahead, take a minute
to drain your mind of all of the half-baked preconceptions
you have about monads. In fact, stop using the term "monads".
Don't use the phrase "a monad". To reinforce your forgetfullness,
I will now refer to our concept of interest as "Interface M", or just
"M" for short.

Have you finished draining your mind of the confusion and
uncertainty you have about M? Good. Now, let's quickly cover
what your expectations should be for the M experience
that I am about to unleash upon you. I expect that you are
already familiar with a typical imperative programming language,
such as C, Java, or Python. I expect you have some basic background
in mathematics; a rudimentary understanding of algebra will suffice.
These two things (and, implicitly, a firm grasp of the English language)
are all I need from you. Here is what you can expect from me.
By the end of this journey, you will honestly be able to say
"I understand monads". (Even though as you say that,
you will cringe because you said "monads";
nevertheless this phrase sounds impressive
to the muggles.) You will be able to join in the conversation with the cool
kids about M. You will recognize that M is not such a big deal.
You will draw diagrams about M. You will learn how to use M.
You will recognize M-like patterns in other languages (we will use Haskell
to explore the meaning of M), and secretly wish that other languages had
better support for M.

Are you secretly saying "Monads" in your head every time you see "M"?
Stop that. Just say "M" or "Interface M" in your mind when you read "M".
If you *must*, then just say "Monad", without the "s".

I am going to teach you about M by using the programming language Haskell.
"But I don't know Haskell", you should be saying right about now. That's OK.
I'm going to teach you some right now. Please install the
[Haskell Platform](http://www.haskell.org/platform/)
immediately, and grab a decent text editor; I don't care which one, but
I recommend that you select one that provides syntax highlighting
for Haskell code.

While the Platform is installing (you *are* installing it, right?)
take this time to mentally prepare yourself to learn Haskell. Again,
forget everything you thought you knew. Clear your mind. Trust me;
I'll take you from the most basic of syntax and lift you up to a
rudimentary understanding of Haskell. But I need you to forget some more.
Forget everything you know about programming. Forget the ones and zeroes.
Forget the curly braces, classes, pointers, variables, everyting. Just
let go; I am taking all of that away from you. Haskell is different. You can
recognize similarities to other languages later, but for now, I want you to
abandon the idea that you can apply your current programming knowledge
and techniques to Haskell. While we're learning Haskell,
it's OK to wonder "how do I do X in Haskell?" However, I will not be addressing
such concerns. We are learning Haskell purely to aid in our understanding of M.
If you already know Haskell, then I still recommend that you try to go through
the mental exercise of forgetting everything you know.

Has the Haskell Platform installed successfully yet? Good. Let's open up
the GHC Interactive environment, known as `ghci`. You should be able to
do this from the command line by invoking the `ghci` command. If you have
trouble getting this far, then seek help from the #haskell irc channel,
or your local Haskeller. If you want the prompt to look like mine,
then type the following command and hit enter:

    :set prompt "ghci> "

Commands in ghci are prefixed with a colon. I'll teach you a few commands
as we go along.

Now, let's talk about *expressions* in Haskell. First, try this in ghci:

    [ghci]
    "Hello" ++ " World"

By typing the *expression* `"Hello" ++ " World"` into ghci,
we are telling it that we want to know the *value* of that expression.
Ghci can only display the value of expressions that can be `Show`n;
functions, for example, cannot be shown.

An expression in Haskell is either a *literal*, a *named value*, or a
*function application*. Literals include numbers, strings, and lists.
Named values can be user defined, or defined in libraries. For example,
`pi` is defined for you already:

    [ghci]
    pi

Within the interactive prompt, you can create your own named value
by using `let name = expression`.

    [ghci]
    let me = "Dan"
    "Hello, " ++ me

This can be confusing to newcomers, because in a Haskell *source* file,
`let` is ommitted. I will explain why this discrepancy exists later.

Functions are mechanisms that take inputs and produce outputs. In algebra,
you would write a function like this: `f(x) = x + 3`. Function application
would look like this: `f(24)`. In Haskell, functions have a slightly different
syntax, so the same function is written like this: `f x = x + 3`, and applied
like this: `f 24`. (You need to prefix the definition with `let` if you are in
the interactive environment.) You should think of functions in Haskell as being
*exactly* the same as functions in math. Were you thinking about functions in
your preferred language, and wondering how Haskell functions compare? Stop that!
Forget that language for now, and trust me. "But what about...?" you might be
wondering. "Can Haskell do...?" It's OK to wonder this, but just know for now
that I have taken everything away from you. We can reach a stage where we will
see that Haskell has its own way of doing things,
but you must stick with me all the way through, from the very beginning.
You will be pleasantly surprised when I give you back some of the things
you were wanting to do, when you realize that the most logical and natural way
to do those things is through M.

Functions can take multiple arguments. For example:

    [ghci]
    let greet greeting name = greeting ++ ", " ++ name
    greet "Hello" "Dan"

If you haven't guessed by now, `++` in Haskell is the string concatenation
operator. Some functions, like `++`, are *infix* functions: functions that take
two inputs, and are called by putting one input on each side. You can
define your own infix functions:

    [ghci]
    let greeting *$* name = greeting ++ ", " ++ name
    "Hello" *$* "Dan"

Basically, if the name consists entirely of symbols, then it is considered
infix by default. This isn't entirely true; you can read the
[Haskell Report]()
for a more careful explanation. I'm not really trying to equip you to write
Haskell programs; I just want to make sure that I can write some Haskell,
and you can understand what it means, as well as copy it and run it for
yourself.

Now, we'd better learn a little bit about M before you start losing interest.
Here is the first thing you must learn: M is an interface. Let's look at another
of Haskell's simple but important interfaces: `Num`. We can use the `:info`
command to help us here.

    [ghci]
    :info Num

Well, that's quite a lot of new stuff to take in. First,
Haskell type signatures. They always appear in the form `identifier :: type`.
Infix functions are displayed with parens around them in their type signature,
as you can see with `(+)` and friends. The first portion of the stuff
ghci is telling us here is what the interface is called, and what functions
pertain to this interface. Then it lists the instances currently in scope.
Currently, for me, there are `Num` instances for `Integer`, `Int`, `Float`,
and `Double` in scope. (sneak peek: try using `:info` to learn about the
`Monad` interface. Try it with `Show`, the interface that determines what
ghci can print out. What do you think that fat arrow `=>` means in the
section about `Show` instances?)

Well, that's all for now. I know you haven't learned much about M yet,
but you will. Step one is to learn some Haskell syntax, and if you've been able
to learn a programming language before, then you should be able to pick up
Haskell syntax fairly quickly. However, please remember to forget your previous
programming experience while you are learning Haskell, for two reasons.
First: because the reason I am teaching you Haskell is not to equip you with the
ability to program in Haskell (though I will point you to some exercises and
resources that will aid you in this also, if that is what you desire). I am only
teaching you a smidgen of Haskell so that we can talk about M. Second:
because the other reason I am teaching you Haskell is so that you start missing
the features of your favorite language. I will show you how M empowers Haskell
and restores those features to what otherwise might appear to be a gimped
language. The true enlightenment will come, however, when I show you how
M empowers any language. Forgetting all languages and only learning a little
bit of Haskell just happens to be a useful way to convincingly illustrate
M's true power, because Haskell lacks some of the mechanisms that are simply
supported at a language level in today's common languages.

If you have learned nothing else, remember this: don't use the terms "a monad"
or "monads". Simply say "Monad", and when you say that, what you really mean
is "The Monad Interface". Monad is an interface. When you hear people say
"a monad" or "monads", if you must, you can think of it in terms of the "is-a"
relationship. Data types which have an *instance* of Monad can be said to *be*
"monads". (When ghci `:info` tells you `instance Monad Maybe`, you could read
this as "Maybe *is a* Monad". However, it is better read: "There is an instance
of the Monad Interface for Maybe".)
Next time, we'll learn more
about what that interface is, and some ways you can use it in Haskell.
Then, we'll explore the explanation from Category Theory, and
draw some diagrams. For the brave, we'll even dip our toes into Comonadic
waters, just to spice things up.

