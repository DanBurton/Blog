My experience with Typed Racket
===============================

A while ago, I began a project to re-implement
Racket's web server library in Typed Racket (TR)
by providing type annotations to the existing code.
I began by typing the xml collection library,
but after spending a lot of time on just that,
I decided to drop the project.
I think TR is a very impressive feat,
and I really like the direction it is going,
but I feel that the current state of TR
is not adequate for typing large swaths of pre-existing Racket code.
In this document, I will attempt to explain
the road blocks and speed bumps I encountered
while working on this project.

Although I will focus on some negative aspects here,
I do not wish to communicate that my overall experience
with TR was bad. Quite the contrary,
friends on the #racket irc channel were extremely helpful,
and for every buggy type annotation that I demonstrate,
there are mountains of code that are easily
and effortlessly typed.
Also, most of these problems could be avoided
by simply starting your project with Typed Racket,
rather than trying to go back to old code and type it.


Expectations
------------

Let me start by stating what I expect, or rather,
what I wish were true of Typed Racket (TR).

First, **I'd like TR to be a drop-in Racket replacement
for *any* module.** I'd like to be able to take a project,
and pick any file written in regular racket,
and just add type annotations and have it just automagically work
with the rest of the project.

Second, **I'd like to be able to take *any* racket code,
and provide a type for it.** Anything I write in regular racket
should *somehow* be typeable in TR,
*without modifying the code itself*. Sometimes, TR type annotations
are intrusive, but as long as the original logic remains identical,
I consider that to be "unmodified" code.
This is essentially impossible, so let me limit that a bit:
any *sane code* that I write in Racket should be typeable by TR.
Most people write code with particular types in mind,
whether or not they are explicitly using type annotations.
The xml collection code I was working with provided
types in the comments for every function, and also provided
contracts for most functions. I think it's reasonable to expect
this kind of code to be typeable.

Now with that in mind,
let's start with one of the first bugs I ran into,
and as it turns out, one of the most devastating.


Regarding Structs
-----------------

TR originally targeted R5RS, and used to be called "Typed Scheme".
This perhaps explains why struct support isn't quite there.

As a passing thought, let me just mention that there is an unfortunate
disconnect between TR conventions and racket struct conventions.
It is typical for TR types to be capitalized (e.g. `Continuation-Mark-Set`).
This helps visually distinguish type names,
and I think it is a good convention.
However, it is typical for racket structs to be lowercased
(e.g. `(struct location (line char offset))`). This is convenient
because it makes for nice derived identifiers (e.g. `location-line`).
However, there is a disconnect between the two conventions:
TR will create a type with the same name as the struct,
which means that we usually end up with a lowercase type name.
This is nothing that a `define-type` can't fix, but it's annoying
nonetheless.

### Extending a struct

Now on to the main event.
Consider these two racket files:
the first provides a struct,
and the second creates another struct which extends the first.

foo.rkt

~~~~ {.scheme}
#lang racket
(define-struct foo ())
(provide (struct-out foo))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bar.rkt

~~~~ {.scheme}
#lang racket
(require "foo.rkt")
(define-struct (bar foo) ())
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's try to convert foo.rkt to Typed Racket.
We'll simply switch `define-struct` to `define-struct:`.
If that struct had fields, we would provide type annotations for those, too.

foo.rkt

~~~~ {.scheme}
#lang typed/racket
(define-struct: foo ())
(provide (struct-out foo))
~~~~~~~~~~~~~~~~~~~~~~~~~~

That file works just fine, but now if we try to run
bar.rkt, the type checker reprimands us:

   Type Checker: The type of struct:foo cannot be converted to a
   contract in: struct:foo5

Thus was born
[ticket 12503](http://bugs.racket-lang.org/query/?cmd=view&pr=12503).

### Typing a stream consumer

On the side, I've been following recent iteratee conversations
in the Haskell community, and wanted to write up "pipes" in TR.

A simplified version of a "pipe" is a stream consumer.
It consumes an unknown number of inputs of the same type,
and then produces some result. The consumer therefore has two states:
"need another input", or "have a result".
(Let's ignore side effects for the sake of simplicity here.)
This is easily written in Racket
using structs to distinguish the two cases.

~~~~ {.scheme}
#lang racket
(require racket/match)

(struct fun (f))
(struct done (result))

; A way to run a consumer by giving it the same thing over and over
(define (repeatedly-apply consumer x)
  (match consumer
    [(fun f) (repeatedly-apply (f x) x)]
    [(done result) result]))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now we wish to type this code. TR's union types should do the trick.

~~~~ {.scheme}
#lang typed/racket
(require racket/match)

(struct: (I R) fun ([f : (I -> (Consumer I R))]))
(struct: (R) done ([result : R]))

(define-type (Consumer I R)
  (U (fun I R)
     (done I R)))

; A way to run a consumer by giving it the same thing over and over
(: repeatedly-apply ((Consumer I R) I -> R))
(define (repeatedly-apply consumer x)
  (match consumer
    [(fun f) (repeatedly-apply (f x) x)]
    [(done result) result]))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unfortunately, in Racket 5.3, TR says that this is a type error:

    Type Checker: Structure type constructor fun applied to
    non-regular arguments (g5588 R) in: (fun I R)

Fortunately, merely a few days after I created
[ticket 12999](http://bugs.racket-lang.org/query/?cmd=view&pr=12999),
a fix was patched onto HEAD. Keep up the good work, guys!

Filters
-------

Type filters are a really cool feature of Typed Racket,
and are essential to typing Racket code.
Basically, whenever you use a function with a filter attached,
you can refine the type information for code constructs with
multiple branches, such as `cond` and `if`.
See the pastebin link at the end of this section
for a more detailed explanation of type filters;
I'm unaware of any good official documentation on the topic.

One annoyance I ran into was being unable to control
which filter a function I defined has. For example,
suppose I have a function that determines whether
its input is a happy char.

~~~~ {.scheme}
#lang racket
(require racket/match)

(define (happy-char? c)
  (match c
    [(or #\h #\a #\p #\y) #t]
    [_ #f]))

(andmap happy-char? (string->list "happy")) ;; => #t
(andmap happy-char? (string->list "sad"))   ;; => #f
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(Coming from Haskell, I really like racket/match.)

We'd like to define a type to represent happy chars.
Happily, this can be done in Typed Racket with relative ease.

~~~~ {.scheme}
#lang typed/racket

(define-type Happy-Char (U #\h #\a #\p #\y))
(define-predicate happy-char? Happy-Char)

(andmap happy-char? (string->list "happy")) ;; => #t
(andmap happy-char? (string->list "sad"))   ;; => #f
~~~~~~~~~~~~~

Sadly, this *requires* using `define-predicate`.
Remember that one of the things I expect from TR is that
*all sane Racket code* that I write should be typeable.
If we try to type the happy-char? that I wrote originally,
we'll run into problems:

~~~~ {.scheme}
#lang typed/racket
(require racket/match)

(define-type Happy-Char (U #\h #\a #\p #\y))

(: happy-char? (Any -> Boolean : Happy-Char))
(define (happy-char? c)
  (match c
    [(or #\h #\a #\p #\y) #t]
    [_ #f]))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The type system says "no" to this.

    Expected result with filter ((Happy-Char @ c) | (! Happy-Char @ c)),
    got filter (Top | Top) in: (match c ((or #\h #\a #\p #\y) #t) (_ #f))

`: Happy-Char` is the filter part of the function type annotation:
if this function produces `#t`, then the result is guaranteed
to have type `Happy-Char`, and if this function produces `#f`,
then the result is guaranteed to *not* have type `Happy-Char`.
In error messages,
TR expresses this as ((Happy-Char @ c) | (! (Happy-Char @ c))).

Aside: One annoyance is that there is no way for the programmer to annotate
anything other than a filter of the form (definitely-yes | definitely-no); see
[feature request #12528](http://bugs.racket-lang.org/query/?cmd=view&pr=12528)
for details.

Back to the issue at hand.
The main reason this is invalid is because
TR is simply unaware of racket/match; filters don't flow through
branches of racket/match like you would expect them to.
(This is on the long term list of goals for TR.)

However, there is a more fundamental problem with
the current implementation of filters.
I wrote up a Typed Racket file with comments
that explain what filters are, and one of the
latest limitations that I ran into:
[http://pastebin.com/JQ9txdrX](http://pastebin.com/JQ9txdrX).


Macros
------

Optional arguments are somewhat annoying to deal with in TR.
Suppose I want to type the following function:

~~~~ {.scheme}
#lang racket
(define (foo x [y 3]) (+ x y))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TR provides the `case->` type, which allows a function
to have multiple arities. We can use this to type `foo` like so:

~~~~ {.scheme}
#lang racket
(: foo (case-> (Number -> Number)
               (Number Number -> Number)))
(define (foo x [y 3]) (+ x y))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Not bad, though it's a bit annoying to have to repeat the other arguments.
Well, in Racket, when you find yourself writing the same flavor of
annoying code over and over, what do you do? You write a macro!

I'm not much of a macro wiz, but
here's a function describing something like the macro I'd like to write:

~~~~ {.scheme}
#lang racket

(define (case-opt s)
 (match s [`(,args ? ,opt-args -> ,result)
  (match opt-args
   [(list)
    (append args `(-> ,result))]
   [(cons opt-first opt-rest)
    `(case->
      ,(append args `(-> ,result))
      ,(case-opt `(,(append args (list opt-first)) ? ,opt-rest -> ,result)))]
  )]))

(case-opt '((Foo Bar) ? (Baz Quux) -> End)) ;; =>
;; '(case-> (Foo Bar -> End)
;;   (case-> (Foo Bar Baz -> End)
;;            (Foo Bar Baz Quux -> End)))
~~~~~~~~~~~

Seems reasonable, right? I would like a cleaner syntax for optional args,
and it seems like a straightforward desugaring of my desired syntax
could be accomplished through the macro system.
Alas, Typed Racket hijacks macros, and happens before them.

~~~~ {.scheme}
#lang typed/racket

(define-syntax-rule (never-mind-me t) t)

(: x (never-mind-me Integer))
(define x 3)

;; Type checker: Unbound type name never-mind-me in: never-mind-me
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `define-type` mechanism could cover this simple example,
but isn't flexible enough to define more complex desugarings,
like `case-opt`.
There are probably some good reasons to keep it this way,
but it's disappointing to run into restrictions like this;
it just feels non-Racket-y.

Contracts
---------

A Good Racket library will often provide *contracts*
with the functions that it exports. Contracts are basically
restrictions on input and promises about output that are checked at runtime.
At this point in time, I think it's safe to say that Racket is
*the one true implementation* of Contracts, and other languages
sometimes provide a dumbed down version of them.

There is a lot of overlap between contracts and a type system.
If TR is to serve as a drop-in replacement for Racket,
then it needs to be able to define and export contracts just
like regular Racket. Remember, an important use case of TR is to
type *some* code originally written in Racket,
in such a way that said code behaves just like it used to,
without having to type any code that depends on it.

Contracts can behave like predicates, and are therefore
connected to the idea of type filters.
Consider the following simplified code taken from
collects/xml/private/xexpr.rkt:

~~~~ {.scheme}
#lang racket/base
(require racket/contract)

(define (correct-xexpr? true false x) ...)
(define (xexpr? x) (correct-xexpr? (λ () #t) (λ (exn) #f) x))
(define (validate-xexpr x) (correct-xexpr? (λ () #t) (λ (exn) (raise exn)) x))

(define xexpr/c
  (make-flat-contract
   #:name 'xexpr?
   #:projection
   (lambda (blame)
     (lambda (val)
       (with-handlers ([exn:invalid-xexpr?
                        ... ])
         (validate-xexpr val)
         val)))
   #:first-order xexpr?))
~~~~~~~~~~~~~~

The contract `xexpr/c` is designed around the `correct-xexpr?` function.
Rather than spitting out plain #t or #f values, `correct-xexpr?` can take
two actions to run under the "true" or "false" circumstances, respectively.
The "false" action must be a function that can take an exn:invalid-xexpr
as input. This design allows `correct-xexpr?` to provide detailed
custom error messages in its implementation, and the caller can choose
whether to inspect the error message, or simply throw it away.

Like racket/match, contracts play a crucial role in large,
well-designed Racket programs, but Typed Racket just isn't powerful enough
yet to grant the programmer the ability to customize contracts.
Instead, you can only generate contracts mechanically for
a given data type using `define-predicate`.
`define-predicate` is an *invasive* change; it forces me to
throw away the custom code that served the same purpose,
and to modify *all* code that depended on custom behavior
of the pre-existing code.


Conclusions
-----------

I love Typed Racket. I hope this post does not discourage you
from looking into TR. I *especially* think that TR
is well-suited to new projects that can be built from the ground up
with TR in mind. The TR type system is surprisingly flexible
about the programs that it can type.

Unfortunately, TR doesn't quite cover all of Racket.
It can serve you very well as Typed Scheme,
but it lacks full support for and cooperation with Rackety things such as
structs, pattern matching, contracts, and macros.
I eagerly look forward to the day when Typed Racket fully
and completely meets my expectations, and I wouldn't be too surprised
if this happened over the next few years.

