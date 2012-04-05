#Description and motivation 

##What is a type-safe URL?

> Type safe URLs encode all accessible paths of a web application
> via an algebraic datatype
> defined specifically for the web application at hand.
> All urls within the application, then, are generated from this datatype,
> which ensures statically that no [internal] dead links
> are present anywhere in the application.

~ [Ozgun Ataman's idea proposal](http://hackage.haskell.org/trac/summer-of-code/ticket/1621)

##Why should Snap provide type-safe URLs?

Snap inherits Haskell's emphasis on reliable, correct code.
Type-safe URLs, implemented properly, have several advantages over
raw string manipulation.

* They are easier to use: the programmer is freed from having to
serialize the data encoded in a String representation of a URL.
* They are more modular: refactoring the appearence of a particular kind of URL
is done in a central location.
* They are safer to use: typos can be caught via type checking,
rather having to hunt for broken links.

##Will this benefit any projects outside of Snap?

Snap is a web framework used by a number of other projects
and people developing websites.
Having a system for type-safe URLs with Snap will benefit anyone using the framework.
Additionally, since Heist is a general-purpose HTML templating engine,
code written for Heist can benefit anyone who uses the library
whether they use Snap or not.
Since the project also involves integration with web-routes,
any improvements made to web-routes along the way
will benefit current and future users of that library.


#Plan

##Goals

The primary goal of this project will be to
develop a working type-safe URL solution for the Snap web framework.
The solution should embody idiomatic Snap,
integrating cleanly with Snap's other features (especially snaplets),
and should be a pleasure to use.

Secondary goals include
providing valuable assessment and feedback for the web-routes package,
and enhancing Heist in such a way that it can continue to be used outside of Snap.
I will also port snapframework.com to use type-safe URLs,
which will increase the quality of the website,
and will also serve as proof of the usability of the new system,
helping us to get a good feel of how it works with Heist.


##Resources 

[Yesod](http://www.yesodweb.com/),
[Play!](http://www.playframework.org/),
and [web-routes](http://hackage.haskell.org/package/web-routes)
all provide solutions for type-safe URLs.
Reviewing these implementations will undoubtedly be valuable information
to consider when crafting a solution for Snap.

Whenever I am at work on this project,
I intend to be on #haskell and #snapframework irc channels.
In my experience, there is almost always someone knowledgable on #haskell
willing to help with even the most esoteric of tasks.
I intend to broadcast my progress continually on #snapframework.
I foresee StackOverflow, reddit, and possibly even mailing lists
being additionally valuable resources as I work on this project.

I will post (at minimum) weekly updates to a blog,
and will strive to post summary posts at least montly,
with sufficiently interesting general content
related to this specific task
to be posted to /r/haskell.


## Implementation sketch

Users of Snap's type-safe URLs will specify
a data type which will serve as a site map.
They will also specify routing,
which will simply define how to transform a URL
into that data type, and vice versa.
Serving a URL will be a simple matter of
pulling apart the data type, which will contain
all of the information necessary to identify the request
in a natural way.
To create a URL, you simply pass a value of that data type
to a renderer function.
Heist could be made aware of URLs,
or they could be rendered into Text before Heist even sees them.


## Timetable and Deliverables

Deliverables include blog posts and the working implementation,
along with intermediate releases, a test suite,
benchmarks, and documentation.


### Community Bonding Period

From April 23 to May 20,
I will primarily focus on familiarizing myself
with the code base for Snap and Heist.

* April 27: summary blog post - describe project
* May 4: blog post
* May 11: blog post
* May 18: blog post

I will begin weekly blog posts during this period,
even though it is not part of the "actual"
Summer of Code time.

### Primary research period

From May 20 to June 17,
I will primarily investigate web-routes
as a Snap solution for type-safe URLs.
I will review Yesod and Play! solutions,
and write a general blog post comparing and contrasting
these to the needs of Snap.
Reddit feedback from this could prove quite valuable.

During this time, I will develop some code,
attempting to use web-routes.
This code will mostly serve as proof-of-concept,
and is for experimenting and eventually determining
if web-routes is right for Snap.

* May 25: blog post
* May 30: release code that compiles, uses Heist and web-routes
* June 1: blog post
* June 7: release possible (simple) imlementation using web-routes
* June 8: summary blog post - type-safe URLs in Yesod, Play!, and Snap
* June 10: decide, with Snap community,
whether or not to pursue web-routes as a solution for Snap
* June 15: blog post

### Primary coding period

From June 18 to August 5,
I will spend the bulk of my time coding.
By this point, the Snap community will have come to a conclusion
about the best way to start actually implementing type-safe URLs for Snap.

* June 22: blog post
* June 28: release something that compiles and works to some degree
* June 29: blog post
* July 5: release something decent, plus example usage.
Should already be cooperating with snaplets.
* July 6: general blog post - design and benchmarking type-safe urls
* July 10: initial snapframework.com port, using code so far.
* July 13: blog post
* July 13: mid-term evaluation
* July 19: release something nice, plus significant test cases.
This should be good enough to actually recommend to real Snap projects for
early adoption.
* July 20: blog post
* July 26: release a solid test suite and examples, plus documentation
* July 27: blog post
* August 2: release "stable" version, and accompanying snapframework.com port
* August 3: general blog post - announce stable version (more benchmarks)


### Wrap-up coding period

From August 5 to August 20,
I will refine the code,
and beef up the documentation.

* August 10: blog post
* August 15: code reviewed; shining example of Snap style
* August 17: blog post
* August 20: comprehensive documentation complete.

By August 20, there should be nothing holding my code back
from being accepted with open arms into the Snap and Heist
code bases.

### The end

* August 24: Final evaluation

#Personal details and qualifications

My name is Dan Burton.
I have been learning Haskell
and involving myself in the Haskell community
since December 2010.
I have made several toy projects in Haskell
(see [https://github.com/DanBurton](https://github.com/DanBurton)).
I am just now completing a custom course
based on Types and Programming Languages,
taught by Racketeer [Jay McCarthy](http://faculty.cs.byu.edu/~jay/home/).
I used Haskell to implement various ideas in that book
(For example, [System F](https://github.com/DanBurton/Blog/blob/master/Literate%20Haskell/SystemF.lhs)).
I have been especially active on StackOverflow,
and am currently among the top 20 Haskell answerers of all time,
and am currently the top Haskell asker of all time
(See [http://stackoverflow.com/tags/haskell/topusers](http://stackoverflow.com/tags/haskell/topusers)).

As a result of the aforementioned details,
I feel I am qualified for this project due to my knowledge of Haskell,
my understanding of the type-safe URL problem,
my solid plan to solve it,
and my (clearly demonstrated) capability to
draw aid from and contribute back to the Haskell community.

See also: [My profile on the Haskell Wiki](http://www.haskell.org/haskellwiki/User:Drb226)
