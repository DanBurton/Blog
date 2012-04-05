#Description and motivation 

##What is a type-safe URL?

> Type safe URLs encode all accessible paths of a web application
> via an algebraic datatype
> defined specifically for the web application at hand.
> All urls within the application, then, are generated from this datatype,
> which ensures statically that no [internal] dead links
> are present anywhere in the application.

~ [mightybyte's idea proposal](http://hackage.haskell.org/trac/summer-of-code/ticket/1621)

##Why should Snap provide type-safe URLs?

Snap inherits Haskell's emphasis on reliable, correct code.
Type-safe URLs, implemented properly, have several advantages over
raw string manipulation.

* They are easier to use: you don't need to know the convention used
to create the URL for a particular resource, instead, you simply need to know
its combinator and input types. The programmer is freed from having to
serialize the data encoded in a String representation of a URL.
* They are more modular: refactoring the appearence of a particular kind of URL
is done in a central location.
* They are safer to use: typos can be caught via type checking,
rather having to hunt for broken links.

##Will this benefit any projects outside of Snap?

I hope so.
The main focus will be on type-safe URLs specifically for Snap,
but in the process,
Heist, a general-purpose html templating engine,
should also benefit from the enhancement.
Analysis and potential use of web-routes should benefit that project as well.

#Plan

##Goals

[Can you give some more detailed design of what precisely you intend to achieve?]
...

##Resources 

[In what ways do you envisage interacting with the wider Haskell community during your project? e.g. How would you seek help on something your mentor wasn't able to deal with? How will you get others interested in what you are doing?]
...

##Timetable and Deliverables

[What deliverables do you think are reasonable targets? Can you outline an approximate schedule of milestones?]
...

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
