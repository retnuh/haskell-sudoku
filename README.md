# haskell-sudoku

An implementation of a Sudoku solver in Haskell, similar/ported from my [Monadoku](https://github.com/retnuh/monadoku) project (which, sadly, fizzled just at the point of using monads to structure things - oh well).

Mostly as a way of learning me some Haskell. It's a non-trivial problem that
I'm familiar with so the focus is on getting the feel for the language, toolchain and libraries,
rather than wrestling with the problem itself.

I've started with the "good enough" version, that solves most (all?) Sudoku that don't require backtracking.
I may or may not implement a version that uses backtracking. Having done one in the clojure project,
and it being a pain in the arse, I'm disinclined. On the other hand, it may be an interesting exploration
of Haskell's laziness.... Hrmmm.

## Running, building, testing

This project uses [Stack](https://www.haskellstack.org/) as the build tool, so uses the usual
`stack build`, `stack test`, and `stack run` commands.

## Thoughts

Certainly a useful learning experience. Some key points, in no particular order.

- Partial application is a very useful way of enclosing information to line up types, especially w/ state monad. It's basically a super convenient way of creating closures.
- Lenses are cool and interesting, but definately very hard for a beginner. In particular, the polymorpism stuff has some gotchas, I spent a lot of time fighting the compiler until I learned that you can't use them in let simple (untyped) let bindings, and they're hard to pass around between functions. They are handy for deeply nested state though, which is what the problem is all about.
- The error messages from GHC, while obscure, slowly start to make sense (at least part of the time :) )
- Debugging can still be quite challenging. I had to jump through some hoops to get readable trace output. I was having some problem where output from trace calls was all interleaved, even though, as far as I'm aware, I don't have any threading. In the end I ended up having to run things from ghci invoked with the following bit of shell magic (should work in bash and zsh): `stack ghci > >(tee /tmp/stdout.log) 2> >(tee /tmp/stderr.log >&2)`
- The Writer monad (transformer) is pretty cool. It's a nice way of separating out the generation of new messages from the updating of state. While working on this, one of the challenges of reading the Clojure code was separating out some of the funkiness for dealing with the two things at the same time.
- Code density between the Clojure and Haskell versions is pretty similar.
- My editor setup could use some work; I've been trying VSCode + [Haskell Language Server](https://github.com/alanz/vscode-hie-server) plugin. It's mostly okay except things get in a weird state occasionally where I'll edit a file, save, and it will nuke the change I just make until I go into the shell and do a `killall hie-wrapper`.
- The `MessageQueue` typeclass, while ultimately reasonably clean, took a long time and I spent quite a bit of time bouncing off the compiler. Still have a fair bit to learn in terms of when to use contraints for types, etc.
- Using the `PartialApplicationLSWSolver` I was able to remove the use of partial functions and possible errors present in the basic `LSWSolver`, but you can see that, with the `Set` based `MessageQueue`, that it suffers - this is due to the fact that you can't really compare functions for equality. I'm still interested in taking a look at type/data families to see if I can get the best of both worlds.
- It pays to measure! I would have thought that the `List` based `MessageQueue` would perform the worst, but looking at the [results](https://retnuh.github.com/haskell-sudoku/benchmarks.html) it is usually the best. And even though the `PartialApplicationLSWSolver` processes more messages, it is quite a bit faster than the `LSWSolver`. If I had to hazard a guess, it's probably due to the fact that it generates less intermediate "gargbage" data - i.e. less allocations. **UPDATE** Actually, I added the `SafeLSWSolver` which uses messages, etc, and it's about the same speed. My next best guess is that the splitting out of the `MessagesState` and `GameState` into two different records and updating them separately leads to less overall copying or something?

## Performance Results

Here are the [Criterion](http://www.serpentine.com/criterion/) [benchmark results](https://retnuh.github.com/haskell-sudoku/benchmarks.html)

## Future stuff

I've done the first of these, and may try second; the rest aren't too likely (at least at the moment).

- ~~make a `MessageQueue` typeclass to see if different ways of gathering the messages can reduce the message count. It's essentially just a `Monoid` and I think a sorted list should see some improvements if the `IsValue` messages get delivered before other messages.~~
- ~~Look at type families to deal with the issue of some of the partial functions. I'm interested in using Haskell to make clearly wrong things inexpressible rather than just something to avoid.~~ **UPDATE** I got this working so far using just partial application of functions; see comment in thoughts above. **SECOND UPDATE** It was straightforward to take the partial application version and tweak the `Message` type definition to make this safe, with no need for type families.
- Look at an implementation using the [Conduit](https://github.com/snoyberg/conduit) library. This would be a pretty different implementation paradigm (maybe) so could be interesting. Also looks like a very interesting library. **UPDATE** Having looked at this a little more it's not clear that it would actually be useful for an implementation - a bit of square peg/round hole going on.
- Also look at doing an implementation using [FRP](https://wiki.haskell.org/Functional_Reactive_Programming) using maybe [Reactive Banana](https://wiki.haskell.org/Reactive-banana) or [Reflex](https://github.com/reflex-frp/reflex).
- Possibly implement the addtional "contstrained by box" rule I've discovered but never implemented in the Clojure version. I don't know if this would solve the "hardest" puzzle or not, but it should be much easier to implement than a full backtracking/search based soultion.
- Implement a backtracking/search solution. Perhaps from scratch, rather than sitting on top of exisiting solution. Dunno.
- ~~Use [Criterion](http://www.serpentine.com/criterion/) to benchmark the various `Solver` + `MessageQueue` combos.~~

## Feedback welcome

I realize that this is quite unlikely to be looked at by anybody, but if anybody does look at it and has any questions, comments, feedback, suggestions or anything else, I'd love to hear it. Feel free to open a github issue or whatever.

## Results

```{bash}
┌─────────────────────────────┬─────────────┬──────────────┬───────────┬──────────┬───────────────┬────────────────────┐
│           Solver            │   Puzzle    │ MessageQueue │ Complete? │ Correct? │ Messages Used │ Messages Remaining │
╞═════════════════════════════╪═════════════╪══════════════╪═══════════╪══════════╪═══════════════╪════════════════════╡
│ LSWSolver                   │ euler       │ set          │ True      │ True     │ 1018          │ 771                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ euler       │ list         │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ euler       │ dlist        │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ euler       │ vector       │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ easy        │ set          │ True      │ True     │ 1048          │ 640                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ easy        │ list         │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ easy        │ dlist        │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ easy        │ vector       │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ mild        │ set          │ True      │ True     │ 1054          │ 701                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ mild        │ list         │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ mild        │ dlist        │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ mild        │ vector       │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult1  │ set          │ True      │ True     │ 1426          │ 458                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult1  │ list         │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult1  │ dlist        │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult1  │ vector       │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult25 │ set          │ True      │ True     │ 1791          │ 123                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult25 │ list         │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult25 │ dlist        │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult25 │ vector       │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ fiendish    │ set          │ True      │ True     │ 1508          │ 460                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ fiendish    │ list         │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ fiendish    │ dlist        │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ fiendish    │ vector       │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ euler       │ set          │ True      │ True     │ 1677          │ 203                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ euler       │ list         │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ euler       │ dlist        │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ euler       │ vector       │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ easy        │ set          │ True      │ True     │ 1594          │ 298                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ easy        │ list         │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ easy        │ dlist        │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ easy        │ vector       │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ mild        │ set          │ True      │ True     │ 1892          │ 44                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ mild        │ list         │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ mild        │ dlist        │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ mild        │ vector       │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult1  │ set          │ True      │ True     │ 1885          │ 71                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult1  │ list         │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult1  │ dlist        │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult1  │ vector       │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult25 │ set          │ True      │ True     │ 1959          │ 25                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult25 │ list         │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult25 │ dlist        │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult25 │ vector       │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ fiendish    │ set          │ True      │ True     │ 1994          │ 36                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ fiendish    │ list         │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ fiendish    │ dlist        │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ fiendish    │ vector       │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ euler       │ set          │ True      │ True     │ 1219          │ 711                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ euler       │ list         │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ euler       │ dlist        │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ euler       │ vector       │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ easy        │ set          │ True      │ True     │ 1181          │ 665                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ easy        │ list         │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ easy        │ dlist        │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ easy        │ vector       │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ mild        │ set          │ True      │ True     │ 1200          │ 742                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ mild        │ list         │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ mild        │ dlist        │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ mild        │ vector       │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ difficult1  │ set          │ True      │ True     │ 1439          │ 532                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ difficult1  │ list         │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ difficult1  │ dlist        │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ difficult1  │ vector       │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ difficult25 │ set          │ True      │ True     │ 1907          │ 75                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ difficult25 │ list         │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ difficult25 │ dlist        │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ difficult25 │ vector       │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ fiendish    │ set          │ True      │ True     │ 1947          │ 103                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ fiendish    │ list         │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ fiendish    │ dlist        │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ fiendish    │ vector       │ True      │ True     │ 2061          │ 17                 │
└─────────────────────────────┴─────────────┴──────────────┴───────────┴──────────┴───────────────┴────────────────────┘
benchmarking LSWSolver/euler/set
time                 37.69 ms   (37.49 ms .. 37.90 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 37.65 ms   (37.52 ms .. 37.96 ms)
std dev              392.1 μs   (209.1 μs .. 655.4 μs)

benchmarking LSWSolver/euler/list
time                 32.15 ms   (26.71 ms .. 37.64 ms)
                     0.944 R²   (0.912 R² .. 1.000 R²)
mean                 28.75 ms   (27.62 ms .. 31.29 ms)
std dev              3.139 ms   (1.340 ms .. 5.109 ms)
variance introduced by outliers: 45% (moderately inflated)

benchmarking LSWSolver/euler/dlist
time                 34.40 ms   (30.71 ms .. 39.14 ms)
                     0.962 R²   (0.936 R² .. 0.999 R²)
mean                 32.18 ms   (31.48 ms .. 34.17 ms)
std dev              2.321 ms   (956.8 μs .. 4.371 ms)
variance introduced by outliers: 28% (moderately inflated)

benchmarking LSWSolver/euler/vector
time                 6.870 ms   (6.789 ms .. 6.945 ms)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 6.965 ms   (6.896 ms .. 7.120 ms)
std dev              282.5 μs   (132.2 μs .. 513.8 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking LSWSolver/easy/set
time                 41.88 ms   (40.28 ms .. 42.84 ms)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 41.25 ms   (40.91 ms .. 41.67 ms)
std dev              737.2 μs   (500.6 μs .. 952.7 μs)

benchmarking LSWSolver/easy/list
time                 37.41 ms   (33.35 ms .. 46.64 ms)
                     0.847 R²   (0.639 R² .. 0.988 R²)
mean                 43.13 ms   (39.31 ms .. 48.65 ms)
std dev              9.294 ms   (5.771 ms .. 12.56 ms)
variance introduced by outliers: 72% (severely inflated)

benchmarking LSWSolver/easy/dlist
time                 39.93 ms   (38.56 ms .. 42.60 ms)
                     0.990 R²   (0.976 R² .. 0.998 R²)
mean                 40.67 ms   (39.97 ms .. 41.88 ms)
std dev              1.843 ms   (1.211 ms .. 2.776 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking LSWSolver/easy/vector
time                 6.771 ms   (6.728 ms .. 6.815 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 6.834 ms   (6.798 ms .. 6.880 ms)
std dev              111.4 μs   (90.61 μs .. 137.0 μs)

benchmarking LSWSolver/mild/set
time                 39.89 ms   (39.52 ms .. 40.75 ms)
                     0.999 R²   (0.995 R² .. 1.000 R²)
mean                 39.90 ms   (39.52 ms .. 40.44 ms)
std dev              922.3 μs   (455.7 μs .. 1.399 ms)

benchmarking LSWSolver/mild/list
time                 29.20 ms   (28.79 ms .. 29.74 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 29.07 ms   (28.87 ms .. 29.29 ms)
std dev              465.3 μs   (368.8 μs .. 607.7 μs)

benchmarking LSWSolver/mild/dlist
time                 31.67 ms   (31.31 ms .. 32.00 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 32.27 ms   (31.94 ms .. 32.82 ms)
std dev              887.9 μs   (521.8 μs .. 1.438 ms)

benchmarking LSWSolver/mild/vector
time                 6.965 ms   (6.881 ms .. 7.054 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 6.980 ms   (6.930 ms .. 7.076 ms)
std dev              201.4 μs   (112.7 μs .. 347.5 μs)
variance introduced by outliers: 10% (moderately inflated)

benchmarking LSWSolver/difficult1/set
time                 43.03 ms   (42.31 ms .. 43.66 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 43.33 ms   (42.96 ms .. 44.45 ms)
std dev              1.158 ms   (385.9 μs .. 2.019 ms)

benchmarking LSWSolver/difficult1/list
time                 26.10 ms   (25.65 ms .. 26.60 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 25.80 ms   (25.61 ms .. 26.02 ms)
std dev              466.2 μs   (337.5 μs .. 644.8 μs)

benchmarking LSWSolver/difficult1/dlist
time                 28.56 ms   (28.35 ms .. 28.75 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 28.66 ms   (28.48 ms .. 29.04 ms)
std dev              553.6 μs   (231.0 μs .. 1.010 ms)

benchmarking LSWSolver/difficult1/vector
time                 6.945 ms   (6.828 ms .. 7.098 ms)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 6.973 ms   (6.926 ms .. 7.062 ms)
std dev              186.3 μs   (97.73 μs .. 355.0 μs)
variance introduced by outliers: 10% (moderately inflated)

benchmarking LSWSolver/difficult25/set
time                 44.21 ms   (42.48 ms .. 46.42 ms)
                     0.996 R²   (0.993 R² .. 1.000 R²)
mean                 41.92 ms   (41.45 ms .. 42.90 ms)
std dev              1.304 ms   (698.3 μs .. 2.147 ms)

benchmarking LSWSolver/difficult25/list
time                 32.70 ms   (31.17 ms .. 34.26 ms)
                     0.994 R²   (0.989 R² .. 0.998 R²)
mean                 30.21 ms   (29.38 ms .. 31.06 ms)
std dev              1.851 ms   (1.378 ms .. 2.462 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking LSWSolver/difficult25/dlist
time                 37.19 ms   (35.61 ms .. 38.82 ms)
                     0.995 R²   (0.992 R² .. 0.999 R²)
mean                 36.50 ms   (35.98 ms .. 37.19 ms)
std dev              1.255 ms   (862.1 μs .. 1.634 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking LSWSolver/difficult25/vector
time                 9.277 ms   (8.934 ms .. 9.786 ms)
                     0.986 R²   (0.973 R² .. 0.996 R²)
mean                 9.458 ms   (9.289 ms .. 9.695 ms)
std dev              556.5 μs   (390.3 μs .. 775.2 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarking LSWSolver/fiendish/set
time                 48.12 ms   (45.34 ms .. 50.80 ms)
                     0.995 R²   (0.993 R² .. 0.998 R²)
mean                 54.07 ms   (52.32 ms .. 55.64 ms)
std dev              3.159 ms   (2.426 ms .. 4.164 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking LSWSolver/fiendish/list
time                 24.05 ms   (22.92 ms .. 26.56 ms)
                     0.942 R²   (0.842 R² .. 0.999 R²)
mean                 24.66 ms   (23.80 ms .. 26.92 ms)
std dev              2.850 ms   (1.177 ms .. 5.142 ms)
variance introduced by outliers: 51% (severely inflated)

benchmarking LSWSolver/fiendish/dlist
time                 27.87 ms   (25.74 ms .. 32.00 ms)
                     0.947 R²   (0.874 R² .. 0.998 R²)
mean                 28.11 ms   (27.31 ms .. 30.57 ms)
std dev              2.713 ms   (950.8 μs .. 4.895 ms)
variance introduced by outliers: 43% (moderately inflated)

benchmarking LSWSolver/fiendish/vector
time                 7.213 ms   (7.171 ms .. 7.252 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 7.245 ms   (7.206 ms .. 7.324 ms)
std dev              154.2 μs   (75.45 μs .. 273.4 μs)

benchmarking PartialApplicationLSWSolver/euler/set
time                 26.12 ms   (25.86 ms .. 26.39 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 26.31 ms   (26.18 ms .. 26.48 ms)
std dev              333.6 μs   (262.1 μs .. 429.8 μs)

benchmarking PartialApplicationLSWSolver/euler/list
time                 8.541 ms   (8.463 ms .. 8.601 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 8.498 ms   (8.438 ms .. 8.566 ms)
std dev              179.0 μs   (133.3 μs .. 250.2 μs)

benchmarking PartialApplicationLSWSolver/euler/dlist
time                 8.892 ms   (8.817 ms .. 8.969 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 8.927 ms   (8.876 ms .. 8.982 ms)
std dev              150.9 μs   (120.5 μs .. 203.3 μs)

benchmarking PartialApplicationLSWSolver/euler/vector
time                 2.501 ms   (2.479 ms .. 2.521 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.489 ms   (2.477 ms .. 2.509 ms)
std dev              51.53 μs   (34.83 μs .. 88.36 μs)

benchmarking PartialApplicationLSWSolver/easy/set
time                 30.80 ms   (30.49 ms .. 31.09 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 31.00 ms   (30.66 ms .. 32.18 ms)
std dev              1.253 ms   (233.3 μs .. 2.433 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking PartialApplicationLSWSolver/easy/list
time                 10.01 ms   (9.935 ms .. 10.11 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 9.981 ms   (9.934 ms .. 10.04 ms)
std dev              151.6 μs   (109.1 μs .. 195.1 μs)

benchmarking PartialApplicationLSWSolver/easy/dlist
time                 10.89 ms   (10.75 ms .. 11.08 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 10.81 ms   (10.76 ms .. 10.89 ms)
std dev              180.9 μs   (137.6 μs .. 239.3 μs)

benchmarking PartialApplicationLSWSolver/easy/vector
time                 2.708 ms   (2.600 ms .. 2.811 ms)
                     0.994 R²   (0.990 R² .. 1.000 R²)
mean                 2.617 ms   (2.599 ms .. 2.655 ms)
std dev              83.83 μs   (53.06 μs .. 147.4 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking PartialApplicationLSWSolver/mild/set
time                 28.45 ms   (27.28 ms .. 29.29 ms)
                     0.994 R²   (0.987 R² .. 0.998 R²)
mean                 30.03 ms   (29.35 ms .. 30.93 ms)
std dev              1.721 ms   (1.190 ms .. 2.438 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking PartialApplicationLSWSolver/mild/list
time                 9.031 ms   (8.954 ms .. 9.099 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 8.997 ms   (8.961 ms .. 9.031 ms)
std dev              97.34 μs   (77.63 μs .. 126.1 μs)

benchmarking PartialApplicationLSWSolver/mild/dlist
time                 9.817 ms   (9.636 ms .. 10.07 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 9.644 ms   (9.572 ms .. 9.743 ms)
std dev              235.0 μs   (167.9 μs .. 337.3 μs)

benchmarking PartialApplicationLSWSolver/mild/vector
time                 2.493 ms   (2.427 ms .. 2.545 ms)
                     0.989 R²   (0.979 R² .. 0.996 R²)
mean                 2.843 ms   (2.732 ms .. 3.054 ms)
std dev              474.1 μs   (323.9 μs .. 684.9 μs)
variance introduced by outliers: 86% (severely inflated)

benchmarking PartialApplicationLSWSolver/difficult1/set
time                 22.97 ms   (22.71 ms .. 23.33 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 23.07 ms   (22.96 ms .. 23.17 ms)
std dev              248.9 μs   (208.6 μs .. 310.6 μs)

benchmarking PartialApplicationLSWSolver/difficult1/list
time                 7.731 ms   (7.681 ms .. 7.780 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.774 ms   (7.749 ms .. 7.811 ms)
std dev              87.63 μs   (70.52 μs .. 116.0 μs)

benchmarking PartialApplicationLSWSolver/difficult1/dlist
time                 9.983 ms   (9.351 ms .. 10.96 ms)
                     0.969 R²   (0.949 R² .. 0.991 R²)
mean                 8.867 ms   (8.572 ms .. 9.277 ms)
std dev              946.7 μs   (637.1 μs .. 1.314 ms)
variance introduced by outliers: 57% (severely inflated)

benchmarking PartialApplicationLSWSolver/difficult1/vector
time                 2.748 ms   (2.712 ms .. 2.782 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.725 ms   (2.709 ms .. 2.744 ms)
std dev              58.17 μs   (48.04 μs .. 76.20 μs)

benchmarking PartialApplicationLSWSolver/difficult25/set
time                 23.64 ms   (23.00 ms .. 24.55 ms)
                     0.995 R²   (0.988 R² .. 0.999 R²)
mean                 24.62 ms   (24.28 ms .. 25.01 ms)
std dev              812.3 μs   (591.2 μs .. 1.102 ms)

benchmarking PartialApplicationLSWSolver/difficult25/list
time                 8.110 ms   (8.056 ms .. 8.170 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 7.940 ms   (7.871 ms .. 7.996 ms)
std dev              167.3 μs   (133.9 μs .. 215.6 μs)

benchmarking PartialApplicationLSWSolver/difficult25/dlist
time                 8.493 ms   (8.441 ms .. 8.555 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 8.457 ms   (8.404 ms .. 8.505 ms)
std dev              145.5 μs   (106.0 μs .. 201.0 μs)

benchmarking PartialApplicationLSWSolver/difficult25/vector
time                 2.596 ms   (2.573 ms .. 2.624 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.586 ms   (2.574 ms .. 2.601 ms)
std dev              43.60 μs   (35.63 μs .. 54.88 μs)

benchmarking PartialApplicationLSWSolver/fiendish/set
time                 23.14 ms   (21.86 ms .. 24.91 ms)
                     0.985 R²   (0.969 R² .. 0.999 R²)
mean                 22.36 ms   (21.98 ms .. 23.04 ms)
std dev              1.164 ms   (635.4 μs .. 1.930 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking PartialApplicationLSWSolver/fiendish/list
time                 7.769 ms   (7.667 ms .. 7.879 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 7.749 ms   (7.711 ms .. 7.795 ms)
std dev              122.7 μs   (89.61 μs .. 166.1 μs)

benchmarking PartialApplicationLSWSolver/fiendish/dlist
time                 8.186 ms   (8.076 ms .. 8.311 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 8.322 ms   (8.214 ms .. 8.461 ms)
std dev              335.8 μs   (235.2 μs .. 455.0 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking PartialApplicationLSWSolver/fiendish/vector
time                 2.629 ms   (2.612 ms .. 2.652 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.635 ms   (2.623 ms .. 2.654 ms)
std dev              49.94 μs   (34.02 μs .. 83.00 μs)

benchmarking SafeLSWSolver/euler/set
time                 27.37 ms   (27.06 ms .. 27.66 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 27.62 ms   (27.46 ms .. 28.02 ms)
std dev              468.2 μs   (247.9 μs .. 877.6 μs)

benchmarking SafeLSWSolver/euler/list
time                 8.343 ms   (8.293 ms .. 8.399 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 8.347 ms   (8.311 ms .. 8.422 ms)
std dev              146.5 μs   (79.68 μs .. 260.8 μs)

benchmarking SafeLSWSolver/euler/dlist
time                 8.961 ms   (8.799 ms .. 9.240 ms)
                     0.994 R²   (0.986 R² .. 0.999 R²)
mean                 9.077 ms   (8.961 ms .. 9.268 ms)
std dev              410.7 μs   (279.5 μs .. 607.4 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking SafeLSWSolver/euler/vector
time                 2.410 ms   (2.391 ms .. 2.429 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.419 ms   (2.405 ms .. 2.446 ms)
std dev              65.40 μs   (39.81 μs .. 116.4 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking SafeLSWSolver/easy/set
time                 25.45 ms   (25.27 ms .. 25.61 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 25.31 ms   (25.24 ms .. 25.38 ms)
std dev              169.0 μs   (127.2 μs .. 229.8 μs)

benchmarking SafeLSWSolver/easy/list
time                 10.17 ms   (10.01 ms .. 10.37 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 10.14 ms   (10.08 ms .. 10.22 ms)
std dev              179.9 μs   (136.9 μs .. 247.1 μs)

benchmarking SafeLSWSolver/easy/dlist
time                 10.87 ms   (10.74 ms .. 11.01 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 10.75 ms   (10.66 ms .. 10.83 ms)
std dev              233.0 μs   (170.8 μs .. 320.2 μs)

benchmarking SafeLSWSolver/easy/vector
time                 2.554 ms   (2.522 ms .. 2.600 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 2.551 ms   (2.537 ms .. 2.580 ms)
std dev              63.71 μs   (40.63 μs .. 111.2 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking SafeLSWSolver/mild/set
time                 28.70 ms   (28.48 ms .. 29.00 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 28.68 ms   (28.60 ms .. 28.81 ms)
std dev              225.4 μs   (170.3 μs .. 302.5 μs)

benchmarking SafeLSWSolver/mild/list
time                 8.997 ms   (8.909 ms .. 9.150 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 8.993 ms   (8.951 ms .. 9.047 ms)
std dev              129.4 μs   (94.61 μs .. 207.9 μs)

benchmarking SafeLSWSolver/mild/dlist
time                 9.394 ms   (9.324 ms .. 9.484 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 9.473 ms   (9.420 ms .. 9.551 ms)
std dev              177.5 μs   (132.0 μs .. 286.0 μs)

benchmarking SafeLSWSolver/mild/vector
time                 2.510 ms   (2.493 ms .. 2.537 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.525 ms   (2.513 ms .. 2.545 ms)
std dev              51.19 μs   (36.55 μs .. 79.44 μs)

benchmarking SafeLSWSolver/difficult1/set
time                 29.62 ms   (29.44 ms .. 29.88 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 29.62 ms   (29.51 ms .. 29.72 ms)
std dev              214.1 μs   (157.8 μs .. 301.7 μs)

benchmarking SafeLSWSolver/difficult1/list
time                 7.935 ms   (7.846 ms .. 8.026 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 8.049 ms   (7.988 ms .. 8.123 ms)
std dev              184.8 μs   (142.8 μs .. 261.0 μs)

benchmarking SafeLSWSolver/difficult1/dlist
time                 8.887 ms   (8.649 ms .. 9.284 ms)
                     0.991 R²   (0.984 R² .. 0.996 R²)
mean                 8.723 ms   (8.578 ms .. 8.893 ms)
std dev              443.0 μs   (321.0 μs .. 550.1 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking SafeLSWSolver/difficult1/vector
time                 2.491 ms   (2.465 ms .. 2.527 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 2.589 ms   (2.566 ms .. 2.622 ms)
std dev              92.81 μs   (70.69 μs .. 121.0 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking SafeLSWSolver/difficult25/set
time                 27.08 ms   (26.88 ms .. 27.25 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 27.04 ms   (26.93 ms .. 27.15 ms)
std dev              224.2 μs   (182.7 μs .. 295.6 μs)

benchmarking SafeLSWSolver/difficult25/list
time                 7.918 ms   (7.849 ms .. 8.014 ms)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 8.021 ms   (7.972 ms .. 8.140 ms)
std dev              206.3 μs   (121.9 μs .. 364.2 μs)

benchmarking SafeLSWSolver/difficult25/dlist
time                 8.424 ms   (8.345 ms .. 8.511 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 8.373 ms   (8.324 ms .. 8.429 ms)
std dev              145.3 μs   (108.8 μs .. 204.9 μs)

benchmarking SafeLSWSolver/difficult25/vector
time                 2.482 ms   (2.465 ms .. 2.502 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.498 ms   (2.485 ms .. 2.519 ms)
std dev              55.29 μs   (38.46 μs .. 78.90 μs)

benchmarking SafeLSWSolver/fiendish/set
time                 30.76 ms   (30.51 ms .. 30.95 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 30.76 ms   (30.63 ms .. 30.87 ms)
std dev              263.2 μs   (197.4 μs .. 363.5 μs)

benchmarking SafeLSWSolver/fiendish/list
time                 7.759 ms   (7.698 ms .. 7.820 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 7.791 ms   (7.760 ms .. 7.849 ms)
std dev              117.4 μs   (72.68 μs .. 214.9 μs)

benchmarking SafeLSWSolver/fiendish/dlist
time                 8.147 ms   (8.069 ms .. 8.205 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 8.173 ms   (8.127 ms .. 8.242 ms)
std dev              151.7 μs   (111.2 μs .. 234.6 μs)

benchmarking SafeLSWSolver/fiendish/vector
time                 2.552 ms   (2.529 ms .. 2.576 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 2.595 ms   (2.575 ms .. 2.637 ms)
std dev              87.10 μs   (55.15 μs .. 153.8 μs)
variance introduced by outliers: 18% (moderately inflated)
```
