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
$ stack run -- --output benchmarks.html
┌─────────────────────────────┬─────────────┬──────────────┬───────────┬──────────┬───────────────┬────────────────────┐
│           Solver            │   Puzzle    │ MessageQueue │ Complete? │ Correct? │ Messages Used │ Messages Remaining │
╞═════════════════════════════╪═════════════╪══════════════╪═══════════╪══════════╪═══════════════╪════════════════════╡
│ LSWSolver                   │ euler       │ set          │ True      │ True     │ 1018          │ 771                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ euler       │ list         │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ euler       │ dlist        │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ easy        │ set          │ True      │ True     │ 1048          │ 640                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ easy        │ list         │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ easy        │ dlist        │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ mild        │ set          │ True      │ True     │ 1054          │ 701                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ mild        │ list         │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ mild        │ dlist        │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult1  │ set          │ True      │ True     │ 1426          │ 458                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult1  │ list         │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult1  │ dlist        │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult25 │ set          │ True      │ True     │ 1791          │ 123                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult25 │ list         │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult25 │ dlist        │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ fiendish    │ set          │ True      │ True     │ 1508          │ 460                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ fiendish    │ list         │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ fiendish    │ dlist        │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ euler       │ set          │ True      │ True     │ 1677          │ 203                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ euler       │ list         │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ euler       │ dlist        │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ easy        │ set          │ True      │ True     │ 1594          │ 298                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ easy        │ list         │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ easy        │ dlist        │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ mild        │ set          │ True      │ True     │ 1892          │ 44                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ mild        │ list         │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ mild        │ dlist        │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult1  │ set          │ True      │ True     │ 1885          │ 71                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult1  │ list         │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult1  │ dlist        │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult25 │ set          │ True      │ True     │ 1959          │ 25                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult25 │ list         │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult25 │ dlist        │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ fiendish    │ set          │ True      │ True     │ 1994          │ 36                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ fiendish    │ list         │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ fiendish    │ dlist        │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ euler       │ set          │ True      │ True     │ 1219          │ 711                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ euler       │ list         │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ euler       │ dlist        │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ easy        │ set          │ True      │ True     │ 1181          │ 665                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ easy        │ list         │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ easy        │ dlist        │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ mild        │ set          │ True      │ True     │ 1200          │ 742                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ mild        │ list         │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ mild        │ dlist        │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ difficult1  │ set          │ True      │ True     │ 1439          │ 532                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ difficult1  │ list         │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ difficult1  │ dlist        │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ difficult25 │ set          │ True      │ True     │ 1907          │ 75                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ difficult25 │ list         │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ difficult25 │ dlist        │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ fiendish    │ set          │ True      │ True     │ 1947          │ 103                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ fiendish    │ list         │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ SafeLSWSolver               │ fiendish    │ dlist        │ True      │ True     │ 2061          │ 17                 │
└─────────────────────────────┴─────────────┴──────────────┴───────────┴──────────┴───────────────┴────────────────────┘
benchmarking LSWSolver/euler/set
time                 44.81 ms   (44.06 ms .. 45.63 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 44.92 ms   (44.58 ms .. 45.29 ms)
std dev              685.9 μs   (492.9 μs .. 936.6 μs)

benchmarking LSWSolver/euler/list
time                 34.93 ms   (34.32 ms .. 36.56 ms)
                     0.994 R²   (0.978 R² .. 1.000 R²)
mean                 34.88 ms   (34.44 ms .. 36.07 ms)
std dev              1.362 ms   (425.2 μs .. 2.363 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking LSWSolver/euler/dlist
time                 40.61 ms   (39.93 ms .. 41.28 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 40.39 ms   (40.10 ms .. 40.76 ms)
std dev              678.8 μs   (460.8 μs .. 1.000 ms)

benchmarking LSWSolver/easy/set
time                 51.86 ms   (48.58 ms .. 55.33 ms)
                     0.992 R²   (0.982 R² .. 0.999 R²)
mean                 50.04 ms   (49.10 ms .. 51.38 ms)
std dev              2.144 ms   (1.482 ms .. 3.178 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking LSWSolver/easy/list
time                 46.22 ms   (44.27 ms .. 48.37 ms)
                     0.995 R²   (0.987 R² .. 0.999 R²)
mean                 47.02 ms   (46.11 ms .. 48.24 ms)
std dev              1.993 ms   (1.400 ms .. 3.010 ms)
variance introduced by outliers: 13% (moderately inflated)

benchmarking LSWSolver/easy/dlist
time                 53.43 ms   (52.31 ms .. 54.99 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 53.15 ms   (52.49 ms .. 53.84 ms)
std dev              1.253 ms   (957.6 μs .. 1.720 ms)

benchmarking LSWSolver/mild/set
time                 49.98 ms   (47.39 ms .. 52.76 ms)
                     0.992 R²   (0.982 R² .. 0.998 R²)
mean                 50.97 ms   (49.94 ms .. 52.22 ms)
std dev              2.295 ms   (1.726 ms .. 2.878 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking LSWSolver/mild/list
time                 38.65 ms   (37.44 ms .. 39.68 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 38.17 ms   (37.59 ms .. 38.87 ms)
std dev              1.301 ms   (827.7 μs .. 1.956 ms)

benchmarking LSWSolver/mild/dlist
time                 43.34 ms   (42.42 ms .. 44.33 ms)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 43.91 ms   (43.09 ms .. 46.98 ms)
std dev              2.851 ms   (700.4 μs .. 5.033 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking LSWSolver/difficult1/set
time                 56.46 ms   (54.12 ms .. 59.36 ms)
                     0.994 R²   (0.987 R² .. 0.998 R²)
mean                 55.69 ms   (54.52 ms .. 56.93 ms)
std dev              2.414 ms   (1.962 ms .. 3.047 ms)

benchmarking LSWSolver/difficult1/list
time                 35.17 ms   (34.10 ms .. 36.49 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 34.28 ms   (33.64 ms .. 34.86 ms)
std dev              1.235 ms   (991.7 μs .. 1.594 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking LSWSolver/difficult1/dlist
time                 36.73 ms   (35.71 ms .. 37.58 ms)
                     0.997 R²   (0.992 R² .. 0.999 R²)
mean                 37.87 ms   (37.21 ms .. 39.03 ms)
std dev              1.724 ms   (1.042 ms .. 2.712 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking LSWSolver/difficult25/set
time                 49.78 ms   (48.90 ms .. 50.80 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 50.39 ms   (49.87 ms .. 51.58 ms)
std dev              1.428 ms   (698.8 μs .. 2.356 ms)

benchmarking LSWSolver/difficult25/list
time                 31.39 ms   (30.80 ms .. 31.93 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 31.39 ms   (31.02 ms .. 32.14 ms)
std dev              1.096 ms   (556.2 μs .. 1.814 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking LSWSolver/difficult25/dlist
time                 36.28 ms   (35.58 ms .. 37.13 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 35.78 ms   (35.21 ms .. 36.32 ms)
std dev              1.119 ms   (736.2 μs .. 1.677 ms)

benchmarking LSWSolver/fiendish/set
time                 54.79 ms   (53.17 ms .. 55.75 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 57.78 ms   (56.75 ms .. 59.31 ms)
std dev              2.215 ms   (1.484 ms .. 3.049 ms)

benchmarking LSWSolver/fiendish/list
time                 28.56 ms   (28.13 ms .. 29.01 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 28.46 ms   (28.30 ms .. 28.68 ms)
std dev              381.0 μs   (299.1 μs .. 512.1 μs)

benchmarking LSWSolver/fiendish/dlist
time                 32.79 ms   (32.33 ms .. 33.22 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 33.10 ms   (32.82 ms .. 33.55 ms)
std dev              729.2 μs   (530.2 μs .. 1.104 ms)

benchmarking PartialApplicationLSWSolver/euler/set
time                 33.41 ms   (32.50 ms .. 34.43 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 32.34 ms   (31.93 ms .. 32.84 ms)
std dev              1.020 ms   (726.3 μs .. 1.377 ms)
variance introduced by outliers: 10% (moderately inflated)

benchmarking PartialApplicationLSWSolver/euler/list
time                 10.00 ms   (9.836 ms .. 10.17 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 10.26 ms   (10.15 ms .. 10.46 ms)
std dev              412.3 μs   (277.5 μs .. 635.5 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking PartialApplicationLSWSolver/euler/dlist
time                 10.78 ms   (10.57 ms .. 10.94 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 10.77 ms   (10.67 ms .. 10.88 ms)
std dev              278.3 μs   (199.9 μs .. 440.8 μs)

benchmarking PartialApplicationLSWSolver/easy/set
time                 41.45 ms   (40.60 ms .. 42.49 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 38.82 ms   (38.05 ms .. 39.56 ms)
std dev              1.605 ms   (1.367 ms .. 2.062 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking PartialApplicationLSWSolver/easy/list
time                 11.75 ms   (11.53 ms .. 11.89 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 12.47 ms   (12.27 ms .. 12.67 ms)
std dev              518.5 μs   (458.3 μs .. 599.2 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking PartialApplicationLSWSolver/easy/dlist
time                 13.03 ms   (12.91 ms .. 13.16 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 12.93 ms   (12.86 ms .. 13.03 ms)
std dev              205.9 μs   (155.7 μs .. 284.8 μs)

benchmarking PartialApplicationLSWSolver/mild/set
time                 34.39 ms   (33.87 ms .. 35.21 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 34.82 ms   (34.50 ms .. 35.25 ms)
std dev              741.3 μs   (541.2 μs .. 1.110 ms)

benchmarking PartialApplicationLSWSolver/mild/list
time                 10.69 ms   (10.56 ms .. 10.83 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 10.76 ms   (10.69 ms .. 10.95 ms)
std dev              289.0 μs   (149.8 μs .. 556.9 μs)

benchmarking PartialApplicationLSWSolver/mild/dlist
time                 11.34 ms   (11.18 ms .. 11.52 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 11.32 ms   (11.25 ms .. 11.41 ms)
std dev              207.3 μs   (155.5 μs .. 292.8 μs)

benchmarking PartialApplicationLSWSolver/difficult1/set
time                 28.63 ms   (28.18 ms .. 29.17 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 28.31 ms   (28.07 ms .. 28.52 ms)
std dev              492.2 μs   (322.3 μs .. 783.9 μs)

benchmarking PartialApplicationLSWSolver/difficult1/list
time                 9.807 ms   (9.632 ms .. 10.02 ms)
                     0.997 R²   (0.994 R² .. 0.998 R²)
mean                 9.971 ms   (9.827 ms .. 10.15 ms)
std dev              447.3 μs   (341.5 μs .. 581.5 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking PartialApplicationLSWSolver/difficult1/dlist
time                 9.917 ms   (9.709 ms .. 10.12 ms)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 10.51 ms   (10.37 ms .. 10.71 ms)
std dev              449.4 μs   (350.2 μs .. 645.9 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking PartialApplicationLSWSolver/difficult25/set
time                 28.77 ms   (28.35 ms .. 29.19 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 28.51 ms   (28.30 ms .. 28.73 ms)
std dev              455.5 μs   (345.1 μs .. 623.7 μs)

benchmarking PartialApplicationLSWSolver/difficult25/list
time                 9.507 ms   (9.395 ms .. 9.649 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 9.583 ms   (9.521 ms .. 9.654 ms)
std dev              177.3 μs   (143.9 μs .. 224.8 μs)

benchmarking PartialApplicationLSWSolver/difficult25/dlist
time                 9.967 ms   (9.836 ms .. 10.12 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 10.10 ms   (9.989 ms .. 10.30 ms)
std dev              388.3 μs   (174.1 μs .. 626.4 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking PartialApplicationLSWSolver/fiendish/set
time                 26.06 ms   (25.63 ms .. 26.44 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 26.38 ms   (26.15 ms .. 26.65 ms)
std dev              545.1 μs   (410.7 μs .. 793.9 μs)

benchmarking PartialApplicationLSWSolver/fiendish/list
time                 9.206 ms   (9.101 ms .. 9.317 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 9.309 ms   (9.244 ms .. 9.405 ms)
std dev              214.9 μs   (124.4 μs .. 323.4 μs)

benchmarking PartialApplicationLSWSolver/fiendish/dlist
time                 9.705 ms   (9.604 ms .. 9.833 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 9.748 ms   (9.689 ms .. 9.880 ms)
std dev              225.2 μs   (141.6 μs .. 357.3 μs)

benchmarking SafeLSWSolver/euler/set
time                 33.30 ms   (32.81 ms .. 33.80 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 32.90 ms   (32.64 ms .. 33.13 ms)
std dev              506.7 μs   (402.4 μs .. 646.1 μs)

benchmarking SafeLSWSolver/euler/list
time                 10.00 ms   (9.926 ms .. 10.08 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 10.05 ms   (9.983 ms .. 10.13 ms)
std dev              198.5 μs   (138.6 μs .. 281.0 μs)

benchmarking SafeLSWSolver/euler/dlist
time                 10.52 ms   (10.36 ms .. 10.65 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 10.58 ms   (10.50 ms .. 10.70 ms)
std dev              267.0 μs   (175.5 μs .. 463.8 μs)

benchmarking SafeLSWSolver/easy/set
time                 30.66 ms   (30.16 ms .. 31.08 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 30.41 ms   (30.15 ms .. 30.63 ms)
std dev              512.6 μs   (371.6 μs .. 767.7 μs)

benchmarking SafeLSWSolver/easy/list
time                 12.28 ms   (12.18 ms .. 12.42 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 12.16 ms   (12.08 ms .. 12.28 ms)
std dev              269.2 μs   (198.1 μs .. 380.9 μs)

benchmarking SafeLSWSolver/easy/dlist
time                 12.89 ms   (12.72 ms .. 13.07 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 12.94 ms   (12.85 ms .. 13.07 ms)
std dev              284.6 μs   (207.9 μs .. 426.6 μs)

benchmarking SafeLSWSolver/mild/set
time                 34.72 ms   (34.17 ms .. 35.30 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 34.38 ms   (34.04 ms .. 34.67 ms)
std dev              625.1 μs   (490.7 μs .. 893.1 μs)

benchmarking SafeLSWSolver/mild/list
time                 10.69 ms   (10.57 ms .. 10.83 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 10.63 ms   (10.56 ms .. 10.72 ms)
std dev              204.1 μs   (147.3 μs .. 290.4 μs)

benchmarking SafeLSWSolver/mild/dlist
time                 11.16 ms   (10.99 ms .. 11.37 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 11.14 ms   (11.07 ms .. 11.23 ms)
std dev              210.9 μs   (152.0 μs .. 289.2 μs)

benchmarking SafeLSWSolver/difficult1/set
time                 35.27 ms   (34.84 ms .. 35.69 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 35.80 ms   (35.48 ms .. 36.34 ms)
std dev              853.6 μs   (459.0 μs .. 1.364 ms)

benchmarking SafeLSWSolver/difficult1/list
time                 9.544 ms   (9.428 ms .. 9.724 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 9.588 ms   (9.531 ms .. 9.691 ms)
std dev              208.6 μs   (124.3 μs .. 353.3 μs)

benchmarking SafeLSWSolver/difficult1/dlist
time                 10.10 ms   (9.924 ms .. 10.24 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 10.18 ms   (10.08 ms .. 10.32 ms)
std dev              303.6 μs   (202.4 μs .. 434.0 μs)

benchmarking SafeLSWSolver/difficult25/set
time                 32.42 ms   (32.18 ms .. 32.68 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 32.54 ms   (32.31 ms .. 32.81 ms)
std dev              544.9 μs   (363.3 μs .. 822.9 μs)

benchmarking SafeLSWSolver/difficult25/list
time                 9.452 ms   (9.375 ms .. 9.561 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 9.510 ms   (9.448 ms .. 9.609 ms)
std dev              204.6 μs   (134.7 μs .. 335.7 μs)

benchmarking SafeLSWSolver/difficult25/dlist
time                 9.911 ms   (9.820 ms .. 10.01 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 9.967 ms   (9.917 ms .. 10.05 ms)
std dev              180.7 μs   (133.1 μs .. 249.5 μs)

benchmarking SafeLSWSolver/fiendish/set
time                 36.96 ms   (36.64 ms .. 37.25 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 36.86 ms   (36.59 ms .. 37.19 ms)
std dev              647.2 μs   (416.6 μs .. 1.026 ms)

benchmarking SafeLSWSolver/fiendish/list
time                 9.105 ms   (9.040 ms .. 9.196 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 9.192 ms   (9.139 ms .. 9.274 ms)
std dev              185.0 μs   (120.3 μs .. 261.3 μs)

benchmarking SafeLSWSolver/fiendish/dlist
time                 9.471 ms   (9.363 ms .. 9.586 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 9.720 ms   (9.602 ms .. 9.922 ms)
std dev              405.6 μs   (242.1 μs .. 623.0 μs)
variance introduced by outliers: 18% (moderately inflated)
```
