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
└─────────────────────────────┴─────────────┴──────────────┴───────────┴──────────┴───────────────┴────────────────────┘
benchmarking LSWSolver/euler/set
time                 45.32 ms   (44.70 ms .. 46.32 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 44.71 ms   (44.33 ms .. 45.16 ms)
std dev              833.2 μs   (636.3 μs .. 1.147 ms)

benchmarking LSWSolver/euler/list
time                 34.24 ms   (33.92 ms .. 34.61 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 34.70 ms   (34.38 ms .. 35.76 ms)
std dev              1.047 ms   (340.2 μs .. 2.018 ms)

benchmarking LSWSolver/euler/dlist
time                 39.47 ms   (38.83 ms .. 40.33 ms)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 39.54 ms   (39.10 ms .. 40.07 ms)
std dev              1.008 ms   (833.8 μs .. 1.350 ms)

benchmarking LSWSolver/easy/set
time                 47.78 ms   (47.13 ms .. 48.71 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 48.14 ms   (47.77 ms .. 49.32 ms)
std dev              1.075 ms   (371.0 μs .. 1.972 ms)

benchmarking LSWSolver/easy/list
time                 44.01 ms   (43.55 ms .. 44.54 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 44.00 ms   (43.66 ms .. 44.45 ms)
std dev              769.8 μs   (501.3 μs .. 1.251 ms)

benchmarking LSWSolver/easy/dlist
time                 51.29 ms   (50.39 ms .. 51.92 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 51.23 ms   (50.92 ms .. 51.55 ms)
std dev              604.0 μs   (484.0 μs .. 786.4 μs)

benchmarking LSWSolver/mild/set
time                 46.33 ms   (45.65 ms .. 46.87 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 46.75 ms   (46.46 ms .. 47.65 ms)
std dev              991.1 μs   (229.3 μs .. 1.756 ms)

benchmarking LSWSolver/mild/list
time                 36.67 ms   (35.91 ms .. 37.30 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 36.35 ms   (36.10 ms .. 36.68 ms)
std dev              610.6 μs   (417.8 μs .. 873.8 μs)

benchmarking LSWSolver/mild/dlist
time                 41.72 ms   (41.14 ms .. 42.53 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 42.29 ms   (41.87 ms .. 43.17 ms)
std dev              1.137 ms   (578.3 μs .. 1.882 ms)

benchmarking LSWSolver/difficult1/set
time                 49.62 ms   (48.63 ms .. 50.30 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 50.31 ms   (49.90 ms .. 51.35 ms)
std dev              1.185 ms   (452.2 μs .. 2.048 ms)

benchmarking LSWSolver/difficult1/list
time                 32.23 ms   (31.90 ms .. 32.74 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 32.78 ms   (32.44 ms .. 33.65 ms)
std dev              1.071 ms   (476.7 μs .. 1.837 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking LSWSolver/difficult1/dlist
time                 37.14 ms   (36.68 ms .. 37.68 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 36.96 ms   (36.53 ms .. 37.41 ms)
std dev              913.2 μs   (639.0 μs .. 1.239 ms)

benchmarking LSWSolver/difficult25/set
time                 48.61 ms   (47.84 ms .. 49.32 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 48.55 ms   (48.21 ms .. 48.89 ms)
std dev              667.9 μs   (532.8 μs .. 896.6 μs)

benchmarking LSWSolver/difficult25/list
time                 31.27 ms   (30.91 ms .. 31.65 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 31.30 ms   (30.97 ms .. 32.28 ms)
std dev              1.035 ms   (437.7 μs .. 1.792 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking LSWSolver/difficult25/dlist
time                 35.43 ms   (34.93 ms .. 35.99 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 35.73 ms   (35.34 ms .. 36.33 ms)
std dev              1.039 ms   (616.6 μs .. 1.678 ms)

benchmarking LSWSolver/fiendish/set
time                 54.69 ms   (54.25 ms .. 55.18 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 55.33 ms   (55.02 ms .. 55.83 ms)
std dev              732.9 μs   (434.1 μs .. 1.150 ms)

benchmarking LSWSolver/fiendish/list
time                 29.15 ms   (28.72 ms .. 29.49 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 29.39 ms   (29.18 ms .. 29.61 ms)
std dev              470.9 μs   (373.2 μs .. 648.9 μs)

benchmarking LSWSolver/fiendish/dlist
time                 34.04 ms   (33.63 ms .. 34.54 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 33.72 ms   (33.29 ms .. 34.04 ms)
std dev              756.1 μs   (547.8 μs .. 1.040 ms)

benchmarking PartialApplicationLSWSolver/euler/set
time                 31.36 ms   (30.87 ms .. 31.85 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 31.28 ms   (31.08 ms .. 31.64 ms)
std dev              549.9 μs   (331.9 μs .. 856.0 μs)

benchmarking PartialApplicationLSWSolver/euler/list
time                 10.22 ms   (10.05 ms .. 10.41 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 10.19 ms   (10.11 ms .. 10.35 ms)
std dev              302.1 μs   (155.7 μs .. 536.4 μs)

benchmarking PartialApplicationLSWSolver/euler/dlist
time                 10.66 ms   (10.55 ms .. 10.79 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 10.65 ms   (10.59 ms .. 10.75 ms)
std dev              200.7 μs   (145.7 μs .. 331.8 μs)

benchmarking PartialApplicationLSWSolver/easy/set
time                 36.99 ms   (36.64 ms .. 37.44 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 36.87 ms   (36.65 ms .. 37.10 ms)
std dev              463.9 μs   (349.2 μs .. 598.1 μs)

benchmarking PartialApplicationLSWSolver/easy/list
time                 12.30 ms   (12.08 ms .. 12.59 ms)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 12.41 ms   (12.33 ms .. 12.52 ms)
std dev              246.3 μs   (198.9 μs .. 301.5 μs)

benchmarking PartialApplicationLSWSolver/easy/dlist
time                 12.93 ms   (12.78 ms .. 13.12 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 12.95 ms   (12.85 ms .. 13.10 ms)
std dev              313.0 μs   (191.4 μs .. 493.9 μs)

benchmarking PartialApplicationLSWSolver/mild/set
time                 36.68 ms   (35.24 ms .. 38.28 ms)
                     0.993 R²   (0.985 R² .. 0.997 R²)
mean                 35.06 ms   (34.28 ms .. 36.05 ms)
std dev              1.827 ms   (1.526 ms .. 2.332 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking PartialApplicationLSWSolver/mild/list
time                 11.49 ms   (11.20 ms .. 11.92 ms)
                     0.992 R²   (0.986 R² .. 0.996 R²)
mean                 11.70 ms   (11.48 ms .. 11.96 ms)
std dev              596.9 μs   (495.6 μs .. 753.7 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking PartialApplicationLSWSolver/mild/dlist
time                 11.27 ms   (11.07 ms .. 11.49 ms)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 11.46 ms   (11.37 ms .. 11.57 ms)
std dev              261.9 μs   (224.4 μs .. 322.8 μs)

benchmarking PartialApplicationLSWSolver/difficult1/set
time                 27.80 ms   (27.40 ms .. 28.21 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 27.50 ms   (27.33 ms .. 27.70 ms)
std dev              425.0 μs   (333.3 μs .. 564.6 μs)

benchmarking PartialApplicationLSWSolver/difficult1/list
time                 9.837 ms   (9.668 ms .. 10.04 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 9.767 ms   (9.702 ms .. 9.879 ms)
std dev              231.8 μs   (158.3 μs .. 365.6 μs)

benchmarking PartialApplicationLSWSolver/difficult1/dlist
time                 10.27 ms   (10.13 ms .. 10.42 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 10.29 ms   (10.23 ms .. 10.53 ms)
std dev              290.0 μs   (108.9 μs .. 582.2 μs)

benchmarking PartialApplicationLSWSolver/difficult25/set
time                 27.61 ms   (27.23 ms .. 28.09 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 27.85 ms   (27.65 ms .. 28.20 ms)
std dev              574.9 μs   (357.4 μs .. 916.2 μs)

benchmarking PartialApplicationLSWSolver/difficult25/list
time                 9.618 ms   (9.544 ms .. 9.713 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 9.598 ms   (9.549 ms .. 9.700 ms)
std dev              173.1 μs   (99.32 μs .. 321.9 μs)

benchmarking PartialApplicationLSWSolver/difficult25/dlist
time                 10.08 ms   (9.967 ms .. 10.21 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 10.13 ms   (10.07 ms .. 10.22 ms)
std dev              193.1 μs   (147.1 μs .. 285.8 μs)

benchmarking PartialApplicationLSWSolver/fiendish/set
time                 25.82 ms   (25.57 ms .. 26.00 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 26.00 ms   (25.78 ms .. 26.23 ms)
std dev              501.3 μs   (371.7 μs .. 704.3 μs)

benchmarking PartialApplicationLSWSolver/fiendish/list
time                 9.405 ms   (9.321 ms .. 9.507 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 9.343 ms   (9.287 ms .. 9.415 ms)
std dev              173.1 μs   (124.5 μs .. 243.1 μs)

benchmarking PartialApplicationLSWSolver/fiendish/dlist
time                 9.656 ms   (9.553 ms .. 9.757 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 9.653 ms   (9.592 ms .. 9.709 ms)
std dev              160.2 μs   (124.3 μs .. 227.3 μs)
```

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
- It pays to measure! I would have thought that the `List` based `MessageQueue` would perform the worst, but looking at the [results](https://retnuh.github.com/haskell-sudoku/benchmarks.html) it is usually the best. And even though the `PartialApplicationLSWSolver` processes more messages, it is quite a bit faster than the `LSWSolver`. If I had to hazard a guess, it's probably due to the fact that it generates less intermediate "gargbage" data - i.e. less allocations.

## Performance Results

Here are the [Criterion](http://www.serpentine.com/criterion/) [benchmark results](https://retnuh.github.com/haskell-sudoku/benchmarks.html)

## Future stuff

I've done the first of these, and may try second; the rest aren't too likely (at least at the moment).

- ~~make a `MessageQueue` typeclass to see if different ways of gathering the messages can reduce the message count. It's essentially just a `Monoid` and I think a sorted list should see some improvements if the `IsValue` messages get delivered before other messages.~~
- Look at type families to deal with the issue of some of the partial functions. I'm interested in using Haskell to make clearly wrong things inexpressible rather than just something to avoid. **UPDATE** I got this working so far using just partial application of functions; see comment in thoughts above.
- Look at an implementation using the [Conduit](https://github.com/snoyberg/conduit) library. This would be a pretty different implementation paradigm (maybe) so could be interesting. Also looks like a very interesting library. **UPDATE** Having looked at this a little more it's not clear that it would actually be useful for an implementation - a bit of square peg/round hole going on.
- Also look at doing an implementation using [FRP](https://wiki.haskell.org/Functional_Reactive_Programming) using maybe [Reactive Banana](https://wiki.haskell.org/Reactive-banana) or [Reflex](https://github.com/reflex-frp/reflex).
- Possibly implement the addtional "contstrained by box" rule I've discovered but never implemented in the Clojure version. I don't know if this would solve the "hardest" puzzle or not, but it should be much easier to implement than a full backtracking/search based soultion.
- Implement a backtracking/search solution. Perhaps from scratch, rather than sitting on top of exisiting solution. Dunno.
- ~~Use [Criterion](http://www.serpentine.com/criterion/) to benchmark the various `Solver` + `MessageQueue` combos.~~

## Feedback welcome

I realize that this is quite unlikely to be looked at by anybody, but if anybody does look at it and has any questions, comments, feedback, suggestions or anything else, I'd love to hear it. Feel free to open a github issue or whatever.
