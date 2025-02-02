# advent-of-code

These are my solutions for the [advent of code](https://adventofcode.com/) using a different programming language each year.
- for [2017](#2017) I have used [Haskell](https://www.haskell.org/)
- for [2016](#2016) I have used [Rust](https://www.rust-lang.org/)
- for [2015](#2015) I have used [Julia](https://julialang.org/)

Most algorithms complete very fast. Those that do not are marked. *slow* algorithms take more than ten seconds to complete. *very slow* algorithms take several minutes to complete.

## 2017

For my third year of Advent of Code, I chose [Haskell](https://www.haskell.org/), the undisputed king of functional programming. Haskell is as challenging as it is beautiful, with its strict purity and laziness pushing me to approach problems in fundamentally different ways. Unlike imperative languages, where one can fall back on mutable state and loops, Haskell demands a deep understanding of recursion, higher-order functions, and immutability, making it both a mental workout and a rewarding experience.

One of the biggest takeaways from using Haskell for AoC was the need to carefully consider data structures. The choice between lists, vectors, and sequences was not just a matter of performance but also about leveraging the right abstractions for the problem at hand. Haskell’s lazy evaluation allowed for elegant, declarative solutions, but also introduced challenges in reasoning about efficiency. Perhaps most strikingly, Haskell opened completely new perspectives on recursion, showing me how to express solutions in a way that feels natural in a functional paradigm. Of all the languages I’ve used for AoC so far, Haskell has been the most pleasant to write in - its expressiveness and mathematical elegance make every solution feel like a carefully crafted piece of art.

Some solutions use the vector library. Install it as needed:
- `cabal install --lib vector`

Haskell’s standard library is minimal by design, excluding constructs considered standard in other languages. Sticking solely to the standard library can lead to severe performance issues or require implementing complex structures far beyond the scope of these puzzles. Thus, I have deliberately opted to use this extra library where appropriate.

To run a solution: `cd 2017 && sh run <filename>`

| Day | Solution #1                    | Solution #2                                | Extra libs used |
|-----|--------------------------------|--------------------------------------------|-----------------|
| #01 | [2017/01/1](2017/2017-01-1.hs) | [2017/01/2](2017/2017-01-2.hs)             |                 |
| #02 | [2017/02/1](2017/2017-02-1.hs) | [2017/02/2](2017/2017-02-2.hs)             |                 |
| #03 | [2017/03/1](2017/2017-03-1.hs) | [2017/03/2](2017/2017-03-2.hs)             |                 |
| #04 | [2017/04/1](2017/2017-04-1.hs) | [2017/04/2](2017/2017-04-2.hs)             |                 |
| #05 | [2017/05/1](2017/2017-05-1.hs) | [2017/05/2](2017/2017-05-2.hs) (slow)      | `Data.Vector`   |
| #06 | [2017/06/1](2017/2017-06-1.hs) | [2017/06/2](2017/2017-06-2.hs)             | `Data.Vector`   |
| #07 | [2017/07/1](2017/2017-07-1.hs) | [2017/07/2](2017/2017-07-2.hs)             |                 |
| #08 | [2017/08/1](2017/2017-08-1.hs) | [2017/08/2](2017/2017-08-2.hs)             |                 |
| #09 | [2017/09/1](2017/2017-09-1.hs) | [2017/09/2](2017/2017-09-2.hs)             |                 |
| #10 | [2017/10/1](2017/2017-10-1.hs) | [2017/10/2](2017/2017-10-2.hs)             |                 |
| #11 | [2017/11/1](2017/2017-11-1.hs) | [2017/11/2](2017/2017-11-2.hs)             |                 |
| #12 | [2017/12/1](2017/2017-12-1.hs) | [2017/12/2](2017/2017-12-2.hs)             |                 |
| #13 | [2017/13/1](2017/2017-13-1.hs) | [2017/13/2](2017/2017-13-2.hs) (very slow) |                 |
| #14 | [2017/14/1](2017/2017-14-1.hs) | [2017/14/2](2017/2017-14-2.hs)             |                 |
| #15 | [2017/15/1](2017/2017-15-1.hs) | [2017/15/2](2017/2017-15-2.hs)             |                 |
| #16 | [2017/16/1](2017/2017-16-1.hs) | [2017/16/2](2017/2017-16-2.hs)             |                 |
| #17 | [2017/17/1](2017/2017-17-1.hs) | [2017/17/2](2017/2017-17-2.hs) (very slow) |                 |
| #18 | [2017/18/1](2017/2017-18-1.hs) | [2017/18/2](2017/2017-18-2.hs)             |                 |
| #19 | [2017/19/1](2017/2017-19-1.hs) | [2017/19/2](2017/2017-19-2.hs)             |                 |

## 2016

For my second year of Advent of Code 2016, I chose [Rust](https://www.rust-lang.org/), a language that offers a unique blend of performance, safety, and modern functional programming features. One of the most striking aspects of Rust is its ownership model, which enforces memory safety at compile time without needing a garbage collector. This paradigm was a fascinating experience and challenged me to think more deeply about resource management, a skill that translates well to systems programming and beyond.

Rust’s immutable-first approach naturally encourages safe and predictable code, reducing unintended side effects. The absence of null eliminates an entire class of runtime errors, making solutions more robust. Additionally, Rust provides many modern functional programming concepts, such as pattern matching, iterators, and algebraic data types, which align well with the kind of problem-solving AoC requires. At the same time, its structs and traits offered a great experience for someone like me with a strong object-oriented programming background, allowing for well-structured and modular code. While Rust has a reputation for being complex, I found it deeply rewarding, as it forced me to write precise and efficient solutions while learning valuable concepts along the way.

To run a solution: `cd 2016 && cargo build && ./target/debug/advent_of_code <puzzle-num>`

"puzzle-num" equals the text in the table below, e.g. "2016/01/1" for Solution #1 on day one.

| Day | Solution #1                           | Solution #2                                |
|-----|---------------------------------------|--------------------------------------------|
| #01 | [2016/01/1](2016/2016-01-1.rs)        | [2016/01/2](2016/2016-01-2.rs)             |
| #02 | [2016/02/1](2016/2016-02-1.rs)        | [2016/02/2](2016/2016-02-2.rs)             |
| #03 | [2016/03/1](2016/2016-03-1.rs)        | [2016/03/2](2016/2016-03-2.rs)             |
| #04 | [2016/04/1](2016/2016-04-1.rs)        | [2016/04/2](2016/2016-04-2.rs)             |
| #05 | [2016/05/1](2016/2016-05-1.rs) (slow) | [2016/05/2](2016/2016-05-2.rs) (very slow) |
| #06 | [2016/06/1](2016/2016-06-1.rs)        | [2016/06/2](2016/2016-06-2.rs)             |
| #07 | [2016/07/1](2016/2016-07-1.rs)        | [2016/07/2](2016/2016-07-2.rs)             |
| #08 | [2016/08/1](2016/2016-08-1.rs)        | [2016/08/2](2016/2016-08-2.rs)             |
| #09 | [2016/09/1](2016/2016-09-1.rs)        | [2016/09/2](2016/2016-09-2.rs)             |
| #10 | [2016/10/1](2016/2016-10-1.rs)        | [2016/10/2](2016/2016-10-2.rs)             |
| #11 | [2016/11/1](2016/2016-11-1.rs)        | [2016/11/2](2016/2016-11-2.rs)             |
| #12 | [2016/12/1](2016/2016-12-1.rs)        | [2016/12/2](2016/2016-12-2.rs)             |
| #13 | [2016/13/1](2016/2016-13-1.rs)        | [2016/13/2](2016/2016-13-2.rs)             |
| #14 | [2016/14/1](2016/2016-14-1.rs)        | [2016/14/2](2016/2016-14-2.rs) (slow)      |
| #15 | [2016/15/1](2016/2016-15-1.rs)        | [2016/15/2](2016/2016-15-2.rs)             |
| #16 | [2016/16/1](2016/2016-16-1.rs)        | [2016/16/2](2016/2016-16-2.rs)             |
| #17 | [2016/17/1](2016/2016-17-1.rs)        | [2016/17/2](2016/2016-17-2.rs)             |
| #18 | [2016/18/1](2016/2016-18-1.rs)        | [2016/18/2](2016/2016-18-2.rs)             |
| #19 | [2016/19/1](2016/2016-19-1.rs)        | [2016/19/2](2016/2016-19-2.rs) (very slow) |
| #20 | [2016/20/1](2016/2016-20-1.rs)        | [2016/20/2](2016/2016-20-2.rs) (slow)      |
| #21 | [2016/21/1](2016/2016-21-1.rs)        | [2016/21/2](2016/2016-21-2.rs)             |
| #22 | [2016/22/1](2016/2016-22-1.rs)        | [2016/22/2](2016/2016-22-2.rs)             |
| #23 | [2016/23/1](2016/2016-23-1.rs)        | [2016/23/2](2016/2016-23-2.rs)             |
| #24 | [2016/24/1](2016/2016-24-1.rs) (slow) | [2016/24/2](2016/2016-24-2.rs) (slow)      |
| #25 | [2016/25/1](2016/2016-25-1.rs)        | [2016/25/2](2016/2016-25-2.rs)             |

## 2015

For my first year of Advent of Code, I chose [Julia](https://julialang.org/), a language that strikes a balance between expressiveness and performance, making it an excellent choice for quickly prototyping and iterating on solutions. Julia’s dynamic and imperative nature allows for an easy start, especially in the context of coding challenges where flexibility is key. The readable, non-verbose syntax - reminiscent of Python, a language I am well acquainted with - made it a comfortable transition.

One of Julia’s standout features is its powerful support for matrices, which proved useful when handling two-dimensional data structures commonly found in AoC problems. The 1-based indexing reduces the risk of off-by-one errors, a frequent pitfall in 0-based languages. Additionally, Julia’s native regular expression support facilitated quick and efficient text processing, which is often required for parsing input data. While Julia is primarily designed for high-performance numerical computing, it proved to be a surprisingly effective and ergonomic choice for the kind of algorithmic problem-solving that AoC entails.

To run a solution: `cd 2015 && julia <filename>`

| Day | Solution #1                                | Solution #2                                |
|-----|--------------------------------------------|--------------------------------------------|
| #01 | [2015/01/1](2015/2015-01-1.jl)             | [2015/01/2](2015/2015-01-2.jl)             |
| #02 | [2015/02/1](2015/2015-02-1.jl)             | [2015/02/2](2015/2015-02-2.jl)             |
| #03 | [2015/03/1](2015/2015-03-1.jl)             | [2015/03/2](2015/2015-03-2.jl)             |
| #04 | [2015/04/1](2015/2015-04-1.jl)             | [2015/04/2](2015/2015-04-2.jl)             |
| #05 | [2015/05/1](2015/2015-05-1.jl)             | [2015/05/2](2015/2015-05-2.jl)             |
| #06 | [2015/06/1](2015/2015-06-1.jl)             | [2015/06/2](2015/2015-06-2.jl)             |
| #07 | [2015/07/1](2015/2015-07-1.jl)             | [2015/07/2](2015/2015-07-2.jl)             |
| #08 | [2015/08/1](2015/2015-08-1.jl)             | [2015/08/2](2015/2015-08-2.jl)             |
| #09 | [2015/09/1](2015/2015-09-1.jl)             | [2015/09/2](2015/2015-09-2.jl)             |
| #10 | [2015/10/1](2015/2015-10-1.jl) (slow)      | [2015/10/2](2015/2015-10-2.jl) (very slow) |
| #11 | [2015/11/1](2015/2015-11-1.jl)             | [2015/11/2](2015/2015-11-2.jl)             |
| #12 | [2015/12/1](2015/2015-12-1.jl)             | [2015/12/2](2015/2015-12-2.jl)             |
| #13 | [2015/13/1](2015/2015-13-1.jl)             | [2015/13/2](2015/2015-13-2.jl)             |
| #14 | [2015/14/1](2015/2015-14-1.jl)             | [2015/14/2](2015/2015-14-2.jl)             |
| #15 | [2015/15/1](2015/2015-15-1.jl)             | [2015/15/2](2015/2015-15-2.jl)             |
| #16 | [2015/16/1](2015/2015-16-1.jl)             | [2015/16/2](2015/2015-16-2.jl)             |
| #17 | [2015/17/1](2015/2015-17-1.jl)             | [2015/17/2](2015/2015-17-2.jl)             |
| #18 | [2015/18/1](2015/2015-18-1.jl)             | [2015/18/2](2015/2015-18-2.jl)             |
| #19 | [2015/19/1](2015/2015-19-1.jl)             | [2015/19/2](2015/2015-19-2.jl)             |
| #20 | [2015/20/1](2015/2015-20-1.jl) (very slow) | [2015/20/2](2015/2015-20-2.jl) (very slow) |
| #21 | [2015/21/1](2015/2015-21-1.jl)             | [2015/21/2](2015/2015-21-2.jl)             |
| #22 | [2015/22/1](2015/2015-22-1.jl)             | [2015/22/2](2015/2015-22-2.jl)             |
| #23 | [2015/23/1](2015/2015-23-1.jl)             | [2015/23/2](2015/2015-23-2.jl)             |
| #24 | [2015/24/1](2015/2015-24-1.jl)             | [2015/24/2](2015/2015-24-2.jl)             |
| #25 | [2015/25/1](2015/2015-25-1.jl)             | [2015/25/2](2015/2015-25-2.jl)             |
