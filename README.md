# advent-of-code

These are my solutions for the [advent of code](https://adventofcode.com/) using a different programming language each year.
- for [2017](#2017) I have used [Haskell](https://www.haskell.org/)
- for [2016](#2016) I have used [Rust](https://www.rust-lang.org/)
- for [2015](#2015) I have used [Julia](https://julialang.org/)

Most algorithms complete very fast. Those that do not are marked. *slow* algorithms take more than ten seconds to complete. *very slow* algorithms take several minutes to complete.

## 2017

Some solutions require the `vector` library. To install it run `cabal install --lib vector`.

To run a solution: `cd 2017 && sh run <filename>`

| Day | Solution #1                    | Solution #2                           |
|-----|--------------------------------|---------------------------------------|
| #01 | [2017/01/1](2017/2017-01-1.hs) | [2017/01/2](2017/2017-01-2.hs)        |
| #02 | [2017/02/1](2017/2017-02-1.hs) | [2017/02/2](2017/2017-02-2.hs)        |
| #03 | [2017/03/1](2017/2017-03-1.hs) | [2017/03/2](2017/2017-03-2.hs)        |
| #04 | [2017/04/1](2017/2017-04-1.hs) | [2017/04/2](2017/2017-04-2.hs)        |
| #05 | [2017/05/1](2017/2017-05-1.hs) | [2017/05/2](2017/2017-05-2.hs) (slow) |
| #06 | [2017/06/1](2017/2017-06-1.hs) | [2017/06/2](2017/2017-06-2.hs)        |
| #07 | [2017/07/1](2017/2017-07-1.hs) | (coming soon)                         |

## 2016

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
