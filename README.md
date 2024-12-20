# queues

[![GitHub CI](https://github.com/awkward-squad/queues/workflows/Haskell-CI/badge.svg)](https://github.com/awkward-squad/queues/actions)
[![Hackage](https://img.shields.io/hackage/v/queues.svg)](https://hackage.haskell.org/package/queues)
[![Stackage LTS](https://stackage.org/package/queues/badge/lts)](https://www.stackage.org/lts/package/queues)
[![Stackage Nightly](https://stackage.org/package/queues/badge/nightly)](https://www.stackage.org/nightly/package/queues)
[![Dependencies](https://img.shields.io/hackage-deps/v/queues)](https://packdeps.haskellers.com/reverse/queues)

A Haskell library for list-based queues.

Featuring, in order of complexity:

- `Data.Steque`: spine-strict amortized queue.

  Naive queue made out of two lists. Constant back insert time, however front access
  occasionally takes linear time to run.

- `Data.Queue`: spine-strict real-time queue.

  Ensures constant-time back insert and front access, at the cost of being slightly
  slower on aggregate than `Steque`.

- `Data.Deque`: spine-strict real-time double-ended queue.

  List-based alternative to
  [`Seq`](https://hackage.haskell.org/package/containers-0.7/docs/Data-Sequence.html).
  Measurably worse performance-wise in every respect, except perhaps for
  constant-time `reverse`. No real reason to use this.
