## [2.0.0.0] - December, 2024

- Moved `Queue` to `Data.Queue`.

- Moved `EphemeralQueue` to `Data.Steque`.

- Queues no longer have `Semigroup`, `Monoid` and `Eq` instances.

- `Queue` is no longer a `Functor` or `Traversable`.

- Queues' `Foldable` instances no longer guarantee order.

- Renamed `toList` and `fromList` to `toListF` and `fromListF` respectfully.

- Renamed `enqueue` and `enqueueFront` to `(|>)` and `(<|)` respectfully with argument
  order matching operator direction.

- Replaced `dequeue` with `viewF`.

- Renamed `isEmpty` to `null`.

- Exposed internals in `*.Unsafe` modules and debug functions in `*.Debug`.

- Added extra functions.



## [1.0.0] - March, 2024

- Initial release.
