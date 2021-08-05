## Draft 2

* Add `name` parameters to `assert*()` methods so that they can provide
  additional debugging information.

* `Value.assertMap()` now returns an empty `SassMap` when called on an empty list.

* Renamed `Value.asMap()` to `Value.tryMap()` to help distinguish it from the
  `asList` getter.

* `Value.hashCode()` now returns a number to match the behavior expected by the
  `immutable` package.

* Removed `SassFunction.signature` since this couldn't be implemented for
  built-in functions.

* Added `SassMap.tryMap()` to override `Value.tryMap()` and declare statically
  that it never returns `null`.

* Make `Value` explicitly implement the `immutable` package's `ValueType`
  interface.

## Draft 1

* Initial draft.
