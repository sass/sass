## Draft 2

- Add an optional `name` parameter to the following methods:

  - `Value.assert*()`
  - `Value.sassIndexToListIndex()`
  - `SassString.sassIndexToStringIndex()`
  - `SassNumber.assert*()`
  - `SassNumber.convert*()`
  - `SassNumber.coerce*()`

- Make `Value.hashCode` a getter that returns a number instead of a string.

- Change `Value.asMap` from a method to a getter to better align with
  `Value.asList`.

## Draft 1

- Initial draft.
