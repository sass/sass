## Draft 3

* If a number is escaped at the beginning of an identifier, its canonical form
  should be its hex escape rather than `\` followed by the character, since that
  could be interpreted as a hex escape.

## Draft 2

* Include U+000D CARRIAGE RETURN and U+000C FORM FEED in the list of characters
  that should be rendered as escape codes.

* Disallow whitespace between `InterpolatedIdentifier` components.

## Draft 1

* Initial draft.
