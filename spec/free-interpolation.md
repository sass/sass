# Free Interpolation

"Free interpolation" refers to interpolation that appears in SassScript values
outside of a quoted string. Earlier versions of Sass had very convoluted rules
for parsing free interpolation, but the current rule is very simple:
interpolation is parsed as though it were an alphabetic character in an
identifier. This means that:

* It can be used within an identifier, as in `a#{b}c`, and is considered part of
  that identifier. The contents of the interpolation is evaluated at runtime and
  concatenated with the rest of the identifier to produce an unquoted string.

* It can be used on its own, as in `#{b}`. This is a single expression that
  converts the contents of the interpolation to an unquoted string.

* Because it's parsed as an *alphabetic* character, when it comes after a hyphen
  that hyphen is considered part of the identifier. This means that `$var -#{b}`
  is parsed as a space-separated list containing `$var` and `-#{b}` rather than
  as the operation `$var - #{b}`.

* Because numbers are not an identifier start characters and the interpolation
  is parsed as an alphabetic character, `0#{$b}` is parsed as a space-separated
  list containing `0` and `#{$b}`
