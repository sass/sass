## Draft 1.1

* Rename one of the two `IfExpression`s to `IfConditionExpression` to avoid a
  name collision.

* Allow `ArbitrarySubstition`s in place of `not` and in place of mixed clauses
  and operators.

* Explicitly indicate that `IfGroup` function calls syntax can't collide with
  `SassCondition`.

* Consider custom functions to be arbitrary substitution functions.

* Consider `SassCondition` to be an error in plain CSS.

* Require whitespace between identifiers and open parentheses.

* Indicate that string calculations in Special Numbers are a historical artifact
  rather than a living list.
