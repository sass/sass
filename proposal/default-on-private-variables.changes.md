## Draft 2.0

* Instead of prohibiting the use of `!default` in private variables, we now
  prohibit the use of private identifiers within `with` clauses. This is because
  there are valid use-cases for `!default` in private variables. The proposal
  title was updated as well.

* Updated deprecation warning name to `with-private`.

## Draft 1.0

* Initial draft.
