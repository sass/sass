## Draft 2.0

* Change the parsing of special function calls as well as custom function
  definitions.

* Continue to forbid vendor-prefixed `calc()` definitions, because `calc()` was
  in the past guarded by a vendor prefix in several browsers.

* Also remove support for special parsing for *calls* to unnecessarily
  vendor-prefixed functions.

* Rename the deprecation from `function-case` to `function-name`.

* Indicate that uppercase definitions of `type()` should still throw an error
  even in Phase 1, since they already do.

* Fix an incorrect bullet number.

## Draft 1

* Initial draft.
