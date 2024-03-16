## Draft 1.2

* Trigger plain CSS nesting behavior based on the type of *the parent* rule's
  stylesheet, so that we don't unexpectedly generate plain-CSS nesting for
  previously un-nested `@import` and `meta.load-css()` rules.

## Draft 1.1

* Trigger plain CSS nesting behavior based on the type of a rule's stylesheet,
  rather than the type of the current stylesheet, so that plain CSS behavior is
  preserved for nested `@import` and `meta.load-css()`.

## Draft 1

* Initial draft.
