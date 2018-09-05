## Draft 3

* Add `@content` to the list of disallowed at-rules.

* Allow `@media` rules within style rules and at-rules. Recent CSS specs allow
  this, so we shouldn't forbid it.

* Disallow silent comments.

## Draft 2

* Explicitly defined the term "canonical URL" as it relates to stylesheets.

* Added a specification for loading plain CSS as an entrypoint file.

* Added a non-normative note explaining that the new import algorithm maintains
  the semantics of the existing import algorithm, other than supporting plain
  CSS files.

## Draft 1

* Initial draft.
