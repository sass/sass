## Loud Comments

## Table of Contents

* [Semantics](#semantics)

### Semantics

To execute a loud comment `comment`:

* Let `contents` be the result of evaluating all the interpolation in `comment`.

* Let `css` be a CSS loud comment with the given `contents`.

* Let `parent` be the result of [splitting the current parent if necessary].

  [splitting the current parent if necessary]: stylesheet.md#splitting-the-current-parent-if-necessary

* Append `css` to `parent` if it's set, or to [the current module]'s CSS
  otherwise.

  [the current module]: spec.md#current-module
