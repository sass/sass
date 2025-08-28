# Stylesheet

## Table of Contents

* [Procedures](#procedures)
  * [Splitting the Current Parent If Necessary](#splitting-the-current-parent-if-necessary)

## Procedures

### Splitting the Current Parent If Necessary

This procedure accepts no arguments and returns a style rule, keyframe block,
at-rule, or null.

* Let `parent` be the [current style rule], [keyframe block], or at-rule if one
  exists; or the innermost if multiple exist.

  [current style rule]: style-rules.md#current-style-rule
  [keyframe block]: style-rules.md#current-style-rule

* If `parent` is null, return null.

* Otherwise, if `parent` is the last statement in its parent, return `parent`.

* Otherwise:

  * Let `copy` by a copy of `parent` without any children.

  * Append `copy` to `parent`'s parent.

  * Set the [current style rule], [keyframe block], or at-rule (according to
    `copy`'s type) to `copy`, for the remaining duration of its previous value.

  * Return `copy`.
