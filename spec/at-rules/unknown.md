# Unknown At-Rules

## Syntax

> In order to be flexible in its compatibility with future additions to CSS, Sass
> supports *all* at-rule names with a default syntax that's highly liberal in the
> structures it allows.

<x><pre>
**UnknownAtRule** ::= '@' [InterpolatedIdentifier] InterpolatedValue?
&#32;                   ('{' Statements '}')?
</pre></x>

[InterpolatedIdentifier]: ../syntax.md#interpolatedidentifier

No whitespace is allowed after `@`. As with all statements, an `UnknownAtRule`
without a block must be separated from other statements with a semicolon.

## Semantics

To execute an unknown at-rule `rule`:

* Let `name` be the result of evaluating `rule`'s `InterpolatedIdentifier`.

* Let `value` be the result of evaluating `rule`'s `InterpolatedValue`, if it
  exists.

* Let `css` be a CSS unknown at-rule with name `name`, value `value`, and with
  an empty list of children if `rule` has `Statements`.

* Let `parent` be the [current style rule], [keyframe block], or at-rule if one
  exists; or the innermost if multiple exist.

  [current style rule]: ../style-rules.md#current-style-rule
  [keyframe block]: ../style-rules.md#current-style-rule

* If `parent` is a keyframe block, throw an error.

* If `rule` has `Statements`:

  * If `parent` isn't set, append `css` to [the current module]'s CSS.

  * Otherwise, if `parent` is a style rule:

    * If `rule`'s name is "font-face", or if its [unprefixed] name is
      "keyframes", append `css` to [the current module]'s CSS.

    * Otherwise:

      * Append `css` to `parent`'s parent.

      * Append a copy of `parent` without any children to `css`.

        > This copy is now the [current style rule] until `rule` is done being
        > executed.

  * Otherwise, append `css` to `parent`.

  * Evaluate each child in `rule`'s `Statement`s.

* Otherwise:

  * Append `css` to `parent` if it's set, or to [the current module]'s CSS
    otherwise.
  
[the current module]: ../spec.md#current-module
[unprefixed]: ../syntax.md#vendor-prefix
