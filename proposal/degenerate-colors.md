# Degenerate Colors: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/4240))*

## Table of Contents

* [Background](#background)
  * [Unspecified Infinities](#unspecified-infinities)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Polar Infinities](#polar-infinities)
    * [Non-Polar Infinities](#non-polar-infinities)
    * [No Deprecation](#no-deprecation)
* [Types](#types)
  * [Number](#number)
    * [Serialization](#serialization)
  * [Color](#color)
    * [Invariants](#invariants)

## Background

> This section is non-normative.

Sass has historically used the full IEEE 754 64-bit floating-point
representation for its numbers, including degenerate numbers such as `infinity`,
`-infinity`, and `NaN`, as well as separate positive and negative zero values.
This is in contrast to CSS, which "censors" the results of calculation trees so
that only rational numbers are visible outside of math functions.

In most cases, this difference in behavior is desirable. The compilation process
provides a natural boundary at which Sass numbers are serialized to CSS numbers,
and we use that to serialize degenerate numbers (although presently not negative
zero) into CSS-compatible formats such as `calc(infinity)` so that the user
agent can handle them according to the spec. (This also avoids Sass needing to
worry about whether a given serialization location is or could be in a
calculation tree.)

Unfortunately, there's one case where this behavior causes observable
differences with standard CSS: colors. Sass color channels retain degenerate
values, but because standard color operations involve numerous operations on
those values, the end result can be substantially different than in CSS,
especially when the highly-contagious `NaN` value is involved.

As an example, `color.change(hsl(0 0% 50%), $saturation: calc(NaN * 1%))` should
by spec be equivalent to `hsl(0 0% 50%)` and so converting it to RGB should
return `rgb(127.5, 127.5, 127.5)`. Instead, it currently returns `rgb(calc(NaN),
calc(NaN), calc(NaN))` because the `NaN` value is preserved to infects all
conversion operations.

### Unspecified Infinities

It's also important to note that the behavior of `infinity` and `-infinity` in
CSS colors are UA-dependent. The only guidance the CSS spec provides as to how
to handle them is that they must be clamped to whatever range the enclosing
context uses, but that range is itself up to the UA and is likely a
platform-dependent maximum or minimum floating point value. This limits Sass's
ability to do eager conversions that are guaranteed to work across all browsers.

## Summary

> This section is non-normative.

This proposal adds an additional "censoring" step to color creation in which
`NaN` and negative zero are converted to positive zero for all color channels.
For polar channels, `infinity` and `-infinity` are also converted to positive
zero; for non-polar channels, they're retained as infinite values.

In addition, new serialization is added for negative zero so that its identity
is retained in CSS.

### Design Decisions

#### Polar Infinities

Infinite values for polar channels are particularly unusual, because polar
channels are intrinsically periodic and infinities don't exist at a well-defined
point in that period. We choose to convert them to zero because `infinity % 360
= NaN`, and the result of censoring `NaN` is 0. This also matches the behavior
in practice of Chrome and Firefox.

#### Non-Polar Infinities

We choose to retain infinities for non-polar channels because CSS [doesn't
specify] exactly what they should resolve to. Preserving them as infinite allows
each user agent to resolve them as it chooses. In color operations where Sass is
effectively acting as a user agent, we expect the floating-point infinite
operations not to differ from what other user agents will do with the very large
numbers they use instead (particularly because many internal operations on those
numbers will result in infinite values anyway).

[doesn't specify]: #unspecified-infinities

Any case where these values will produce `NaN` or negative zero will once again
be censored upon constructing the resulting color.

#### No Deprecation

This proposal is technically a breaking change, because it causes a difference
in observable behavior that did not previously produce an error. However, it
does not include a deprecation period for the following reasons:

* A proper deprecation would require a multi-step process in which users would
  have to explicitly guard against passing values whose semantics are changing
  to color functions, something that would cause substantial churn for affected
  users.

* It's unlikely that this churn would benefit users; the new behavior is likely
  to be substantially more correct both in terms of matching browser behavior
  and producing colors closer to user intuition.

* Degenerate colors are inherently exceptional to begin with, so we expect there
  to be few if any cases where users are running into this at all, let alone
  where users are relying on the old behavior. This is especially likely given
  that we've received no reports relating to this behavior, even in edge cases
  that are known to produce invalid CSS.

* Sass's support for Color 4 as a whole is relatively young, which limits the
  time frame in which it would have even been possible to encounter this error.

## Types

### Number

Add the following non-normative note to [the number type]:

[the number type]: ../spec/types/number.md#types

> Note that while [CSS does not support] [degenerate numbers] outside `calc()`,
> *any* Sass number may be degenerate. (The same is true for negative zero.)
> This ensures that Sass can follow floating-point semantics in its own
> calculations, and that we can emit `calc(infinity)` and other similar forms
> rather than choosing an arbitrary number. Not only is this output more
> readable, the behavior of infinite values is UA-specific in most contexts, so
> there's no number we could choose that would be guaranteed to be equivalent to
> `calc(infinity)` everywhere.

[CSS does not support]: https://drafts.csswg.org/css-values/#calc-ieee
[degenerate numbers]: ../spec/types/number.md#degenerate-number

#### Serialization

In the first bullet point of [serializing a number], replace "or if it's
degenerate" with "if it's degenerate, or if it's negative zero".

[serializing a number]: ../spec/types/number.md#serialization

### Color

In the second bullet point of [the color type], add "(excluding `NaN` and
negative zero)" after the word "double".

[the color type]: ../spec/types/color.md#types

In addition, add the following paragraph:

CSS defines numerous operations on colors in terms of mathematical procedures
over the colors' channels. Although CSS doesn't allow infinite channel values
for its color, these procedures are still well-defined using the IEEE 754
operations on infinities, so Sass expands their domains to include infinities
and otherwise handles them as defined by CSS.

#### Invariants

When creating a new color or changing an existing one, implementations must
ensure the following invariants are maintained by enacting the given
conversions:

* [Polar angle channels] may not contain [degenerate numbers] or negative zero.
  These numbers are converted to 0.

* Non-polar-angle channels may not contain `NaN` or negative zero. These numbers
  are converted to 0.

[Polar angle channels]: ../spec/types/color.md#known-color-space
