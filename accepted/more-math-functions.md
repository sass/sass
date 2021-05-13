# More Math Functions: Draft 2.2

*[(Issue)](https://github.com/sass/sass/issues/851)*

This proposal adds the following members to the built-in `sass:math` module.

## Table of Contents
* [Background](#background)
* [Summary](#summary)
* [Semantics](#semantics)
  * [Built-in Module Variables](#built-in-module-variables)
* [Variables](#variables)
  * [`$e`](#e)
  * [`$pi`](#pi)
* [Functions](#functions)
  * [`clamp()`](#clamp)
  * [`hypot()`](#hypot)
  * [Exponentiation](#exponentiation)
    * [`log()`](#log)
    * [`pow()`](#pow)
    * [`sqrt()`](#sqrt)
  * [Trigonometry](#trigonometry)
    * [`cos()`](#cos)
    * [`sin()`](#sin)
    * [`tan()`](#tan)
    * [`acos()`](#acos)
    * [`asin()`](#asin)
    * [`atan()`](#atan)
    * [`atan2()`](#atan2)
      * [Edge cases](#edge-cases)

## Background

> This section is non-normative.

Sass recently implemented a module system with a new built-in `sass:math`
module. The demand for built-in math functions can now be fulfilled safely by
implementing them inside this module. None of these new functions will be made
available on the global namespace.

## Summary

> This section is non-normative.

This proposal defines Sassified versions of all the mathematical functions in
the [CSS Values and Units 4 Draft][], as well as logarithms and the constants
`e` and `pi`. Each function is basically equivalent to its mathematical form,
with stricter unit handling. Proper unit handling prevents these functions from
creating meaningless units. For instance, consider `(1px)^(1/3)`—what does
the unit `px^(1/3)` mean?

[CSS Values and Units 4 Draft]: https://drafts.csswg.org/css-values-4/#math

To avoid issues like this, the exponential functions—`log()`, `pow()`, `sqrt()`—
accept only a unitless number as input, and output a unitless number.

The trig functions—`cos()`, `sin()`, `tan()`—accept a SassScript number with a
unit, as long as that unit is an [angle][] type. If the input is a unitless
number, it is treated as though it were in `rad`. These functions output a
unitless number.

[angle]: https://drafts.csswg.org/css-values-4/#angles

The inverse trig functions—`acos()`, `asin()`, `atan()`—accept a unitless number
and output a SassScript number in `deg`. `atan2()` is similar, but it accepts
two unitless numbers.

`clamp()` accepts three SassScript numbers with [compatible][] units: the
minimum value, preferred value, and maximum value. This function "clamps" the
preferred value in between the minimum and maximum values, while preserving
their units appropriately. For example, `clamp(1in, 15cm, 12in)` outputs `15cm`,
whereas `clamp(1in, 1cm, 12in)` outputs `1in`.

[compatible]: ../spec/built-in-modules/math.md#compatible

`hypot()` accepts `n` SassScript numbers with compatible units, and outputs the
length of the `n`-dimensional vector that has components equal to each of the
inputs. Since the inputs' units may all be different, the output takes the unit
of the first input.

## Semantics

### Built-in Module Variables

Variables defined in built-in modules are not modifiable. As such, this proposal
modifies the semantics of [Executing a Variable Declaration][] within the
[Variables spec][] to read as follows:

[Executing a Variable Declaration]: ../spec/variables.md#executing-a-variable-declaration
[Variables spec]: ../spec/variables.md

To execute a `VariableDeclaration` `declaration`:

* Let `value` be the result of evaluating `declaration`'s `Expression`.

* Let `name` be `declaration`'s `Variable`.

* **Let `resolved` be the result of [resolving a variable][] named `name`.**

[resolving a variable]: ../spec/modules.md#resolving-a-member

* If `name` is a `NamespacedVariable` and `declaration` has a `!global` flag,
  throw an error.

* **Otherwise, if `resolved` is a variable from a built-in module, throw an
  error.**

* Otherwise, if `declaration` is outside of any block of statements, *or*
  `declaration` has a `!global` flag, *or* `name` is a `NamespacedVariable`:

  * ~~Let `resolved` be the result of [resolving a variable][] named `name` using
    `file`, `uses`, and `import`.~~

  (...)

* Otherwise, if `declaration` is within one or more blocks associated with
  `@if`, `@each`, `@for`, and/or `@while` rules *and no other blocks*:

  * ~~Let `resolved` be the result of [resolving a variable][] named `name`.~~

  (...)

* ~~Otherwise, if no block containing `declaration` has a [scope][] with a
  variable named `name`, set the innermost block's scope's variable `name` to
  `value`.~~

[scope]: ../spec/variables.md#scope

* **Otherwise, if `resolved` is null, get the innermost block containing
  `declaration` and set its scope's variable `name` to `value`.**

* ~~Otherwise, let `scope` be the scope of the innermost block such that `scope`
  already has a variable named `name`.~~

* **Otherwise, set `resolved`'s value to `value`.**

## Variables

### `$e`

Equal to the value of the mathematical constant `e` with a precision of 10
digits after the decimal point: `2.7182818285`.

### `$pi`

Equal to the value of the mathematical constant `pi` with a precision of 10
digits after the decimal point: `3.1415926536`.

## Functions

### `clamp()`

```
clamp($min, $number, $max)
```

* If the units of `$min`, `$number`, and `$max` are not compatible with each
  other, throw an error.
* If some arguments have units and some do not, throw an error.
* If `$min >= $max`, return `$min`.
* If `$number <= $min`, return `$min`.
* If `$number >= $max`, return `$max`.
* Return `$number`.

### `hypot()`

```
hypot($numbers...)
```

* If all numbers are not compatible with each other, throw an error.
* If some numbers have units and some do not, throw an error.
* If all numbers are unitless, the return value is unitless.
* Otherwise, the return value takes the unit of the leftmost number.
* If any number equals `Infinity` or `-Infinity`, return `Infinity`.
* Return the square root of the sum of the squares of each number.

### Exponentiation

#### `log()`

```
log($number, $base: null)
```

* If `$number` has units, throw an error.
* If `$base` is null:
  * If `$number < 0`, return `NaN` as a unitless number.
  * If `$number == 0`, return `-Infinity` as a unitless number.
  * If `$number == Infinity`, return `Infinity` as a unitless number.
  * Return the [natural log][] of `$number`, as a unitless number.
* Otherwise, return the natural log of `$number` divided by the natural log of
  `$base`, as a unitless number.

[natural log]: https://en.wikipedia.org/wiki/Natural_logarithm

#### `pow()`

```
pow($base, $exponent)
```

* If `$base` or `$exponent` has units, throw an error.

* If `$exponent == 0`, return `1` as a unitless number.

* Otherwise, if `$exponent == Infinity` or `$exponent == -Infinity`:
  * If `$base == 1` or `$base == -1`, return `NaN` as a unitless number.
  * If `$base < -1` or `$base > 1` and if `$exponent > 0`, *or* if `$base > -1`
    and `$base < 1` and `$exponent < 0`, return `Infinity` as a
    unitless number.
  * Return `0` as a unitless number.

* Otherwise:
  * If `$base < 0` and `$exponent` is not an integer, return `NaN` as a unitless
    number.

  * If `$base == 0` and `$exponent < 0`, or if `$base == Infinity` and
    `$exponent > 0`, return `Infinity` as a unitless number.

  * If `$base == -0` and `$exponent < 0`, or if `$base == -Infinity` and
    `$exponent > 0`:
    * If `$exponent` is an odd integer, return `-Infinity` as a unitless number.
    * Return `Infinity` as a unitless number.

  * If `$base == 0` and `$exponent > 0`, or if `$base == Infinity` and
    `$exponent < 0`, return `0` as a unitless number.

  * If `$base == -0` and `$exponent > 0`, or if `$base == -Infinity` and
    `$exponent < 0`:
    * If `$exponent` is an odd integer, return `-0` as a unitless number.
    * Return `0` as a unitless number.

  * Return `$base` raised to the power of `$exponent`, as a unitless number.

#### `sqrt()`

```
sqrt($number)
```

* If `$number` has units, throw an error.
* If `$number < 0`, return `NaN` as a unitless number.
* If `$number == -0`, return `-0` as a unitless number.
* If `$number == 0`, return `0` as a unitless number.
* If `$number == Infinity`, return `Infinity` as a unitless number.
* Return the square root of `$number`, as a unitless number.

### Trigonometry

#### `cos()`

```
cos($number)
```

* If `$number` has units but is not an angle, throw an error.
* If `$number` is unitless, treat it as though its unit were `rad`.
* If `$number == Infinity` or `$number == -Infinity`, return `NaN` as a unitless
  number.
* Return the [cosine][] of `$number`, as a unitless number.

[cosine]: https://en.wikipedia.org/wiki/Trigonometric_functions#Right-angled_triangle_definitions

#### `sin()`

```
sin($number)
```

* If `$number` has units but is not an angle, throw an error.
* If `$number` is unitless, treat it as though its unit were `rad`.
* If `$number == Infinity` or `$number == -Infinity`, return `NaN` as a unitless
  number.
* If `$number == -0`, return `-0` as a unitless number.
* If `$number == 0`, return `0` as a unitless number.
* Return the [sine][] of `$number`, as a unitless number.

[sine]: https://en.wikipedia.org/wiki/Trigonometric_functions#Right-angled_triangle_definitions

#### `tan()`

```
tan($number)
```

* If `$number` has units but is not an angle, throw an error.
* If `$number` is unitless, treat it as though its unit were `rad`.
* If `$number == Infinity` or `$number == -Infinity`, return `NaN` as a unitless
  number.
* If `$number == -0`, return `-0` as a unitless number.
* If `$number == 0`, return `0` as a unitless number.
* If `$number` is equivalent to `90deg +/- 360deg * n`, where `n` is any
  integer, return `Infinity` as a unitless number.
* If `$number` is equivalent to `-90deg +/- 360deg * n`, where `n` is any
  integer, return `-Infinity` as a unitless number.
* Return the [tangent][] of `$number`, as a unitless number.

[tangent]: https://en.wikipedia.org/wiki/Trigonometric_functions#Right-angled_triangle_definitions

#### `acos()`

```
acos($number)
```

* If `$number` has units, throw an error.
* If `$number < -1` or `$number > 1`, return `NaN` as a number in `deg`.
* If `$number == 1`, return `0deg`.
* Return the [arccosine][] of `$number`, as a number in `deg`.

[arccosine]: https://en.wikipedia.org/wiki/Inverse_trigonometric_functions#Basic_properties

#### `asin()`

```
asin($number)
```

* If `$number` has units, throw an error.
* If `$number < -1` or `$number > 1`, return `NaN` as a number in `deg`.
* If `$number == -0`, return `-0deg`.
* If `$number == 0`, return `0deg`.
* Return the [arcsine][] of `$number`, as a number in `deg`.

[arcsine]: https://en.wikipedia.org/wiki/Inverse_trigonometric_functions#Basic_properties

#### `atan()`

```
atan($number)
```

* If `$number` has units, throw an error.
* If `$number == -0`, return `-0deg`.
* If `$number == 0`, return `0deg`.
* If `$number == -Infinity`, return `-90deg`.
* If `$number == Infinity`, return `90deg`.
* Return the [arctangent][] of `$number`, as a number in `deg`.

[arctangent]: https://en.wikipedia.org/wiki/Inverse_trigonometric_functions#Basic_properties

#### `atan2()`

> `atan2($y, $x)` is distinct from `atan($y / $x)` because it preserves the
> quadrant of the point in question. For example, `atan2(1, -1)` corresponds to
> the point `(-1, 1)` and returns `135deg`. In contrast, `atan(1 / -1)` and
> `atan(-1 / 1)` resolve first to `atan(-1)`, so both return `-45deg`.

```
atan2($y, $x)
```

* If `$y` and `$x` are not compatible, throw an error.
* If `$y` has units and `$x` does not, or vice-versa, throw an error.
* If the inputs match one of the following edge cases, return the provided
  number. Otherwise, return the [2-argument arctangent][] of `$y` and `$x`, as a
  number in `deg`.

[2-argument arctangent]: https://en.wikipedia.org/wiki/Atan2

##### Edge cases

<table>
  <thead>
    <tr>
      <td colspan="2"></td>
      <th colspan="6" style="text-align: center">X</th>
    </tr>
    <tr>
      <td colspan="2"></td>
      <th>−Infinity</th>
      <th>-finite</th>
      <th>-0</th>
      <th>0</th>
      <th>finite</th>
      <th>Infinity</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th rowspan="6">Y</th>
      <th>−Infinity</th>
      <td>-135deg</td>
      <td>-90deg</td>
      <td>-90deg</td>
      <td>-90deg</td>
      <td>-90deg</td>
      <td>-45deg</td>
    </tr>
    <tr>
      <th>-finite</th>
      <td>-180deg</td>
      <td></td>
      <td>-90deg</td>
      <td>-90deg</td>
      <td></td>
      <td>-0deg</td>
    </tr>
    <tr>
      <th>-0</th>
      <td>-180deg</td>
      <td>-180deg</td>
      <td>-180deg</td>
      <td>-0deg</td>
      <td>-0deg</td>
      <td>-0deg</td>
    </tr>
    <tr>
      <th>0</th>
      <td>180deg</td>
      <td>180deg</td>
      <td>180deg</td>
      <td>0deg</td>
      <td>0deg</td>
      <td>0deg</td>
    </tr>
    <tr>
      <th>finite</th>
      <td>180deg</td>
      <td></td>
      <td>90deg</td>
      <td>90deg</td>
      <td></td>
      <td>0deg</td>
    </tr>
    <tr>
      <th>Infinity</th>
      <td>135deg</td>
      <td>90deg</td>
      <td>90deg</td>
      <td>90deg</td>
      <td>90deg</td>
      <td>45deg</td>
    </tr>
  </tbody>
</table>
