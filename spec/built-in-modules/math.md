# Math Module

This built-in module is available from the URL `sass:math`.

This module contains Sassified versions of all the mathematical constants and
functions in the [CSS Values and Units 4 Draft], and more (such as logarithms,
`e`, `pi`). Each function is basically equivalent to its mathematical form,
though some have special handling of units.

[CSS Values and Units 4 Draft]: https://drafts.csswg.org/css-values-4/#math

## Table of Contents

* [Variables](#variables)
  * [`$e`](#e)
  * [`$pi`](#pi)
  * [`$epsilon`](#epsilon)
  * [`$max-safe-integer`](#max-safe-integer)
  * [`$min-safe-integer`](#min-safe-integer)
  * [`$max-number`](#max-number)
  * [`$min-number`](#min-number)
* [Functions](#functions)
  * [Bounding Functions](#bounding-functions)
    * [`ceil()`](#ceil)
    * [`clamp()`](#clamp)
    * [`floor()`](#floor)
    * [`max()`](#max)
    * [`min()`](#min)
    * [`round()`](#round)
  * [Distance Functions](#distance-functions)
    * [`abs()`](#abs)
    * [`hypot()`](#hypot)
  * [Exponential Functions](#exponential-functions)
    * [`log()`](#log)
    * [`pow()`](#pow)
    * [`sqrt()`](#sqrt)
  * [Trigonometric Functions](#trigonometric-functions)
    * [`acos()`](#acos)
    * [`asin()`](#asin)
    * [`atan()`](#atan)
    * [`atan2()`](#atan2)
    * [`cos()`](#cos)
    * [`sin()`](#sin)
    * [`tan()`](#tan)
  * [Unit Functions](#unit-functions)
    * [`compatible()`](#compatible)
    * [`is-unitless()`](#is-unitless)
    * [`unit()`](#unit)
  * [Other Functions](#other-functions)
    * [`div()`](#div)
    * [`percentage()`](#percentage)
    * [`random()`](#random)

## Variables

### `$e`

A unitless number whose value is the closest possible [double] approximation of
the [mathematical constant e].

[double]: ../types/number.md#double
[mathematical constant e]: https://en.wikipedia.org/wiki/E_(mathematical_constant)

> This is `2.718281828459045`.

### `$pi`

A unitless number whose value is the closest possible [double] approximation of
the [mathematical constant π].

[mathematical constant π]: https://en.wikipedia.org/wiki/Pi

> This is `3.141592653589793`.

### `$epsilon`

A unitless number whose value is the difference between 1 and the smallest
[double] greater than 1.

> This is `2.220446049250313e-16`.

### `$max-safe-integer`

A unitless number whose value represents the maximum mathematical integer `n`
such that `n` and `n + 1` both have an exact [double] representation.

> This is `9007199254740991`.

### `$min-safe-integer`

A unitless number whose value represents the minimum mathematical integer `n`
such that `n` and `n - 1` both have an exact [double] representation.

> This is `-9007199254740991`.

### `$max-number`

A unitless number whose value represents the greatest finite number that can be
represented by a [double].

> This is `1.7976931348623157e+308`.

### `$min-number`

A unitless number whose value represents the least positive number that can be
represented by a [double].

> This is `5e-324`.

## Functions

### Bounding Functions

#### `ceil()`

```
ceil($number)
```

This function is also available as a global function named `ceil()`.

* Return a number whose value is the result of
  `convertToIntegerTowardPositive($number.value)` as defined by [IEEE 754 2019],
  §5.8; and whose units are the same as `$number`'s.

[IEEE 754 2019]: https://ieeexplore.ieee.org/document/8766229

#### `clamp()`

```
clamp($min, $number, $max)
```

* If some arguments have units and some do not, throw an error.
* If `$min`, `$number`, and `$max` have units, but the units are not
  [compatible] with each other, throw an error.
* If `$min >= $max`, return `$min`.
* If `$number <= $min`, return `$min`.
* If `$number >= $max`, return `$max`.
* Return `$number`.

[compatible]: ../types/number.md#compatible-units

#### `floor()`

```
floor($number)
```

This function is also available as a global function named `floor()`.

* Return a number whose value is the result of
  `convertToIntegerTowardNegative($number.value)` as defined by [IEEE 754 2019],
  §5.8; and whose units are the same as `$number`'s.

#### `max()`

```
max($numbers...)
```

This function is also available as a global function named `max()`.

#### `min()`

```
min($numbers...)
```

This function is also available as a global function named `min()`.

#### `round()`

```
round($number)
```

This function is also available as a global function named `round()`.

* Return a number whose value is the result of
  `convertToIntegerTiesToAway($number.value)` as defined by [IEEE 754 2019],
  §5.8; and whose units are the same as `$number`'s.

### Distance Functions

#### `abs()`

```
abs($number)
```

This function is also available as a global function named `abs()`.

* Return a number whose value is the result of `abs($number.value)` as defined
  by [IEEE 754 2019], §5.5.1; and whose units are the same as `$number`'s.

#### `hypot()`

```
hypot($numbers...)
```

* If some numbers have units and some do not, throw an error.
* If all numbers have units, but the units are not [compatible] with each other,
  throw an error.
* If all numbers are unitless, the return value is unitless.
* Otherwise, the return value takes the unit of the leftmost number.
* If any number equals `Infinity` or `-Infinity`, return `Infinity`.
* Return the square root of the sum of the squares of each number.

### Exponential Functions

> Exponential operations on numbers with units would create meaningless units
> (e.g. `(1px)^(1/3)` has a unit of `px^(1/3)`). To prevent this, the
> exponential functions accept only a unitless number as input, and return a
> unitless number.

#### `log()`

```
log($number, $base: null)
```


* If `$number` has units, throw an error.

* Return a unitless number whose value is the result of `log($number.value)` as
  defined by [IEEE 754 2019], §9.2.

> This is the [natural logarithm].
>
> [natural logarithm]: https://en.wikipedia.org/wiki/Natural_logarithm

#### `pow()`

```
pow($base, $exponent)
```

* If `$base` or `$exponent` has units, throw an error.

* Return a unitless number whose value is the result of `pow($number.value,
  $exponent.value)` as defined by [IEEE 754 2019], §9.2.

#### `sqrt()`

```
sqrt($number)
```

* If `$number` has units, throw an error.

* Return a unitless number whose value is the result of `rootn($number.value,
  2)` as defined by [IEEE 754 2019], §9.2.

### Trigonometric Functions

> The trigonometric functions accept a number with a unit, as long as that unit
> is an [angle] type, and output a unitless number. If the input is unitless, it
> must be treated as though it were in `rad`.
>
> The inverse trig functions accept unitless numbers and output a number in
> `deg`.
>
> [angle]: https://drafts.csswg.org/css-values-4/#angles

#### `acos()`

```
acos($number)
```

* If `$number` has units, throw an error.

* Let `result` be a number in `rad` whose value is the result of
  `acos($number.value)` as defined by [IEEE 754 2019], §9.2.

* Return the result of [converting `result` to `deg`].

  [converting `result` to `deg`]: ../types/number.md#converting-a-number-to-a-unit

#### `asin()`

```
asin($number)
```

* If `$number` has units, throw an error.

* Let `result` be a number in `rad` whose value is the result of
  `asin($number.value)` as defined by [IEEE 754 2019], §9.2.

* Return the result of [converting `result` to `deg`].

#### `atan()`

```
atan($number)
```

* If `$number` has units, throw an error.

* Let `result` be a number in `rad` whose value is the result of
  `atan($number.value)` as defined by [IEEE 754 2019], §9.2.

* Return the result of [converting `result` to `deg`].

#### `atan2()`

> `atan2($y, $x)` is distinct from `atan($y / $x)` because it preserves the
> quadrant of the point in question. For example, `atan2(1, -1)` corresponds to
> the point `(-1, 1)` and returns `135deg`. In contrast, `atan(1 / -1)` and
> `atan(-1 / 1)` resolve first to `atan(-1)`, so both return `-45deg`.

```
atan2($y, $x)
```

* If the units of `$y` and `$x` are not [compatible], throw an error.

* If `$y` has units and `$x` does not, or vice-versa, throw an error.

* Let `result` be a number in `rad` whose value is the result of
  `atan2($y.value, $x.value)` as defined by [IEEE 754 2019], §9.2.

* Return the result of [converting `result` to `deg`].

#### `cos()`

```
cos($number)
```

* Let `double` be the value of [converting `$number` to `rad`] allowing
  unitless.

  [converting `$number` to `rad`]: ../types/number.md#converting-a-number-to-a-unit

* Return a unitless number whose value is the result of `cos(double)` as defined
  by [IEEE 754 2019], §9.2.

#### `sin()`

```
sin($number)
```

* Let `double` be the value of [converting `$number` to `rad`] allowing
  unitless.

* Return a unitless number whose value is the result of `sin(double)` as defined
  by [IEEE 754 2019], §9.2.

#### `tan()`

```
tan($number)
```

* Let `double` be the value of [converting `$number` to `rad`] allowing
  unitless.

* Return a unitless number whose value is the result of `tan(double)` as defined
  by [IEEE 754 2019], §9.2.

### Unit Functions

#### `compatible()`

```
compatible($number1, $number2)
```

This function is also available as a global function named `comparable()`.

* If `$number1` or `$number2` is not a number, throw an error.
* If `$number1` or `$number2` is unitless, return true.
* If the units of `$number1` and `$number2` are [compatible], return true.
* Otherwise, return false.

#### `is-unitless()`

```
is-unitless($number)
```

This function is also available as a global function named `unitless()`.

#### `unit()`

```
unit($number)
```

This function is also available as a global function named `unit()`.

### Other Functions

#### `div()`

```
div($number1, $number2)
```

* If `$number1` is a color and `$number2` is either a number or a color, throw an
  error.
* Otherwise, if `$number2` is a number and `$number2` is a color, throw an error.
* Otherwise, if either of `$number1` or `$number2` are not numbers, return an
  unquoted string whose contents is the result of serializing `$number1`
  followed by `"/"` followed by the result of serializing `$number2`.
* Let `quotient` be a number such that:
  * Its value is the result of `divide($number1.value, $number2.value)` as defined
    by [IEEE 754 2019], §5.4.1.
  * Its numerator units are equal to `$number1`'s numerator units followed by
    `$number2`'s denominator units.
  * Its denominator units are equal to `$number1`'s denominator units followed
    by `$number2`'s numerator units.
* Return the result of simplifying `quotient`.

#### `percentage()`

```
percentage($number)
```

This function is also available as a global function named `percentage()`.

#### `random()`

```
random($limit: null)
```

This function is also available as a global function named `random()`.

* If `$limit` is `null` then return a pseudo-random unitless number whose value
   is in the range `[0, 1)`.

  > Example: `math.random() => 0.1337001337`

* If `$limit` is an [integer] greater than zero:

  * Return a pseudo-random integer in the range `[1, $limit]` with the same
    units as `$limit`.

    > Examples:
    > - `math.random(123) => 87`
    > - `math.random(123px) => 43px`
    > - `math.random(500%) => 238%`

* Otherwise throw an error.

[integer]: ../types/number.md#integer
