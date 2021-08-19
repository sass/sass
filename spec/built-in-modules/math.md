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
      * [Edge cases](#edge-cases)
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

Equal to the value of the mathematical constant `e` with a precision of 10
digits after the decimal point: `2.7182818285`.

### `$pi`

Equal to the value of the mathematical constant `pi` with a precision of 10
digits after the decimal point: `3.1415926536`.

## Functions

### Bounding Functions

#### `ceil()`

```
ceil($number)
```

This function is also available as a global function named `ceil()`.

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

### Distance Functions

#### `abs()`

```
abs($number)
```

This function is also available as a global function named `abs()`.

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
* If `$base` is null:
  * If `$number < 0`, return `NaN` as a unitless number.
  * If `$number == 0`, return `-Infinity` as a unitless number.
  * If `$number == Infinity`, return `Infinity` as a unitless number.
  * Return the [natural log] of `$number`, as a unitless number.
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
* If `$number < -1` or `$number > 1`, return `NaN` as a number in `deg`.
* If `$number == 1`, return `0deg`.
* Return the [arccosine] of `$number`, as a number in `deg`.

[arccosine]: https://en.wikipedia.org/wiki/Inverse_trigonometric_functions#Basic_properties

#### `asin()`

```
asin($number)
```

* If `$number` has units, throw an error.
* If `$number < -1` or `$number > 1`, return `NaN` as a number in `deg`.
* If `$number == -0`, return `-0deg`.
* If `$number == 0`, return `0deg`.
* Return the [arcsine] of `$number`, as a number in `deg`.

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
* Return the [arctangent] of `$number`, as a number in `deg`.

[arctangent]: https://en.wikipedia.org/wiki/Inverse_trigonometric_functions#Basic_properties

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
* If the inputs match one of the following edge cases, return the provided
  number. Otherwise, return the [2-argument arctangent] of `$y` and `$x`, as a
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

#### `cos()`

```
cos($number)
```

* If `$number` has units but is not an angle, throw an error.
* If `$number` is unitless, treat it as though its unit were `rad`.
* If `$number == Infinity` or `$number == -Infinity`, return `NaN` as a unitless
  number.
* Return the [cosine] of `$number`, as a unitless number.

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
* Return the [sine] of `$number`, as a unitless number.

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
* Return the [tangent] of `$number`, as a unitless number.

[tangent]: https://en.wikipedia.org/wiki/Trigonometric_functions#Right-angled_triangle_definitions

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
  * Its value is the result of dividing `$number1`'s value by `$number2`'s
    value.
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
