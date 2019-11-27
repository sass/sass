# More Math Functions: Draft 1

*[(Issue)](https://github.com/sass/sass/issues/851)*

This proposal adds the following members to the built-in `sass:math` module.

## Table of Contents
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

## Variables

### `$e`

Equal to the value of the mathematical constant `e` with a precision of 10
digits: `2.718281828`.

### `$pi`

Equal to the value of the mathematical constant `pi` with a precision of 10
digits: `3.141592654`.

## Functions

### `clamp()`

```
clamp($min, $number, $max)
```

* Throw an error if the units of `$min`, `$number`, and `$max` are not
  [compatible][] with each other.
* Return `$min` if `$min >= $max`
* Return `$min` if `$number <= $min`.
* Return `$max` if `$number >= $max`.
* Return `$number`.

[compatible]: ../spec/built_in_modules/math.md#compatible

### `hypot()`

```
hypot($numbers...)
```

* Throw an error if any argument is not a [length][].
* Return `Infinity` if any argument is `Infinity`.
* Return the square root of the sum of the squares of each argument.

[length]: https://drafts.csswg.org/css-values-4/#lengths

### Exponentiation

#### `log()`

```
log($number, $base: null)
```

* Throw an error if `$number` has units or `$number < 0`.
* If `$base == null`:
  * Return `-Infinity` if `$number == 0`.
  * Return `Infinity` if `$number == Infinity`.
  * Return the [natural log][] of `$number`.
* Otherwise:
  * Throw an error if `$base < 0` or `$base == 0` or `$base == 1`.
  * Return the natural log of `$number` divided by the natural log of `$base`.

[natural log]: https://en.wikipedia.org/wiki/Natural_logarithm

#### `pow()`

```
pow($base, $exponent)
```

* Throw an error if `$base` or `$exponent` has units.

* If `$exponent == 0`:
  * Return `1`.

* Otherwise, if `$exponent == Infinity`:
  * Return `NaN` if the `absolute value of $base == 1`.
  * Return `Infinity` if the `absolute value of $base > 1` and `$exponent > 0`,
    or if the `absolute value of $base < 1` and `$exponent < 0`.
  * Return `0`.

* Otherwise:
  * Return `NaN` if `$base < 0` and `$exponent` is not an integer.

  * Return `Infinity` if `$base == 0` and `$exponent < 0`, or if
    `$base == Infinity` and `$exponent > 0`.
  * If `$base == -0` and `$exponent < 0`, or if `$base == -Infinity` and
    `$exponent > 0`:
    * Return `-Infinity` if `$exponent` is an odd integer.
    * Return `Infinity`.

  * Return `0` if `$base == 0` and `$exponent > 0`, or if `$base == Infinity`
    and `$exponent < 0`.
  * If `$base == -0` and `$exponent > 0`, or if `$base == -Infinity` and
    `$exponent < 0`:
    * Return `-0` if `$exponent` is an odd integer.
    * Return `0`.

  * Return `$base` raised to the power of `$exponent`.

#### `sqrt()`

```
sqrt($number)
```

* Throw an error if `$number` has units.
* Return `NaN` if `$number < 0`.
* Return `-0` if `$number == -0`.
* Return `Infinity` if `$number == Infinity`.
* Return the square root of `$number`.

### Trigonometry

#### `cos()`

```
cos($number)
```

* Throw an error if `$number` has units but is not an [angle][]. If `$number` is
  unitless, it is assumed to be in radians.
* Return `NaN` if `$number == Infinity`.
* Return the [cosine][] of `$number`, which is a unitless number between `-1`
  and `1`, exclusive.

[angle]: https://drafts.csswg.org/css-values-4/#angles
[cosine]: https://en.wikipedia.org/wiki/Trigonometric_functions#Right-angled_triangle_definitions

#### `sin()`

```
sin($number)
```

* Throw an error if `$number` has units but is not an angle. If `$number` is
  unitless, it is assumed to be in radians.
* Return `NaN` if `$number == Infinity`.
* Return `-0` if `$number == -0`.
* Return the [sine][] of `$number`, which is a unitless number between `-1` and
  `1`, exclusive.

[sine]: https://en.wikipedia.org/wiki/Trigonometric_functions#Right-angled_triangle_definitions

#### `tan()`

```
tan($number)
```

* Throw an error if `$number` has units but is not an angle. If `$number` is
  unitless, it is assumed to be in radians.
* Return `NaN` if `$number == Infinity`.
* Return `-0` if `$number == -0`.
* Return `Infinity` if `$number == 90 +/- (360 * n)`, where `n` is any
  integer.
* Return `-Infinity` if `$number == -90 +/- (360 * n)`, where `n` is any
  integer.
* Return the [tangent][] of `$number`, which is a unitless number between
  `-Infinity` and `Infinity`.

[tangent]: https://en.wikipedia.org/wiki/Trigonometric_functions#Right-angled_triangle_definitions

#### `acos()`

```
acos($number)
```

* Throw an error if `$number` has units.
* Return `NaN` if the `absolute value of $number > 1`.
* Return `0` if `$number == 1`.
* Return the [arccosine] of `$number`, in radians. It is between `0` and
  `pi rad`, inclusive.

[arccosine]: https://en.wikipedia.org/wiki/Inverse_trigonometric_functions#Basic_properties

#### `asin()`

```
asin($number)
```

* Throw an error if `$number` has units.
* Return `NaN` if `absolute value of $number > 1`.
* Return `-0` if `$number == -0`.
* Return the [arcsine] of `$number`, in rad. It is between `-pi/2 rad` and
  `pi/2 rad`, inclusive.

[arcsine]: https://en.wikipedia.org/wiki/Inverse_trigonometric_functions#Basic_properties

#### `atan()`

```
atan($number)
```

* Throw an error if `$number` has units.
* Return `-0` if `$number == -0`.
* Return `-90` if `$number == -Infinity`.
* Return `90` if `$number == Infinity`.
* Return the [arctangent] of `$number`, in rad. It is between `-pi/2 rad` and
  `pi/2 rad`, inclusive.

[arctangent]: https://en.wikipedia.org/wiki/Inverse_trigonometric_functions#Basic_properties

#### `atan2()`

> `atan2($y, $x)` is distinct from `atan($y / $x)` because it preserves the
> quadrant of the point in question. For example, `atan2(1, -1)` corresponds to
> the point `(-1, 1)` and returns `(0.75 * pi) rad`. In contrast, `atan(1 / -1)`
> and `atan(-1 / 1)` resolve first to `atan(-1)`, so both return (`-0.25 * pi)
> rad`.

```
atan2($y, $x)
```

* `$y` and `$x` must either have the same unit or be unitless. If not, throw an
  error.
* If the inputs match one of the following edge cases, return that value in
  radians. Otherwise, return the [2-argument arctangent][] of `$y` and `$x`, in
  radians.

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
      <td>-0.75 * pi</td>
      <td>-0.5 * pi</td>
      <td>-0.5 * pi</td>
      <td>-0.5 * pi</td>
      <td>-0.5 * pi</td>
      <td>-0.25 * pi</td>
    </tr>
    <tr>
      <th>-finite</th>
      <td>-pi</td>
      <td></td>
      <td>-0.5 * pi</td>
      <td>-0.5 * pi</td>
      <td></td>
      <td>-0</td>
    </tr>
    <tr>
      <th>-0</th>
      <td>-pi</td>
      <td>-pi</td>
      <td>-pi</td>
      <td>-0</td>
      <td>-0</td>
      <td>-0</td>
    </tr>
    <tr>
      <th>0</th>
      <td>pi</td>
      <td>pi</td>
      <td>pi</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
    </tr>
    <tr>
      <th>finite</th>
      <td>pi</td>
      <td></td>
      <td>0.5 * pi</td>
      <td>0.5 * pi</td>
      <td></td>
      <td>0</td>
    </tr>
    <tr>
      <th>Infinity</th>
      <td>0.75 * pi</td>
      <td>0.5 * pi</td>
      <td>0.5 * pi</td>
      <td>0.5 * pi</td>
      <td>0.5 * pi</td>
      <td>0.25 * pi</td>
    </tr>
  </tbody>
</table>
