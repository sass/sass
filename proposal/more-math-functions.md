# Math Module

This built-in module is available from the URL `sass:math`.

## Table of Contents
* [Variables](#variables)
  * [`e`](#e)
  * [`pi`](#pi)
* [Functions](#functions)
  * [Approximation](#approximation)
    * [`ceil()`](#ceil)
    * [`floor()`](#floor)
    * [`round()`](#round)
  * [Comparison](#comparison)
    * [`clamp()`](#clamp)
    * [`max()`](#max)
    * [`min()`](#min)
    * [`percentage()`](#percentage)
  * [Division](#division)
    * [`div()`](#div)
    * [`mod()`](#mod)
  * [Exponentiation](#exponentiation)
    * [`log()`](#log)
    * [`pow()`](#pow)
    * [`sqrt()`](#sqrt)
  * [Trigonometry](#trigonometry)
    * [Procedures](#procedures)
      * [Verify Inputs](#verify-inputs)
    * [`cos()`](#cos)
    * [`sin()`](#sin)
    * [`tan()`](#tan)
    * [`acos()`](#acos)
    * [`asin()`](#asin)
    * [`atan()`](#atan)
    * [`atan2()`](#atan2)
      * [Edge cases](#edge-cases)
  * [Units](#units)
    * [`compatible()`](#compatible)
    * [`is-unitless()`](#is-unitless)
    * [`unit()`](#unit)
  * [Util](#util)
    * [`abs()`](#abs)
    * [`hypot()`](#hypot)
    * [`random()`](#random)

## Variables

### `e`

```
$e
```

### `pi`

```
$pi
```

## Functions

### Approximation

#### `ceil()`

```
ceil($number)
```
This function is also available as a global function named `ceil()`.

#### `floor()`

```
floor($number)
```

This function is also available as a global function named `floor()`.

#### `round()`

```
round($number)
```

This function is also available as a global function named `round()`.

### Comparison

#### `clamp()`

```
clamp($min, $number, $max)
```

* Return `$min` if `$min` >= `$max`
* Return `$min` if `$number` <= `$min`.
* Return `$max` if `$number` => `$max`.
* Return `$number`.

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

#### `percentage()`

```
percentage($number)
```

This function is also available as a global function named `percentage()`.

### Division

#### `div()`

```
div($dividend, $divisor)
```

This function is also available as a global function named `divide()`.

* Throw error if the units of `$dividend` and `$divisor` are not
  [compatible](#compatible).
* Return `$dividend` / `$divisor`.

#### `mod()`

```
mod($dividend, $modulus)
```

* Throw error if the units of `$dividend` and `$modulus` are not
  [compatible](#compatible).
* Return `$dividend` % `$modulus`.

### Exponentiation

#### `log()`

```
log($number)
```
* Throw error if `$number` has units or `$number` < 0.
* Return -Infinity if `$number` is 0.
* Return Infinity if `$number` is infinite.
* Return the natural log of `$number`.

```
log($number, $base)
```

* Throw error if `$base` < 0 or `$base` is 0 or `$base` is 1.
* Return [log](#log)(`$number`) / [log](#log)(`$base`).

#### `pow()`

```
pow($base, $exponent)
```

* Throw error if `$base` or `$exponent` has units.

* If `$exponent` is 0:
  * Return 1.

* Else if `$exponent` is infinite:
  * Return NaN if [abs](#abs)(`$base`) is 1.
  * Return Infinity if [abs](#abs)(`$base`) > 1 and `$exponent` > 0, or if
    [abs](#abs)(`$base`) < 1 and `$exponent` < 0.
  * Return 0.

* Else if `$exponent` is finite:
  * Return NaN if `$base` < 0 and `$exponent` is not an integer.

  * Return Infinity if `$base` is 0 and `$exponent` < 0, or if `$base` is
    Infinity and `$exponent` > 0.
  * If `$base` is -0 and `$exponent` < 0, or if `$base` is -Infinity and
    `$exponent` > 0:
    * Return -Infinity if `$exponent` is an odd integer.
    * Return Infinity.

  * Return 0 if `$base` is 0 and `$exponent` > 0, or if `$base` is Infinity and
    `$exponent` < 0.
  * If `$base` is -0 and `$exponent` > 0, or if `$base` is -Infinity and
    `$exponent` < 0:
    * Return -0 if `$exponent` is an odd integer.
    * Return 0.

  * Return `$base` raised to the power of `$exponent`.

#### `sqrt()`

```
sqrt($number)
```

* Throw error if `$number` has units.
* Return NaN if `$number` < 0.
* Return -0 if `$number` is -0.
* Return Infinity if `$number` is infinite.
* Return the square root of `$number`.

### Trigonometry

#### Procedures

##### Verify Inputs

`cos($number), sin($number), and tan($number)` throw error if `$number` has
units but is not a rotation value. If `$number` is unitless, it is assumed to be
in radians.

`acos($number), asin($number), and atan($number)` throw error if `$number` has
units.

Both inputs to `atan2($y, $x)` must have the same unit or be unitless. If not,
throw error.

#### `cos()`

```
cos($number)
```

* [Verify inputs](#verify-inputs).
* Return NaN if `$number` is infinite.
* Return cos(`$number`).

#### `sin()`

```
sin($number)
```

* [Verify inputs](#verify-inputs).
* Return NaN if `$number` is infinite.
* Return -0 if `$number` is -0.
* Return sin(`$number`).

#### `tan()`

```
tan($number)
```

* [Verify inputs](#verify-inputs).
* Return NaN if `$number` is infinite.
* Return -0 if `$number` is -0.
* If `$number` is an asymptote value:
  * Return Infinity if `$number` is 90deg +/- 360*n* deg, where *n* is any integer.
  * Return -Infinity if `$number` is -90deg +/- 360*n* deg, where *n* is any
    integer.
* Return tan(`$number`).

#### `acos()`

```
acos($number)
```

* [Verify inputs](#verify-inputs).
* Return NaN if [abs](#abs)(`$number`) > 1.
* Return 0 if `$number` is 1.
* Return acos(`$number`).

#### `asin()`

```
asin($number)
```

* [Verify inputs](#verify-inputs).
* Return NaN if [abs](#abs)(`$number`) > 1.
* Return -0 if `$number` is -0.
* Return asin(`$number`).

#### `atan()`

```
atan($number)
```

* [Verify inputs](#verify-inputs).
* Return -0 if `$number` is -0.
* Return -90deg if `$number` is -Infinity.
* Return 90deg if `$number` is Infinity.
* Return atan(`$number`).

#### `atan2()`

`atan2($y, $x)` is distinct from `atan($y / $x)` because it preserves the
quadrant of the point in question. For example, `atan2(1, -1)` corresponds to
the point (-1, 1), which returns 135deg. In contrast, `atan(1 / -1)` and
`atan(-1 / 1)` resolve first to `atan(-1)`, so both return -45deg.

```
atan2($y, $x)
```

* [Verify inputs](#verify-inputs).
* If the inputs match one of the following edge cases, return the value for that
  edge case. Otherwise, return normal atan2($y, $x).

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
      <td>(normal)</td>
      <td>-90deg</td>
      <td>-90deg</td>
      <td>(normal)</td>
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
      <td>(normal)</td>
      <td>90deg</td>
      <td>90deg</td>
      <td>(normal)</td>
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

### Units

#### `compatible()`

```
compatible($number1, $number2)
```

This function is also available as a global function named `comparable()`.

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

### Util

#### `abs()`

```
abs($number)
```

This function is also available as a global function named `abs()`.

#### `hypot()`

```
hypot($numbers...)
```

* Throw error if any argument is not a length value.
* Return Infinity if any argument is infinite.
* Return the square root of the sum of the squares of each argument.

#### `random()`

```
random($limit: null)
```

This function is also available as a global function named `random()`.
