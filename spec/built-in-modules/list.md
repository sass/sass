# List Module

This built-in module is available from the URL `sass:list`.

## Table of Contents

* [Functions](#functions)
  * [`append()`](#append)
  * [`index()`](#index)
  * [`is-bracketed()`](#is-bracketed)
  * [`join()`](#join)
  * [`length()`](#length)
  * [`separator()`](#separator)
  * [`slash()`](#slash)
  * [`nth()`](#nth)
  * [`set-nth()`](#set-nth)
  * [`zip()`](#zip)

## Functions

### `append()`

```
append($list, $val, $separator: auto)
```

This function is also available as a global function named `append()`.

### `index()`

```
index($list, $value)
```

This function is also available as a global function named `index()`.

### `is-bracketed()`

```
is-bracketed($list)
```

This function is also available as a global function named `is-bracketed()`.

### `join()`

```
join($list1, $list2, $separator: auto, $bracketed: auto)
```

This function is also available as a global function named `join()`.

### `length()`

```
length($list)
```

This function is also available as a global function named `length()`.

### `separator()`

```
separator($list)
```

This function is also available as a global function named `list-separator()`.

### `slash()`

```
slash($elements...)
```

* If `$elements` contains zero or one values, throw an error.
* Return an unbracketed slash-separated list containing `$elements`.

### `nth()`

```
nth($list, $n)
```

This function is also available as a global function named `nth()`.

### `set-nth()`

```
set-nth($list, $n, $value)
```

This function is also available as a global function named `set-nth()`.

### `zip()`

```
zip($lists...)
```

This function is also available as a global function named `zip()`.
