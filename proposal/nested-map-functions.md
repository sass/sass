# Nested Map Functions: Draft 1.0

*[(Issue)](https://github.com/sass/sass/issues/1739)*

This proposal updates the built-in `sass:map` module to better support merging,
setting, and getting items from nested maps.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Syntax](#syntax)
  * [`get()`](#get)
  * [`has-key()`](#has-key)
  * [`set()`](#set)
  * [`merge()`](#merge)

## Background

> This section is non-normative.

The current map inspection and manipulation functions don't provide any built-in
support for managing nested maps. Projects often build thir own tooling, but
the results are inconsistent, and often slow.

## Summary

> This section is non-normative.

This proposal updates the `sass:map` module to better support inspection and
manipulation of nested maps.

The `has-key()` and `get()` functions both accept multiple `$keys...`:

```scss
@use 'sass:map';

$nav: (
  'bg': 'gray',
  'color': (
    'hover': (
      'search': yellow,
      'home': red,
      'filter': blue,
    ),
  ),
);

$has-search: map.has-key($nav, 'color', 'hover', 'search'); // true
$search-hover: map.get($nav, 'color', 'hover', 'search'); // yellow
```

The `merge()` function now accepts multiple `$keys...` between the two maps
being merged. The keys form a path to the nested location in `$map1` where
`$map2` should be merged. For example, we update the hover colors in our `$nav`
map above:

```scss
@use 'sass:map';

$new-hover: (
  'search': green,
  'logo': orange,
);

$nav: map.merge($nav, 'color', 'hover', $new-hover);

// $nav: (
//   'bg': 'gray',
//   'color': (
//     'hover': (
//       'search': green,
//       'home': red,
//       'filter': blue,
//       'logo': orange,
//     ),
//   ),
// );
```

This proposal also adds a `set()` function to `sass:map`, with a similar syntax,
returning a map with any nested key set to a specific value. To achieve the
same output as our merge example, we can set each key individually:

```scss
@use 'sass:map';

$nav: map.set($nav, 'color', 'hover', 'search', green);
$nav: map.set($nav, 'color', 'hover', 'logo', orange);
```

## Syntax

### `get()`

```
get($map, $keys...)
```

* Let `key` be the first (or only) element in `$keys`

* If `$map` does not have a key with the same name as `key`, throw an error.

* Let `value` be the value assigned to `key` in `$map`

* If there is more than one element in `$keys`:

  * Let `keys` be all elements in `$keys` after the first element

  * Call `get()` with `value` and expanded `keys` as arguments

* Otherwise, return `value`

### `has-key()`

```
has-key($map, $keys...)
```

* Let `key` be the first (or only) element in `$keys`

* If `$map` does not have a key with the same name as `key`, return boolean
  `false`.

* If there is more than one element in `$keys`:

  * Let `map` be the value assigned to `key` in `$map`

  * Let `keys` be all but the first element in `$keys`

  * Call `has-key()` with `map` and expanded `keys` as arguments

* Otherwise, return boolean `true`

### `set()`

```
set($map1, $keys..., $value)
```

* If `$map1` is not a map, throw an error.

* If fewer than three arguments are provided, throw an error.

* Let `map` be an empty map .

* If there is more than one argument in arglist `$keys`:

  * Let `set-key` be the first element in `$keys`.

  * Let `keys` be a slice of all `$keys` elements except the first.

  * If `$map1` has a key `current-key` with the same name as `set-key`:

    * Let `current` be the value of `current-key`.

  * Otherwise:

    * Let `current` be an empty map.

  * Let `set-value` be the result of calling `set()` with `current` and expanded
      `keys` as arguments.

    > This will error if `current` is not a map, but we stil have nested `keys`

* Otherwise:

  * Let `set-key` be the only element in `$keys`

  * Let `set-value` be the value of `$value`

* Add a key with name `set-key` and value `set-value` to `map`

* For each `key`, `value` pair in `$map1`

  * If a key with name `key` already exists in `map`, do nothing.

  * Otherwise, add a key to `map` with name `key` and value `value`.

* Return the value `map`

### `merge()`

```
merge($map1, $keys..., $map2)
```

* If only one argument is provided, throw an error.

* If either the first (`$map1`) or last (`$map2`) argument is not a map, throw
  an error.

* Let `map` be an empty map.

* For each `key`, `new` pair in `$map2`:

  * If `$map1` has a key with the same name as `key`

  * Let `keys` be the result of appending `key` to the argument list `$keys`.

  * Update `map` to be the result of calling `set()` with `map`,
    expanded `keys`, and `value` as arguments.

* Return the value of `map`
