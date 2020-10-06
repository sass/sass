# Nested Map Functions: Draft 1.0

*[(Issue)](https://github.com/sass/sass/issues/1739)*

This proposal updates the built-in `sass:map` module to better support merging,
setting, and getting elements from nested maps.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Functions](#functions)
  * [`get()`](#get)
  * [`has-key()`](#has-key)
  * [`set()`](#set)
  * [`merge()`](#merge)
  * [`deep-merge()`](#deep-merge)
  * [`deep-remove()`](#deep-remove)

## Background

> This section is non-normative.

Variables have always been a key feature of the Sass language. But these days,
design systems and component libraries form the basis of most CSS projects --
with well organized _design tokens_ as the foundation. While Individual token
variables can be quite useful, the ability to group tokens into structured and
meaningful relationships is essential for creating resilient systems.

There are many ways to group tokens. The popular [Style Dictionary] recommends a
deep nesting of _category_, _type_, _item_, _sub-item_, and _state_. Other
taxonomies also include concepts like _theme_, or even _operating system_. Most
of the existing tools rely on YAML or JSON objects to achieve that nested
structure, at the expense of other important information. YAML and JSON are not
design languages, and do not understand fundamental CSS concepts like color or
length.

[Style Dictionary]: https://amzn.github.io/style-dictionary

With Sass, we don't have to make that tradeoff. We already support nestable map
structures, and the ability to interact with them programmatically -- adding or
removing properties, accessing values, and looping over entire structures. But
current built-in functions don't provide much support for managing nested maps.
Projects often build their own tooling.

The results are inconsistent across projects, difficult to re-use, and often
slow to compile. Implementing core support for nested maps could change all that.

## Summary

> This section is non-normative.

This proposal updates existing map functions with better support for inspection
and manipulation of nested maps, as well as adding new functions to the
`sass:map` module. For existing legacy functions (`get()`, `has-key()`,
`merge()`) the new behavior will be accessible through both the `sass:map`
module, and global legacy names (`map-get()`, `map-has-key()`, `map-merge()`).
New functions (`set()`, `deep-merge()`) will only be available inside the
`sass:map` module.

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

And finally, a new `deep-merge()` function in the `sass:map` module allows
merging two or more nested maps. This works much like the existing `merge()`
function, but when both maps have a nested-map at the same key, those nested
maps are also merged:

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

$update: (
  'bg': white,
  'color': (
    'hover': (
      'search': green,
      'logo': orange,
    )
  )
);

$nav: map.deep-merge($nav, $update);

// $nav: (
//   'bg': white,
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

## Functions

All new and modified functions are part of the `sass:map` built-in module.

### `get()`

This proposal updates the signature and behavior of the existing `get()`
function.

> This also affects the global `map-get()` function.

```
get($map, $key, $keys...)
```

> Intuitively, `get($map, $key1, $key2, $key3)` is equivalent to
> `get(get(get($map, $key1), $key2), $key3)` with the exception that if any
> intermediate value isn't a map or doesn't have the given key the whole
> function returns `null` rather than throwing an error.

* If `$map` is not a map, throw an error.

* Let `child` be `$map`.

* Let `keys` be a list containing `$key` followed by the elements of `$keys`.

* For each element `key` in `keys`:

  * If `child` is not a map, return `null`.

  * If `child` contains a key that's `==` to `key`, set `child` to the value
    associated with that key. Otherwise, return `null`.

* Return `child`.

### `has-key()`

This proposal updates the signature and behavior of the existing `get()`
function.

> This also affects the global `map-has-key()` function.

```
has-key($map, $key, $keys...)
```

> Intuitively, `has-key($map, $key1, $key2, $key3)` is equivalent to
> `has-key(get(get($map, $key1), $key2), $key3)` with the exception that if any
> intermediate value isn't a map or doesn't have the given key the whole
> function returns `false` rather than throwing an error.

* If `$map` is not a map, throw an error.

* Let `child` be `$map`.

* Let `keys` be a list containing `$key` followed by the elements of `$keys`.

* For each element `key` in `keys`:

  * If `child` is not a map, return `false`.

  * If `child` contains a key that's `==` to `key`, set `child` to the value
    associated with that key. Otherwise, return `false`.

* Return `true`.

### `set()`

> Note: For consistency with other functions whose multi-key overloads were
> added after their single-key versions, `set()` is defined to have a separate
> single-key overload and multi-key overload.

* ```
  set($map, $key, $value)
  ```

  > Intuitively, `set($map, $key, $value)` is equivalent to `merge($map, ($key: $value))`.

  * If `$map` is not a map, throw an error.

  * Let `map` be a copy of `$map`.

  * If `map` has a key that's `==` to `$key`, remove it and its associated value.

  * Associate `$key` with `$value` in `map`.

  * Return `map`.

* ```
  set($map, $args...)
  ```

  > Intuitively, `set($map, $key1, $key2, $value)` is equivalent to `set($map,
  > $key1, set(get($map, $key1), $key2, $value))` with the exception that if any
  > intermediate value isn't set or isn't a map it's replaced with a map.

  * If `$map` is not a map, throw an error.

  * If `$args` has fewer than three elements, throw an error.

  * Let `map` be a copy of `$map`.

  * Let `key` be the first element of `$args`.

  * Let `remaining` be the slice of all elements in `$args` except the first.

  * If `map` has a key that's `==` to `key`:

    * Remove that key and its associated value from `map`.

    * Let `child` be the value that was associated with that key if that value
      is a map, or an empty map otherwise.

  * Otherwise:

    * Let `child` be an empty map.

  * Let `new-child` be the result of calling `set()` with `child` as the first
    argument and the elements of `remaining` as the remaining arguments.

  * Associate `key` with `new-child` in `map`.

  * Return `map`.

### `merge()`

This proposal adds a new overload to the existing `merge()` function with lower
priority than the existing signature.

> This means that the new overload is only called if the existing signature
> doesn't match.

This proposal adds a new overload to the existing `merge()` function:

```
merge($map1, $args...)
```

> Intuitively, `map.merge($map1, $keys..., $map2)` is equivalent to
> `map.set($map1, $keys..., map.merge(map.get($map1, $keys...), $map2))`.

* If `$args` is empty, return `$map1`.

* Let `map2` be the last element of `$args`.

* If either `$map1` or `map2` is not a map, throw an error.

* If `$args` has fewer than two elements, throw an error.

* Let `keys` be a slice of all elements in `$args` except the last.

* Let `sub` be the result of calling `get()` with `$map1` as the first
  argument and the contents of `keys` as the remaining arguments.

* If `sub` is a map:

  * Let `sub-merged` be the result of calling `merge()` with `sub` and `map2` as
    arguments.

* Otherwise:

  * Let `sub-merged` be `map2`.

* Return the result of calling `set()` with `$map1` as the first argument,
  followed by the contents of `keys` as separate arguments, followed by
  `sub-merged`.

### `deep-merge()`

```
deep-merge($map1, $map2)
```

* If `$map1` and `$map2` are not maps, throw an error.

* Let `merged` be a copy of `$map1`.

* For each `new-key`/`new-value` pair in `$map2`:

  * If `merged` has a key `old-key` that's `==` to `new-key`:

    * Let `old-value` be the value associated with `old-key` in `merged`.

    * Remove `old-key`/`old-value` from `merged`.

    * If both `old-value` and `new-value` are maps, set `new-value` to the
      result of calling `deep-merge()` with `old-value` and `new-value`.

  * Associate `new-key` with `new-value` in `merged`.

* Return `merged`.

### `deep-remove()`

```
deep-remove($map, $key, $keys...)
```

> Note: This is explicitly *not* an override of `remove()`, because `remove()`
> already accepts a variable number of arguments as a way of removing multiple
> keys from the same map. This proposal adds a new function rather than adjust
> the existing behavior to avoid backwards-compatibility pain.

> Intuitively, `map.deep-remove($map, $keys..., $last-key)` is equivalent to
> `map.set($map, $keys..., map.remove(map.get($map, $keys...), $last-key)`.

* If `$map` isn't a map, throw an error.

* If `$keys` has no elements:

  * Return the result of calling `map.remove($map, $key)`.

* Otherwise:

  * Let `last-key` be the last element of `$keys`.

  * Let `other-keys` be a list containing `$key` followed by all elements in
    `$keys` except the last.

  * Let `sub` be the result of calling `get()` with `$map` as the first
    argument and the contents of `other-keys` as the remaining arguments.

  * If `sub` is a map with a key `old-key` that's `==` to `last-key`:

    * Set `sub` to a copy of itself.

    * Remove `old-key` and its associated value from `sub`.

    * Return the result of calling `set()` with `$map` as the first argument,
      followed by the contents of `other-keys` as separate arguments, followed
      by `sub`.

  * Otherwise:

    * Return `$map`.
