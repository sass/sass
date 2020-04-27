# Nested Map Functions: Draft 1.0

*[(Issue)](https://github.com/sass/sass/issues/1739)*

This proposal updates the built-in `sass:map` module to better support merging,
setting, and getting items from nested maps.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Functions](#functions)
  * [`get()`](#get)
  * [`has-key()`](#has-key)
  * [`set()`](#set)
  * [`merge()`](#merge)
  * [`deep-merge()`](#deep-merge)

## Background

> This section is non-normative.

Variables have always been a key feature of the Sass language. But these days,
design systems and component libraries form the basis of most CSS projects --
with well organized _design tokens_ as the foundation. While Individual token
variables can be quite useful, the ability to group tokens into strucutred and
meaningful relationships is essential for creating resilient systems.

There are many ways to group tokens. The popular Style Dictionary recommends
a deep nesting of _category_,  _type_, _item_, _sub-item_, and _state_. Other
taxonomies also include concepts like _theme_, or even _operating system_. Most
of the existing tools rely on YAML or JSON objects to achieve that nested
structure, at the expense of other important information. YAML and JSON are not
design languages, and do not understand fundamental CSS concepts like color or
length.

With Sass, we don't have to make that tradeoff. We already support nestable map
structures, and the ability to interact with them programmatically -- adding or
removing properties, accessing values, and looping over entire structures. But
current built-in functions don't provide much support for managing nested maps.
Projects often build thir own tooling.

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

### `get()`

```
get($map, $keys...)
```

* If `$map` is not a map, throw an error.

* If `$keys` is empty, throw an error.

* Let `key` be the first (or only) element in `$keys`

* If `$map` does not have a key equal to `key`, return `null`.

* Let `value` be the value assigned to `key` in `$map`

* If there is more than one element in `$keys`:

  * Let `keys` be all elements in `$keys` after the first element

  * Call `get()` with `value` and expanded `keys` as arguments

* Otherwise, return `value`

### `has-key()`

```
has-key($map, $keys...)
```

* If `$map` is not a map, throw an error.

* If `$keys` is empty, throw an error.

* Let `key` be the first (or only) element in `$keys`

* If `$map` does not have a key with the same name as `key`, return boolean
  `false`.

* If there is more than one element in `$keys`:

  * Let `value` be the value assigned to `key` in `$map`

  * If `value` is not a map, return boolean `false`.

  * Let `keys` be all but the first element in `$keys`

  * Call `has-key()` with `value` and expanded `keys` as arguments

* Otherwise, return boolean `true`

### `set()`

```
set($map, $args...)
```

* If `$map` is not a map, throw an error.

* Let `map` be an empty map.

* Let `set-key` be the first item in arglist `$args`.

* Let `remaining` be a slice of all the other items in arglist `$args`.

* If there are no items in `remaining`, throw an error.

* If there is more than one argument in `remaining`:

  * If `$map` has a key `current-key` that is equal to `set-key`:

    * Let `current` be the value of `current-key`.

  * Otherwise:

    * Let `current` be an empty map.

  * Let `set-value` be the result of calling `set()` with `current` and expanded
    `remaining` as arguments.

    > This will error if `current` is not a map, but we stil have `remaining`

* Otherwise:

  * Let `set-value` be the only item in `remaining`

* Return a copy of `map` with `set-key` set to `set-value`, overriding an
  existing value for `set-key` if one exists.

### `merge()`

This proposal add a new overload to the existing `merge()` function:

```
merge($map1, $args...)
```

* If `$args` is empty, throw an error

* Let `map2` be the last item in arglist `$args`

* If either `$map1` or `map2` is not a map, throw an error.

* If arglist `$args` has more than one item:

  * Let `get-keys` be a slice of all except the last item in `args`.

  * Let `sub` be the result of calling `get()` with `sub` and expanded `get-keys`
    as arguments.

  * Let `sub-merged` be the result of calling `merge()` with `sub` and `map2`
    as argumets.

  * Let `set-args` be the result of appending `sub-merged` to the list `keys`

  * Call `set()` with `$map1` and expanded `set-args` as arguments.

* Otherwise:

  * Let `map` be a copy of `$map1`.

  * For each `key`, `value` in `map2`:

    * Replace `map` with the result of calling `set()` with `map`, `key`, and
      `value` as arguments.

  * Return `map`.

### `deep-merge()`

```
deep-merge($maps...)
```

* If the length of `$maps` is less than two, throw an error.

* If  any of the items in `$maps` are not maps, throw an error.

* Let `merged` be an empty map.

* For each `map` in `maps`:

  * for each `key`, `value` in `map`:

    * If `value` is a map:

      * Let `current` be the result of calling `get()` with `merged` and `key` as
        arguments.

      * If `current` is a map:

        * Let `deep` be the result of calling `deep-merge()` with `current` and
          `value` as arguments.

        * Replace `map` with the result of calling `set()` with `map`, `key`, and
          `deep` as arguments.

  * Replace `merged` with the result of calling `merge` with `merged` and `map`
    as arguments.

* Return `merged`.
