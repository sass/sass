# Map Module

This built-in module is available from the URL `sass:map`.

## Table of Contents

* [Functions](#functions)
  * [`deep-merge()`](#deep-merge)
  * [`deep-remove()`](#deep-remove)
  * [`get()`](#get)
  * [`has-key()`](#has-key)
  * [`keys()`](#keys)
  * [`merge()`](#merge)
  * [`remove()`](#remove)
  * [`set()`](#set)
  * [`values()`](#values)

## Functions

### `deep-merge()`

```
deep-merge($map1, $map2)
```

* If `$map1` and `$map2` are not maps, throw an error.

* Let `merged` be an empty map.

* For each `old-key`/`old-value` pair in `$map1`:

  * If `$map2` has a key `new-key` that's `==` to `old-key`:

    * Let `new-value` be the value associated with `new-key` in `$map2`.

    * If both `old-value` and `new-value` are maps, set `new-value` to the
      result of calling `deep-merge()` with `old-value` and `new-value`.

    * Associate `old-key` with `new-value` in `merged`.

  * Otherwise, associate `old-key` with `old-value` in `merged`.

* For each `new-key`/`new-value` pair in `$map2`:

  * If `merged` doesn't have key that's `==` to `new-key`, associate `new-key`
    with `new-value` in `merged`.

* Return `merged`.

> Note that the order of keys in each merged map is the same as the keys in
> `$map1`, with any new keys from `$map2` added at the end in the same order
> they appear in `$map2`. This matches the ordering of the `merge()` function.

### `deep-remove()`

```
deep-remove($map, $key, $keys...)
```

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

### `get()`

This function is also available as a global function named `map-get()`.

* ```
  get($map, $key)
  ```

* ```
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

This function is also available as a global function named `map-has-key()`.

* ```
  has-key($map, $key)
  ```

* ```
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

### `keys()`

```
keys($map)
```

This function is also available as a global function named `map-keys()`.

### `merge()`

This function is also available as a global function named `map-merge()`.

* ```
  merge($map1, $map2)
  ```

* ```
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

### `remove()`

```
remove($map, $key, $keys...)
```

This function is also available as a global function named `map-remove()`.

### `set()`

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

### `values()`

```
values($map)
```

This function is also available as a global function named `map-values()`.
