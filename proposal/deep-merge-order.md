# Deep Merge Order: Draft 1

*([Issue](https://github.com/sass/sass/issues/3092))*

This proposal changes the ordering of maps returned by `map.deep-merge()` to
match that returned by `map.merge()`.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Whether to Specify Order](#whether-to-specify-order)
* [Functions](#functions)
  * [`map.deep-merge()`](#mapdeep-merge)
* [Deprecation Process](#deprecation-process)

## Background

> This section is non-normative.

When `map.deep-merge()` was first discussed in [issue 1739] and later [added to
the spec], their ordering wasn't explicitly discussed. In practice, the ordering
implied by the original specification put any keys that appeared in both maps at
the end of the result, in the order they appeared in `$map2`. This was different
than the ordering produced by the `map.merge()` function in a way that confused
users.

[issue 1739]: https://github.com/sass/sass/issues/1739
[added to the spec]: ../accepted/nested-map-functions.md

## Summary

> This section is non-normative.

This proposal changes the `map.deep-merge()` function to match the ordering of
`map.merge()`, in which all keys in `$map1` appear in the result the same order
they did in `$map1` (whether or not they're in `$map2`), followed by all keys
that are only in `$map2` in the same relative order as in `$map2`. For example:

* `map.deep-merge((a: 1, b: 1), (b: 2, c: 2))` produces `(a: 1, b: 2, c: 2)` in
  both the current spec and this proposal.

* `map.deep-merge((a: 1, b: 1), (a: 2, c: 2))` produces `(b: 1, a: 2, c: 2)` in
  the current spec but `(a: 2, b: 1, c: 2)` in this proposal.

### Design Decisions

#### Whether to Specify Order

Rather than change the specified order of map entries, we considered updating
the specification to explicitly make the order an implementation detail. This
would have the advantage of allowing implementations to choose a more performant
ordering in the future if, for example, they used an immutable representation of
maps that could re-use internal data structures.

However, because in practice there's currently only one recommended
implementation of Sass, its behavior would still end up being the *de facto*
standard. In addition, users clearly desire an intuitive map ordering and
there's not clear evidence that any performance gains would be substantial
enough to warrant breaking that intuition.

## Functions

Replace the definition of the `deep-merge()` function in the `sass:map` built-in
module with the following definition:

### `map.deep-merge()`

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

## Deprecation Process

This is technically a breaking change, since stylesheets could be relying on the
current ordering of `map.deep-merge()`. However, there are several reasons why a
standard deprecation process isn't a good fit here:

* There isn't a good way to deprecate the old behavior non-disruptively. If we
  support the old behavior as written, the new behavior would need to be awkward
  to use.

* The breaking change is small. Output ordering is not a core part of
  `map.deep-merge()`'s behavior and is unlikely to be something anyone is
  relying on in practice.

* `map.deep-merge()` is relatively young, which means that there are not as many
  Sass stylesheets using it (and thus relying on every aspect of its behavior)
  as there are using older behaviors.

* The change is a clear improvement in terms of consistency with `map.merge()`
  and with merge behavior in other languages. It could even be argued as a bug
  fix. Any pain caused by this change is likely to be mitigated by the pain due
  to confusing ordering it will prevent.
