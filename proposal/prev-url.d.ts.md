# Containing URL: Draft 2.0

*([Issue](https://github.com/sass/sass/issues/3247))*

```ts
import {PromiseOr} from '../spec/js-api/util/promise_or';
```

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Invariants](#invariants)
    * [Risks](#risks)
    * [Alternatives Considered](#alternatives-considered)
      * [Unavailable for Pre-Resolved Loads](#unavailable-for-pre-resolved-loads)
      * [Unavailable for Absolute Loads](#unavailable-for-absolute-loads)
* [Types](#types)
  * [`FileImporter`](#fileimporter)
    * [`findFileUrl`](#findfileurl)
  * [`Importer`](#importer)
    * [`nonCanonicalScheme`](#noncanonicalscheme)
    * [`canonicalize`](#canonicalize)
* [Embedded Protocol](#embedded-protocol)
  * [`Importer`](#importer-1)
    * [`non_canonical_scheme`](#non_canonical_scheme)
  * [`CanonicalizeRequest`](#canonicalizerequest)
    * [`containing_url`](#containing_url)
  * [`CanonicalizeResponse`](#canonicalizeresponse)
    * [`url`](#url)
  * [`FileImportRequest`](#fileimportrequest)
    * [`containing_url`](#containing_url-1)

## Background

> This section is non-normative.

Among many other changes, the [new importer API] dropped an importer's ability
to access the URL of the stylesheet that contained the load, known in the legacy
API as the "previous URL". This was an intentional design choice which enforced
the invariant that the same canonical URL always refers to the same file.

[new import API]: ../accepted/new-js-importer.d.ts

However, this restriction makes it difficult for importers to work as expected
in certain contexts. For example, in the Node.js ecosystem JS loads depend on
the structure of the `node_modules` directory closest to the containing file.
The new import API can't match this behavior.

This is particularly problematic for the widely-used Webpack importer, which
expands on the concept of directory-specific load contexts to allow users to do
fine-grained customization of how different files will load their dependencies.
In order to ease migration to the new API for this plugin and its users, and to
better match external ecosystems' load semantics, a solution is needed.

## Summary

> This section is non-normative.

This proposal adds an additional option to the `Importer.canonicalize()` API
that provides the canonical URL of the containing file (the "containing URL").
However, in order to preserve the desired invariants, this option is only
provided when either:

- `Importer.canonicalize()` is being passed a relative URL (which means the URL
  has already been tried as a load relative to the current canonical URL), or

- `Importer.canonicalize()` is passed an absolute URL whose scheme the importer
  has declared as non-canonical.

A "non-canonical" scheme is a new concept introduced by this proposal.
Importers will optionally be able to provide a `nonCanonicalScheme` field which
will declare one or more URL schemes that they'll never return from
`canonicalize()`. (If they do, Sass will throw an error.)

### Design Decisions

#### Invariants

The specific restrictions for this API were put in place to preserve the
following invariants:

1. There must be a one-to-one mapping between canonical URLs and stylesheets.
   This means that even when a user loads a stylesheet using a relative URL,
   that stylesheet must have an absolute canonical URL associated with it *and*
   loading that canonical URL must return the same stylesheet. This means that
   any stylesheet can *always* be unambiguously loaded using its canonical URL.

2. Relative URLs are resolved like paths and HTTP URLs. For example, within
   `scheme:a/b/c.scss`, the URL `../d` should be resolved to `scheme:a/d`.

3. Loads relative to the current stylesheet always take precedence over loads
   from importers, so if `scheme:a/b/x.scss` exists then `@use "x"` within
   `scheme:a/b/c.scss` will always load it.

#### Risks

Providing access to the containing URL puts these invariants at risk in two ways:

1. Access to the containing URL in addition to a canonical URL makes it possible
   for importer authors to handle the same canonical URL differently depending
   in different contexts, violating invariant (1).

2. It's likely that importer authors familiar with the legacy API will
   incorrectly assume that any containing URL that exists is the best way to
   handle relative loads, since the only way to do so in the legacy API was to
   manually resolve them relative to the `prev` parameter. Doing so will almost
   certainly lead to violations of invariant (3) and possibly (2).

#### Alternatives Considered

To mitigate these risks, we need to have _some_ restriction on when the
containing URL is available to importers. We considered the following
alternative restrictions before settling on the current one:

##### Unavailable for Pre-Resolved Loads

**Don't provide the containing URL when the `canonicalize()` function is called
for pre-resolved relative loads.** When the user loads a relative URL, the Sass
compiler first resolves that URL against the current canonical URL and passes
the resulting absolute URL to the current importer's `canonicalize()` function.
This invocation would not have access to the containing URL; all other
invocations would, including when Sass passes the relative URL as-is to
`canonicalize()`.

This mitigates risk (2) by ensuring that all relative URL resolution is handled
by the compiler by default. The importer will be invoked with an absolute URL
and no containing URL first for each relative load, which will break for any
importers that naïvely try to use the containing URL in all cases.

This has several drawbacks. First, a badly-behaved importer could work around
this by returning `null` for all relative loads and then manually resolving
relative URLs as part of its load path resolution, thus continuing to violate
invariant (3). Second, this provides no protection against risk (1) since the
stylesheet author may still directly load a canonical URL.

##### Unavailable for Absolute Loads

**Don't provide the containing URL when the `canonicalize()` function is being
called for any absolute URL.** Since relative loads always pass absolute URLs to
their importers first, this is a superset of "Unavailable for Pre-Resolved
Loads". In addition, it protects against risk (1) by ensuring that all absolute
URLs (which are a superset of canonical URLs) are canonicalized without regard
to context.

However, this limits the functionality of importers that use a custom URL scheme
for *non-canonical* URLs. For example, if we choose to support [package imports]
by claiming the `pkg:` scheme as a "built-in package importer", implementations
of this scheme wouldn't be able to do context-sensitive resolution. This would
make the scheme useless for supporting Node-style resolution, a core use-case.
Given that we want to encourage users to use URL schemes rather than relative
URLs, this is a blocking limitation.

[package imports]: https://github.com/sass/sass/issues/2739

Thus we arrive at the actual behavior, which makes the containing URL
unavailable for absolute loads _unless_ they have a URL scheme declared
explicitly non-canonical. This supports the `pkg:` use-case while still
protecting against risk (1), since the containing URL is never available for
canonical resolutions.

## Types

```ts
declare module '../spec/js-api/importer' {
```

### `FileImporter`

Replace [the invocation of `findFileUrl`] with:

[the invocation of `findFileUrl`]:../spec/js-api/importer.d.ts.md#fileimporter

* Let `containingUrl` be the canonical URL of the [current source file] if it
  has one, or undefined otherwise.

  [current source file]: ../spec/spec.md#current-source-file

* Let `url` be the result of calling `findFileUrl` with `string`, `fromImport`,
  and `containingUrl`. If it returns a promise, wait for it to complete and use
  its value instead, or rethrow its error if it rejects.

```ts
interface FileImporter<sync extends 'sync' | 'async' = 'sync' | 'async'> {
```

#### `findFileUrl`

```ts
findFileUrl(
  url: string,
  options: {fromImport: boolean, containingUrl?: URL}
): PromiseOr<URL | null, sync>;
```

```ts
} // FileImporter
```

### `Importer`

Replace the first two bullet points for [invoking an importer with a string]
with:

[invoking an importer with a string]: ../spec/js-api/importer.d.ts.md#importer

* Let `fromImport` be `true` if the importer is being run for an `@import` and
  `false` otherwise.

* If `string` is a relative URL, or if it's an absolute URL whose scheme is
  non-canonical for this importer, let `containingUrl` be the canonical URL of
  the [current source file]. Otherwise, or if the current source file has no
  canonical URL, let `containingUrl` be undefined.

* Let `url` be the result of calling `canonicalize` with `string`, `fromImport`,
  and `containingUrl`. If it returns a promise, wait for it to complete and use
  its value instead, or rethrow its error if it rejects.

* If the scheme of `url` is [non-canonical][non-canonical-js] for this importer,
  throw an error.

  [non-canonical-js]: #noncanonicalscheme

```ts
interface Importer<sync extends 'sync' | 'async' = 'sync' | 'async'> {
```

#### `nonCanonicalScheme`

The set of URL schemes that are considered *non-canonical* for this importer. If
this is a single string, treat it as a list containing that string.

Before beginning compilation, throw an error if any element of this contains a
character other than a lowercase ASCII letter, an ASCII numeral, U+002B (`+`),
U+002D (`-`), or U+002E (`.`).

> Uppercase letters are normalized to lowercase in the `URL` constructor, so for
> simplicity and efficiency we only allow lowercase here.

```ts
nonCanonicalScheme?: string | string[];
```

#### `canonicalize`

```ts
  canonicalize(
    url: string,
    options: {fromImport: boolean; containingUrl?: URL}
  ): PromiseOr<URL | null, sync>;
}
```

```ts
} // Importer
```

## Embedded Protocol

### `Importer`

#### `non_canonical_scheme`

The set of URL schemes that are considered *non-canonical* for this importer.
This must be empty unless `importer.importer_id` is set.

If any element of this contains a character other than a lowercase ASCII letter,
an ASCII numeral, U+002B (`+`), U+002D (`-`), or U+002E (`.`), the compiler must
treat the compilation as failed.

```proto
repeated string non_canonical_scheme = 4;
```

### `CanonicalizeRequest`

#### `containing_url`

The canonical URL of the [current source file] that contained the load to be
canonicalized.

The compiler must set this if and only if `url` is relative or its scheme is
[non-canonical][non-canonical-proto] for the importer being invoked, unless the
current source file has no canonical URL.

[non-canonical-proto]: #non_canonical_scheme

```proto
optional string containing_url = 6;
```

### `CanonicalizeResponse`

#### `url`

If this URL's scheme is [non-canonical][non-canonical-proto] for this importer,
the compiler must treat that as an error thrown by the importer.

### `FileImportRequest`

Replace the sending of `FileImportRequest` with:

* Let `containingUrl` be the canonical URL of the [current source file] if it
  has one, or undefined otherwise.

* Let `response` be the result of sending a `FileImportRequest` with `string` as
  its `url`, `fromImport` as `from_import`, and `containingUrl` as
  `containing_url`.

#### `containing_url`

The canonical URL of the [current source file] that contained the load to be
canonicalized. The compiler must set this unless the current source file has no
canonical URL.

```proto
optional string containing_url = 6;
```
