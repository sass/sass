# Deprecations Except: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/4252))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Bonus Breaking Change: Future Versions](#bonus-breaking-change-future-versions)
  * [Command-Line API](#command-line-api)
* [Types](#types)
  * [`DeprecationSelector`](#deprecationselector)
  * [`DeprecationSelectorObject`](#deprecationselectorobject)
    * [`include`](#include)
    * [`exclude`](#exclude)
  * [`Options`](#options)
    * [`fatalDeprecations`](#fataldeprecations)
    * [`futureDeprecations`](#futuredeprecations)
    * [`silentDeprecations`](#silentdeprecations)
  * [`Deprecations`](#deprecations)
  * [`DeprecationsUtil`](#deprecationsutil)
    * [`dartSass2`](#dartsass2)
* [Top-Level Members](#top-level-members)
  * [`deprecations`](#deprecations)
* [Embedded Protocol](#embedded-protocol)
  * [`CompileRequest`](#compilerequest)
* [Deprecation Process](#deprecation-process)
  * [Phase 1](#phase-1)
  * [Phase 2](#phase-2)

## Background

> This section is non-normative.

In [Deprecations API], we added support for new APIs for fine-grained control of
how each deprecation was handled by Sass. This provided a considerable amount of
power to users of the JS API, but as we draw closer to the release of Dart Sass
2 it's becoming clear that there are some places where it's awkward to express
common desirable behaviors.

[Deprecations API]: ../accepted/deprecations-api.md

Dart Sass 2 will make *most* but not *all* deprecations added prior to Dart Sass
1.105.0 into errors, with exceptions in place for `@import`- and `if()`-related
deprecations. While the deprecation API can express "all deprecations up to this
version", it *can't* express "all deprecations up to this version except for
these specifically".

## Summary

> This section is non-normative.

This proposal adds a new `DeprecationSelector` type for the
`Options.fatalDeprecations` field. This allows the existing type
(`(DeprecationOrId | Version)[]`), while also allowing a value of the form
`{include: ..., except: ...}` which selects all deprecations except certain
explicit exceptions. For example, the Dart Sass 2 deprecations can be expressed
as

```
{
  fatalDeprecations: {
    include: sass.Version.parse("1.105.0"),
    except: ["import", "global-builtin", "color-module-compat", "if-function"],
  }
}
```

Because this specific value is likely to be widely used, this adds a new
`sass.deprecations.dartSass2` value which encapsulates it specifically.

As a side effect of the refactoring, this allows `fatalDeprecations` to directly
specify a deprecation or version number without wrapping it in a list. For
consistency, the same is also allowed for `futureDeprecations` and
`silenceDeprecations`.

### Bonus Breaking Change: Future Versions

While investigating the current behavior, we realized that passing a version
*beyond* the current Sass version didn't produce an error. This allows users to
upgrade deprecations *they don't even know about* to errors, which is explicitly
something we don't want to support (and if we did, we'd support it more clearly
as `fatalDeprecations: true`). This proposal makes this an error.

### Command-Line API

> The command-line API isn't actually part of the Sass spec, but it's desirable
> to have it match the capabilities of the API in this case so we include an
> informal outline of the corresponding changes here.

To support this behavior on the command line, we will allow deprecation names to
be prefixed with `-` to exclude them. For example, the Dart Sass 2 deprecations
can be expressed as:

```
--fatal-deprecations:1.105.0,-import,-global-builtin,-color-module-compat,-if-function
```

In addition, we'll support the special syntax `--fatal-deprecations=dart-sass-2`
which will select the specific deprecations that will be enabled in Dart Sass 2.

## Types

```ts
import {Deprecations, DeprecationOrId, Version} from '../spec/js-api';
```

### `DeprecationSelector`

A value that selects a set of [deprecations].

[deprecations]: ../spec/deprecations.yaml

* If this is a `DeprecationOrId`, it selects the indicated deprecation. Emit a
  warning when resolving this `DeprecationSelector` if this deprecation is
  obsolete.

* If this is a `Version`, it selects each deprecation whose `deprecatedIn`
  version is less than or equal to that `Version`. If this `Version` is greater
  than the version of the current implementation, this selector is invalid.

  > It's important that this even selects obsolete deprecations, because
  > otherwise the valid `DeprecationSelectorObject` `{include:
  > sass.Version.parse("1.2.3"), except: "some-deprecation"}` would become
  > invalid as soon as `some-deprecation` became obsolete. We want it to emit a
  > warning instead.

* If this is a `DeprecationSelectorObject`, it selects that selector's deprecations.

* If this is a list, it selects the union of all deprecations selected by any
  element of that list.

```ts
type DeprecationSelector =
  | DeprecationOrId
  | Version
  | DeprecationSelectorObject
  | DeprecationSelector[];
```

### `DeprecationSelectorObject`

An object that selects a set of [deprecations].

```ts
interface DeprecationSelectorObject {
```

#### `include`

The set of deprecations included by this object, except as modified by
[`exclude`].

[`exclude`]: #exclude

```ts
include: DeprecationSelector;
```

#### `exclude`

This object does *not* select this deprecation (for a `DeprecationOrId`) or any
deprecation included in this list (for a `DeprecationOrId[]`). If any
deprecation in this list is not selected by `include`, this selector is invalid.

```ts
exclude: DeprecationOrId | DeprecationOrId[];
```

```ts
} // DeprecationSelectorObject
```

### `Options`

```ts
interface Options {
```

#### `fatalDeprecations`

Replace the first four paragraphs of [`Options.fatalDeprecations`] with:

[`Options.fatalDeprecations`]: ../spec/js-api/options.d.ts.md

If a deprecation warning for any of the selected deprecations would be emitted
during compilation, the compiler must error instead.

The compiler must error if this selector is invalid.

The compiler must emit a warning if a future deprecation is selected, unless
that deprecation is also included in `futureDeprecations`.

```ts
fatalDeprecations?: DeprecationSelector;
```

#### `futureDeprecations`

```ts
futureDeprecations?: DeprecationOrId | DeprecationOrId[];
```

#### `silentDeprecations`

```ts
silentDeprecations?: DeprecationOrId | DeprecationOrId[];
```

```ts
} // Options
```

### `Deprecations`

> Make `Deprecations` implement `Record` so that `Object.values()` and related
> methods become well-typed.

Add `Record<string, Deprecation<string>>` as a supertype of `Deprecations`.

### `DeprecationsUtil`

> This interface exists to declare non-enumerable properties on
> [`sass.deprecations`]. They can't be declared directly on [`Deprecations`]
> because they don't match its `Record` type, and TypeScript doesn't support
> [non-enumerable properties] which would allow this to work.

[`sass.deprecations`]: ../spec/js-api/deprecations.d.ts.md#deprecations
[`Deprecations`]: ../spec/js-api/deprecations.d.ts.md#deprecations
[non-enumerable properties]: https://github.com/microsoft/TypeScript/issues/9726

All properties on this interface are non-enumerable.

```ts
interface DeprecationsUtil {
```

#### `dartSass2`

A `DeprecationSelector` that selects all deprecations whose `active` version is
less than or equal to 1.105.0 other than `import`, `global-builtin`,
`color-module-compat`, and `if-function`.

The specific structure of this value is not specified and may vary between
implementations or versions.

```ts
dartSass2: DeprecationSelector;
```

```ts
} // DeprecationsUtil
```

## Top-Level Members

### `deprecations`

```ts
export const deprecations: Deprecations & DeprecationsUtil;
```

## Embedded Protocol

### `CompileRequest`

Add the field

```proto
// Deprecation IDs to treat as normal deprecations, even if they appear in
// `fatal_deprecations`.
//
// The compiler must throw an error if a deprecation appears here and not in
// `fatal_deprecation` (including deprecations marked fatal via version number).
repeated string fatal_deprecation_except = 18;
```

## Deprecation Process

The deprecation process will be divided into two phases:

### Phase 1

Initially, if a `DeprecationSelector` is a `Version` later than the current
implementation's version, rather than emitting an error, the compiler should
emit a `deprecation-version` deprecation warning.

### Phase 2

Phase 2 impements the full changes described above. Passing a future `Version`
will produce an error.
