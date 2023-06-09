# Deprecations API: Draft 3

*([Issue](https://github.com/sass/sass/issues/3520),
[Changelog](deprecations-api.changes.md))*

## Table of Contents
* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Exposing the Full `Deprecation` Interface](#exposing-the-full-deprecation-interface)
    * [Formally Specifying the Deprecations](#formally-specifying-the-deprecations)
    * [Warnings for Invalid Deprecations and Precedence of Options](#warnings-for-invalid-deprecations-and-precedence-of-options)
* [API](#api)
* [Types](#types)
  * [`Options`](#options)
    * [`fatalDeprecations`](#fataldeprecations)
    * [`silenceDeprecations`](#silencedeprecations)
    * [`futureDeprecations`](#futuredeprecations)
  * [`Logger`](#logger)
    * [`warn`](#warn)
  * [`Deprecations`](#deprecations)
    * [`call-string`](#call-string)
    * [`elseif`](#elseif)
    * [`moz-document`](#moz-document)
    * [`relative-canonical`](#relative-canonical)
    * [`new-global`](#new-global)
    * [`color-module-compat`](#color-module-compat)
    * [`slash-div`](#slash-div)
    * [`bogus-combinators`](#bogus-combinators)
    * [`strict-unary`](#strict-unary)
    * [`function-units`](#function-units)
    * [`duplicate-var-flags`](#duplicate-var-flags)
    * [`import`](#import)
    * [`user-authored`](#user-authored)
  * [`DeprecationOrId`](#deprecationorid)
  * [`DeprecationStatus`](#deprecationstatus)
  * [`Deprecation`](#deprecation)
    * [`id`](#id)
    * [`description`](#description)
    * [`deprecatedIn`](#deprecatedin)
    * [`obsoleteIn`](#obsoletein)
  * [`Version`](#version)
    * [Constructor](#constructor)
    * [`major`](#major)
    * [`minor`](#minor)
    * [`patch`](#patch)
    * [`parse`](#parse)
* [Top-Level Members](#top-level-members)
  * [`deprecations`](#deprecations)
* [Embedded Protocol](#embedded-protocol)
  * [CompileRequest](#compilerequest)
    * [`fatal_deprecation`](#fatal_deprecation)
    * [`silence_deprecation`](#silence_deprecation)
    * [`future_deprecation`](#future_deprecation)

## Background

> This section is non-normative.

We recently added support to Dart Sass that allowed users to opt in to
treating deprecation warnings as errors (on a per-deprecation basis), as
well as opting in early to certain future deprecations. This is currently
supported on the command line and via the Dart API, but we'd like to extend
this support to the JS API as well.

We would also like to add support for silencing a particular deprecation's
warnings, primarily to enable a gentler process for deprecating `@import`.

## Summary

> This section is non-normative.

This proposal adds a new `Deprecation` interface and `Version` class to the
JS API, three new optional properties on `Options` (`fatalDeprecations`,
`silenceDeprecations`, and `futureDeprecations`), a new parameter on
`Logger.warn` (`options.deprecationType`) two type aliases (`DeprecationOrId`
and `DeprecationStatus`) and a new object `deprecations` that contains the
various `Deprecation` objects.

All deprecations are specified in `deprecations`, and any new deprecations
added in the future (even those specific to a particular implementation)
should update the specification accordingly. Deprecations should never be
removed from the specification; when the behavior being deprecated is removed
(i.e. there's a major version release), the deprecation status should be
changed to obsolete, but remain in the specification.

Every `Deprecation` has a unique `id`, one of four `status` values, and
(optionally) a human-readable `description`. Depending on the status, each
deprecation may also have a `deprecatedIn` version and an `obsoleteIn`
version that specify the compiler versions the deprecation became active
and became obsolete in, respectively.

### Design Decisions

#### Exposing the Full `Deprecation` Interface

One alternative to specifying a full `Deprecation` interface is to just have
the relevant APIs take in string IDs. We considered this, but concluded that
each deprecation has additional metadata that users of the API may wish to
access (for example, a bundler may wish to surface the `description` and
`deprecatedIn` version to its users).

#### Formally Specifying the Deprecations

We chose to make the list of deprecations part of the specification itself,
as this ensures that the language-wide deprecations are consistent across
implementations. However, if an implementation wishes to add a deprecation
that applies only to itself, it may still do so.

Additionally, while a deprecation's status is part of the specification, we
chose to leave the `deprecatedIn` and `obsoleteIn` versions of each
deprecation out of the specification. As the two current implementers of this
API are both based on Dart Sass, these versions are _currently_ consistent
across implementations in practice, potential future implementers should not
need to be tied to Dart Sass's versioning.

#### Warnings for Invalid Deprecations and Precedence of Options

Whenever potentially invalid sets of deprecations are passed to any of the
options, we choose to emit warnings rather than errors, as the status of
each deprecation can change over time, and users may share a configuration
when compiling across multiple implementations/versions whose dependency
statuses may not be in sync.

The situations we chose to warn for are:

* an invalid string ID.

  This is disallowed by the API's types, but may still occur at runtime,
  and should be warned for accordingly.

* a future deprecation is passed to `fatalDeprecations` but not
  `futureDeprecations`.

  In this scenario, the future deprecation will still be treated as fatal,
  but we want to warn users to prevent situtations where a user tries to
  make every deprecation fatal and ends up including future ones too.

* an obsolete deprecation is passed to `fatalDeprecations`.

  If a deprecation is obsolete, that means the breaking change has already
  happened, so making it fatal is a no-op.

* passing anything other than an active deprecation to `silenceDeprecations`.

  This is particularly important for obsolete deprecations, since otherwise
  users may not be aware of a subtle breaking change for which they were
  previously silencing warnings. We also warn for passing
  `Deprecation.userAuthored`, since there's no way to distinguish between
  different deprecations from user-authored code, so silencing them as a
  group is inadvisable. Passing a future deprecation here is either a no-op,
  or cancels out passing it to `futureDeprecations`, so we warn for that as
  well.

* passing a non-future deprecation to `futureDeprecations`.

  This is a no-op, so we should warn users so they can clean up their
  configuration.

## API

```ts
import {SourceSpan} from '../spec/js-api';
```

## Types

### `Options`

```ts
declare module '../spec/js-api' {
  interface Options<sync extends 'sync' | 'async'> {
```

#### `fatalDeprecations`

A set of deprecations to treat as fatal.

If a deprecation warning of any provided type is encountered during compilation,
the compiler must error instead.

The compiler should convert any string passed here to a `Deprecation` by
indexing `deprecations`. If an invalid deprecation ID is passed here, the
compiler must emit a warning. If a version is passed here, it should be treated
equivalently to passing all active deprecations whose `deprecatedIn` version is
less than or equal to it.

The compiler must emit a warning if a future deprecation that's not also
included in `futureDeprecations` or any obsolete deprecation is included here.

If a deprecation is passed both here and to `silenceDeprecations`, a warning
must be emitted, but making the deprecation fatal must take precedence.

```ts
fatalDeprecations?: (DeprecationOrId | Version)[];
```

#### `silenceDeprecations`

A set of active deprecations to ignore.

If a deprecation warning of any provided type is encountered during compilation,
the compiler must ignore it.

The compiler should convert any string passed here to a `Deprecation` by
indexing `Deprecations`. If an invalid deprecation ID is passed here, the
compiler must emit a warning.

The compiler must emit a warning if any non-active deprecation is included here.
If a future deprecation is included both here and in `futureDeprecations`, then
silencing it takes precedence.

```ts
silenceDeprecations?: DeprecationOrId[];
```

#### `futureDeprecations`

A set of future deprecations to opt into early.

For each future deprecation provided here, the compiler must treat that
deprecation as if it is active, emitting warnings as necessary (subject to
`fatalDeprecations` and `silenceDeprecations`).

The compiler should convert any string passed here to a `Deprecation` by
indexing `Deprecations`. If an invalid deprecation ID is passed here, the
compiler must emit a warning.

The compiler must emit a warning if a non-future deprecation is included here.

```ts
futureDeprecations?: DeprecationOrId[];
```

```ts
} // Options
```

### `Logger`

```ts
interface Logger {
```

#### `warn`

Update the third sub-bullet of bullet two to read:

If this warning is caused by behavior that used to be allowed but will be
disallowed in the future, set `options.deprecation` to `true` and set
`options.deprecationType` to the relevant `Deprecation`. Otherwise, set
`options.deprecation` to `false` and leave `options.deprecationType` undefined.

```ts
warn?(
  message: string,
  options: {
    deprecation: boolean;
    deprecationType?: Deprecation;
    span?: SourceSpan;
    stack?: string;
  }
): void;
```

```ts
  } // Logger
} // module
```

### `Deprecations`

```ts
interface Deprecations {
```

#### `call-string`

Deprecation for passing a string to `call` instead of `get-function`.

```ts
'call-string': Deprecation<'call-string'>;
```

#### `elseif`

Deprecation for `@elseif`.

```ts
elseif: Deprecation<'elseif'>;
```

#### `moz-document`

Deprecation for parsing `@-moz-document`.

```ts
'moz-document': Deprecation<'moz-document'>;
```

#### `relative-canonical`

Deprecation for importers using relative canonical URLs.

```ts
'relative-canonical': Deprecation<'relative-canonical'>;
```

#### `new-global`

Deprecation for declaring new variables with `!global`.

```ts
'new-global': Deprecation<'new-global'>;
```

#### `color-module-compat`

Deprecation for certain functions in the color module matching the
behavior of their global counterparts for compatibility reasons.

```ts
'color-module-compat': Deprecation<'color-module-compat'>;
```

#### `slash-div`

Deprecation for treaing `/` as division.

Update the proposal for forward slash as a separator to say that it emits
deprecation warnings with ID 'slash-div'.

```ts
'slash-div': Deprecation<'slash-div'>;
```

#### `bogus-combinators`

Deprecation for leading, trailing, and repeated combinators.

Update the proposal for bogus combinators to say that it emits deprecation
warnings with ID 'bogus-combinators'.

```ts
'bogus-combinators': Deprecation<'bogus-combinators'>;
```

#### `strict-unary`

Deprecation for ambiguous `+` and `-` operators.

Update the proposal for strict unary operators to say that it emits deprecation
warnings with ID 'strict-unary'.

```ts
'strict-unary': Deprecation<'strict-unary'>;
```

#### `function-units`

Deprecation for passing invalid units to certain built-in functions.

Update the proposals for function units, random with units, and angle units to
say that they emit deprecation warnings with ID 'function-units'.

```ts
'function-units': Deprecation<'function-units'>;
```

#### `duplicate-var-flags`

Deprecation for using multiple `!global` or `!default` flags on a single
variable.

> This deprecation was never explicitly listed in a proposal.

```ts
'duplicate-var-flags': Deprecation<'duplicate-var-flags'>;
```

#### `import`

Deprecation for `@import` rules.

Update the proposal for the module system to say that, when `@import` is
deprecated, Sass will emit deprecation warnings with ID 'import' when `@import`
rules are encountered.

```ts
import: Deprecation<'import'>;
```

#### `user-authored`

Used for deprecations coming from user-authored code.

```ts
'user-authored': Deprecation<'user-authored', 'user'>;
```

```ts
} // Deprecations
```

### `DeprecationOrId`

A deprecation, or the ID of one.

```ts
export type DeprecationOrId = Deprecation | keyof Deprecations;
```

### `DeprecationStatus`

A deprecation's status.

```ts
export type DeprecationStatus = 'active' | 'user' | 'future' | 'obsolete';
```

### `Deprecation`

A deprecated feature in the language.

```ts
export interface Deprecation<
  id extends keyof Deprecations = keyof Deprecations,
  status extends DeprecationStatus = DeprecationStatus
> {
```

#### `id`

A kebab-case ID for this deprecation.

```ts
id: id;
```

The status of this deprecation.

* 'active' means this deprecation is currently enabled. `deprecatedIn` is
  non-null and `obsoleteIn` is null.
* 'user' means this deprecation is from user-authored code. Both `deprecatedIn`
  and `obsoleteIn` are null.
* 'future' means this deprecation is not yet enabled. Both `deprecatedIn` and
  `obsoleteIn` are null.
* 'obsolete' means this deprecation is now obsolete, as the feature it was for
  has been fully removed. Both `deprecatedIn` and `obsoleteIn` are non-null.

```ts
status: status;
```

#### `description`

A brief user-readable description of this deprecation.

```ts
description?: string;
```

#### `deprecatedIn`

The compiler version this feature was first deprecated in.

This is implementation-dependent, so versions are not guaranteed to be
consistent between different compilers. For future deprecations, or those
originating from user-authored code, this is null.

```ts
deprecatedIn: status extends 'future' | 'user' ? null : Version;
```

#### `obsoleteIn`

The compiler version this feature was fully removed in, making the deprecation
obsolete.

This is null for active and future deprecations.

```ts
obsoleteIn: status extends 'obsolete' ? Version : null;
```

```ts
} // Deprecation
```

### `Version`

A [semantic version] of the compiler.

[semantic version]: https://semver.org/

```ts
export class Version {
```

#### Constructor

Creates a new `Version` with its `major`, `minor`, and `patch` fields set to the
corresponding arguments.

```ts
constructor(major: number, minor: number, patch: number);
```

#### `major`

The major version.

This must be a non-negative integer.

```ts
readonly major: number;
```

#### `minor`

The minor version.

This must be a non-negative integer.

```ts
readonly minor: number;
```

#### `patch`

The patch version.

This must be a non-negative integer.

```ts
readonly patch: number;
```

#### `parse`

Parses a string in the form "major.minor.patch" into a `Version`.

```ts
static parse(version: string): Version;
```

```ts
} // Version
```

## Top-Level Members

```ts
declare module '../spec/js-api' {
```

### `deprecations`

An object containing all of the deprecations.

```ts
export const deprecations: Deprecations;
```

```ts
} // module
```

## Embedded Protocol

### CompileRequest

#### `fatal_deprecation`

A set of deprecation IDs to treat as fatal.

If a deprecation warning of any provided type is encountered during compilation,
the compiler must respond with a `CompileFailure` instead.

The compiler must emit an event of type `LogEventType.WARNING` if any of the
following is true:
* an invalid deprecation ID is passed
* an obsolete deprecation ID is passed
* a future deprecation ID is passed that is not also passed to
  `future_deprecation`
* a deprecation ID is passed both here and to `silence_deprecation`
  (making it fatal takes precedence)

```proto
repeated string fatal_deprecation = 14;
```

#### `silence_deprecation`

A set of deprecation IDs to ignore.

If a deprecation warning of any provided type is encountered during compilation,
the compiler must ignore it.

The compiler must emit an event of type `LogEventType.WARNING` if an invalid
deprecation ID or any non-active deprecation ID is passed here.

If a future deprecation ID is passed both here and to `future_deprecation`, then
silencing it takes precedence.

```proto
repeated string silence_deprecation = 15;
```

#### `future_deprecation`

A set of future deprecations IDs to opt into early.

For each future deprecation ID provided here, the compiler must treat that
deprecation as if it is active, emitting warnings as necessary (subject to
`fatal_deprecation` and `silence_deprecation`).

The compiler must emit an event of type `LogEventType.WARNING` if an invalid
deprecation ID or any non-future deprecation ID is passed here.

```proto
repeated string future_deprecation = 16;
```
