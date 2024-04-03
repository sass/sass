# Deprecations API

> The APIs specifying the various deprecation types recognized by the
> `fatalDeprecations`, `futureDeprecations`, and `silenceDeprecations` options.

## Table of Contents

* [Types](#types)
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
    * [`null-alpha`](#null-alpha)
    * [`abs-percent`](#abs-percent)
    * [`fs-importer-cwd`](#fs-importer-cwd)
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

## Types

### `Deprecations`

```ts
export interface Deprecations {
```

#### `call-string`

```ts
'call-string': Deprecation<'call-string'>;
```

#### `elseif`

```ts
elseif: Deprecation<'elseif'>;
```

#### `moz-document`

```ts
'moz-document': Deprecation<'moz-document'>;
```

#### `relative-canonical`

```ts
'relative-canonical': Deprecation<'relative-canonical'>;
```

#### `new-global`

```ts
'new-global': Deprecation<'new-global'>;
```

#### `color-module-compat`

```ts
'color-module-compat': Deprecation<'color-module-compat'>;
```

#### `slash-div`

```ts
'slash-div': Deprecation<'slash-div'>;
```

#### `bogus-combinators`

```ts
'bogus-combinators': Deprecation<'bogus-combinators'>;
```

#### `strict-unary`

```ts
'strict-unary': Deprecation<'strict-unary'>;
```

#### `function-units`

```ts
'function-units': Deprecation<'function-units'>;
```

#### `duplicate-var-flags`

```ts
'duplicate-var-flags': Deprecation<'duplicate-var-flags'>;
```

#### `null-alpha`

```ts
'null-alpha': Deprecation<'null-alpha'>;
```

#### `abs-percent`

```ts
'abs-percent': Deprecation<'abs-percent'>;
```

#### `fs-importer-cwd`

```ts
'fs-importer-cwd': Deprecation<'fs-importer-cwd'>;
```

#### `import`

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

This is implementation-dependent, so versions are not guaranteed to be
consistent between different compilers. This is null for active and future
deprecations.

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

All of `major`, `minor`, and `patch` must be non-negative integers, or else
the compiler must throw an error.

```ts
constructor(major: number, minor: number, patch: number);
```

#### `major`

The major version.

This will always be a non-negative integer.

```ts
readonly major: number;
```

#### `minor`

The minor version.

This will always be a non-negative integer.

```ts
readonly minor: number;
```

#### `patch`

The patch version.

This will always be a non-negative integer.

```ts
readonly patch: number;
```

#### `parse`

Parses a string in the form "major.minor.patch" into a `Version`.

The compiler must throw an error when `version` cannot be parsed into a valid
`Version`.

```ts
static parse(version: string): Version;
```

```ts
} // Version
```

## Top-Level Members

### `deprecations`

An object containing all of the deprecations.

```ts
export const deprecations: Deprecations;
```
