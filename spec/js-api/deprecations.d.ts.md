# Deprecations API

> Interfaces for user-declared importers that customize how Sass loads
> stylesheet dependencies.

## Table of Contents

## Types

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

Deprecation for treating `/` as division.

```ts
'slash-div': Deprecation<'slash-div'>;
```

#### `bogus-combinators`

Deprecation for leading, trailing, and repeated combinators.

```ts
'bogus-combinators': Deprecation<'bogus-combinators'>;
```

#### `strict-unary`

Deprecation for ambiguous `+` and `-` operators.

```ts
'strict-unary': Deprecation<'strict-unary'>;
```

#### `function-units`

Deprecation for passing invalid units to certain built-in functions.

```ts
'function-units': Deprecation<'function-units'>;
```

#### `duplicate-var-flags`

Deprecation for using multiple `!global` or `!default` flags on a single
variable.

```ts
'duplicate-var-flags': Deprecation<'duplicate-var-flags'>;
```

#### `import`

Deprecation for `@import` rules.

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

### `deprecations`

An object containing all of the deprecations.

```ts
export const deprecations: Deprecations;
```
