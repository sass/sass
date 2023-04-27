# List API

```ts
import {List} from 'immutable';

import {Value} from './index';
```

## Table of Contents

* [Types](#types)
  * [`ListSeparator`](#listseparator)
  * [`SassList`](#sasslist)
    * [`internal`](#internal)
    * [Constructor](#constructor)
    * [`separator`](#separator)

## Types

### `ListSeparator`

The JS API representation of a Sass list separator. null represents the
undecided separator type.

```ts
export type ListSeparator = ',' | '/' | ' ' | null;
```

### `SassList`

The JS API representation of a Sass list.

```ts
export class SassList extends Value {
```

#### `internal`

The [private `internal` field] refers to [a Sass list].

[private `internal` field]: index.d.ts.md#internal
[a Sass list]: ../../types/list.md

#### Constructor

Creates a Sass list:

* If the first argument is an `Array` or a `List`:
  * Let `contents` be the first argument.
  * Let `options` be the second argument, or `{}` if it's undefined.

* Otherwise:
  * Let `contents` be `[]`.
  * Let `options` be the first argument, or `{}` if it's undefined.

* Let `separator` be `options.separator`, or `','` if that's undefined.

* Let `brackets` be `options.brackets`, or `false` if that's undefined.

* Set `internal` to a Sass list with contents set to `contents`, separator set
  to `separator`, and brackets set to `brackets`.

* Return `this`.

```ts
constructor(
  contents: Value[] | List<Value>,
  options?: {
    separator?: ListSeparator;
    brackets?: boolean;
  }
);

constructor(options?: {separator?: ListSeparator; brackets?: boolean});
```

#### `separator`

[`internal`]'s list separator.

[`internal`]: #internal

```ts
get separator(): ListSeparator;
```

```ts
} // SassList
```
