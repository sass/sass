# Argument List API

```ts
import {List, OrderedMap} from 'immutable';

import {Value} from './index';
import {SassList, ListSeparator} from './list';
```

## Table of Contents

* [Types](#types)
  * [`SassArgumentList`](#sassargumentlist)
    * [`internal`](#internal)
    * [Constructor](#constructor)
    * [`keywords`](#keywords)

## Types

### `SassArgumentList`

The JS API representation of a Sass argument list.

```ts
export class SassArgumentList extends SassList {
```

#### `internal`

The [private `internal` field] refers to a Sass argument list.

[private `internal` field]: index.d.ts.md#internal

#### Constructor

Creates a Sass argument list:

* Set `internal` to a Sass argument list with contents set to `contents`,
  keywords set to `keywords`, and list separator set to `separator`.

* Return `this`.

```ts
constructor(
  contents: Value[] | List<Value>,
  keywords: Record<string, Value> | OrderedMap<string, Value>,
  /** @default ',' */
  separator?: ListSeparator
);
```

#### `keywords`

`internal`'s keywords.

```ts
get keywords(): OrderedMap<string, Value>;
```

```ts
} // SassArgumentList
```
