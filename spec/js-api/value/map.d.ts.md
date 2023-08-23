# Map API

```ts
import {OrderedMap} from 'immutable';

import {SassList} from './list';
import {Value} from './index';
```

## Table of Contents

* [Types](#types)
  * [`SassMap`](#sassmap)
    * [`internal`](#internal)
    * [Constructor](#constructor)
    * [`contents`](#contents)
    * [`get`](#get)
    * [`tryMap`](#trymap)

## Types

### `SassMap`

The JS API representation of a Sass map.

```ts
export class SassMap extends Value {
```

#### `internal`

The [private `internal` field] refers to a Sass map.

[private `internal` field]: index.d.ts.md#internal

#### Constructor

Creates a Sass map:

* If `contents` is undefined, set it to an empty `OrderedMap`.
* Set `internal` to a Sass map with contents set to `contents`.
* Return `this`.

```ts
constructor(contents?: OrderedMap<Value, Value>);
```

#### `contents`

Returns a map containing `internal`'s contents:

* Let `result` be an empty `OrderedMap`.
* Add each key and value from `internal`'s contents to `result`, in order.
* Return `result`.

```ts
get contents(): OrderedMap<Value, Value>;
```

#### `get`

* If the first argument is a JavaScript number, pass it to `this.asList.get` and
  return the result.

* Otherwise, pass it to `this.contents.get` and return the result.

```ts
get(key: Value): Value | undefined;

get(index: number): SassList | undefined;
```

#### `tryMap`

```ts
tryMap(): SassMap;
```

```ts
} // SassMap
```
