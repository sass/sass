# Source Location

## Table of Contents

* [Types](#types)
  * [`SourceLocation`](#sourcelocation)
    * [`offset`](#offset)
    * [`line`](#line)
    * [`column`](#column)

## Types

### `SourceLocation`

An interface that represents a location in a text file.

```ts
export interface SourceLocation {
```

#### `offset`

The 0-based offset of this location within the file it refers to, in terms of
UTF-16 code units.

```ts
offset: number;
```

#### `line`

The number of U+000A LINE FEED characters between the beginning of the file and
`offset`, exclusive.

> In other words, this location's 0-based line.

```ts
line: number;
```

#### `column`

The number of UTF-16 code points between the last U+000A LINE FEED character
before `offset` and `offset`, exclusive.

> In other words, this location's 0-based column.

```ts
column: number;
```

```ts
} // SourceLocation
```
