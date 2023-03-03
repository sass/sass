# String Module

This built-in module is available from the URL `sass:string`.

## Table of Contents

* [Functions](#functions)
  * [`index()`](#index)
  * [`insert()`](#insert)
  * [`length()`](#length)
  * [`quote()`](#quote)
  * [`slice()`](#slice)
  * [`split()`](#split)
  * [`to-lower-case()`](#to-lower-case)
  * [`to-upper-case()`](#to-upper-case)
  * [`unique-id()`](#unique-id)
  * [`unquote()`](#unquote)

## Functions


### `index()`

```
index($string, $substring)
```

This function is also available as a global function named `str-index()`.

### `insert()`

```
insert($string, $insert, $index)
```

This function is also available as a global function named `str-insert()`.

### `length()`

```
length($string)
```

This function is also available as a global function named `str-length()`.

### `quote()`

```
quote($string)
```

This function is also available as a global function named `quote()`.

### `slice()`

```
slice($string, $start-at, $end-at: -1)
```

This function is also available as a global function named `str-slice()`.

### `split()`

```
split($string, $separator, $limit: null)
```

* If `$string` is not a string, throw an error.

* If `$separator` is not a string, throw an error.

* If `$limit` is a value other than an integer or `null`, throw an error.

* If `$limit` is less than 1, throw an error.

* If `$string` is an empty string, return a list with `$string` as the only 
  item.

* Let `split-list` be an empty list.

* If `$limit` is `null`, set `$limit` to the value of calling 
  `string.length($string)`.

* Let `split-counter` equal 0.

* While `split-counter <= $limit` and `string.length($string) > 0`:

  * If `split-counter == $limit`:

    * Append `$string` to `split-list`.

    * Set `$string` to an empty string. 

  * Otherwise:

    * If `$separator` is an empty string:

      * Let `code-point` be the value of calling `string.slice($string, 1, 1)`.

      * Append `code-point` to `split-list`.

      * Set `$string` to `string.slice($string, 2)`.

      * Increase `split-counter` by 1.

    * Otherwise:

      * Let `index` be the result of calling 
        `string.index($string, $separator)`.

      * If `index` is `null`, append `$string` to `split-list` and set `$string` 
        to an empty string.

      * Otherwise:

        * Let `current-substring` be the result of calling
          `string.slice($string, 1, index - 1)`.

        * Append `current-substring` to `split-list`.
    
        * Set `$string` to 
          `string.slice($string, index + string.length($separator))`.

        * Increase `split-counter` by 1.
      
* Return `split-list` as a bracketed, comma-separated list.

### `to-lower-case()`

```
to-lower-case($string)
```

This function is also available as a global function named `to-lower-case()`.

### `to-upper-case()`

```
to-upper-case($string)
```

This function is also available as a global function named `to-upper-case()`.

### `unique-id()`

```
unique-id()
```

This function is also available as a global function named `unique-id()`.

### `unquote()`

```
unquote($string)
```

This function is also available as a global function named `unquote()`.

