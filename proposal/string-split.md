# Add split function to Strings Module: Draft 1

*([Issue](https://github.com/sass/sass/issues/1950))*

This proposal adds `string.split()` to the `sass:string` module.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Semantics](#semantics)
  * [`split()`](#split)

## Background

> This section is non-normative.

The `sass:string` module contains several functions for manipulating and finding 
out information about strings. Currently, though, there is no built-in function 
that splits one string into a list of substrings, and authors have been creating 
their own versions of functions that achieve this functionality.

## Summary

> This section is non-normative.

This proposal adds the `string.split()` function to the `sass:string` module. 
The function takes a string, splits it based on a provided separator, and 
returns an unbracketed, comma-separated list of substrings.

This could be used to take a string and repurpose parts of it for some other 
use. For example, fonts contained in a font stack list could be split into 
segments and then used as keys in a new map. 

Examples:

```scss
$fonts: "Helvetica Neue, Helvetica, Arial";
string.split($fonts, ', '); // "Helvetica Neue", "Helvetica", "Arial"
```

A third argument can set a limit to the the number of splits performed on the 
string:

```scss
string.split($fonts, ', ', 1); // "Helvetica Neue", "Helvetica, Arial"
```


An empty `$separator` returns all Unicode code points in the original string:

```scss
$font: "Helvetica"
string.split($font, ''); // "H", "e", "l", "v", "e", "t", "i", "c", "a"
```


## Semantics

### `split()`

```
split($string, $separator, $limit: null)
```

* If `$string` is not a string, throw an error.

* If `$separator` is `null` or is not a string, throw an error.

* If `$limit` is a value other than an integer or `null`, throw an error.

* If `$limit` is a negative number, throw an error.

* If `$string` is an empty string, return a list with `$string` as the only 
  item.

* If `$limit` is 0, return a list with `$string` as the only item.

* Let `split-list` be an empty list.

* Let `length` be the result of calling `string.length($string)`.

* Let `limit` be the value of `$limit`.

* Otherwise, if `$limit` is `null`, set `limit` to the value of `length`.

* Let `split-counter` equal 0.

* While `split-counter` is `<=` `limit` and `length` is `>` 0:

  * If `split-counter` `==` `limit`:

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

      * If `index` is null, append `$string` to `split-list` and set `$string` 
        to an empty string.

      * Otherwise:

        * Let `current-substring` be the result of calling
          `string.slice($string, 1, index - 1)`.

        * Append `current-substring` to `split-list`.
    
        * Set `$string` to 
          `string.slice($string, index + string.length($separator))`.

        * Increase `split-counter` by 1.

  * Set `length` to `string.length($string)`.
      
* Return `split-list` as an unbracketed, comma-separated list.
