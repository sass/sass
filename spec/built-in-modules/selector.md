# Selector Module

This built-in module is available from the URL `sass:selector`.

## Table of Contents

* [Procedures](#procedures)
  * [Parse a Selector From a SassScript Object](#parse-a-selector-from-a-sassscript-object)
* [Functions](#functions)
  * [`append()`](#append)
  * [`extend()`](#extend)
  * [`is-superselector()`](#is-superselector)
  * [`nest()`](#nest)
  * [`parse()`](#parse)
  * [`replace()`](#replace)
  * [`simple-selectors()`](#simple-selectors)
  * [`unify()`](#unify)

## Procedures

### Parse a Selector From a SassScript Object

This algorithm takes a SassScript object `selector` and returns an abstract
representation of a CSS selector.

* Set `text` to an empty string.

* If `selector` is a list:

  * If `selector` is bracketed and/or slash-separated, throw an error.

  * If `selector` is space-separated:

    * If `selector` contains any non-string elements, throw an error.

    * Set `text` to the concatenation of each element of `selector`, separated
      by spaces.

  * Otherwise, if `selector` is comma-separated:

    * For each element `complex` of `selector`:

      * If `complex` is a list:

        * If `complex` is bracketed or comma-separated, throw an error.

        * Otherwise, if `complex` contains any non-string elements, throw an error.

        * Otherwise, append the concatenation of each element of `selector`, separated
          by spaces, to `text`.

      * Otherwise, if `complex` is not a string, throw an error.

      * Otherwise, append `complex` to text.

      * Append a comma to `text` unless `complex` is the last element of
        `selector`.
      
* Otherwise, if `selector` is not a string, throw an error.

* Otherwise, set `text` to the contents of `selector`.

* Parse `text` as a selector and return the result.

## Functions

### `append()`

```
append($selectors...)
```

This function is also available as a global function named `selector-append()`.

### `extend()`

```
extend($selector, $extendee, $extender)
```

This function is also available as a global function named `selector-extend()`.

### `is-superselector()`

```
is-superselector($super, $sub)
```

This function is also available as a global function named `is-superselector()`.

### `nest()`

```
nest($selectors...)
```

This function is also available as a global function named `selector-nest()`.

### `parse()`

```
parse($selector)
```

This function is also available as a global function named `selector-parse()`.

### `replace()`

```
replace($selector, $original, $replacement)
```

This function is also available as a global function named `selector-replace()`.

### `simple-selectors()`

```
simple-selectors($selector)
```

This function is also available as a global function named `simple-selectors()`.

### `unify()`

```
unify($selector1, $selector2)
```

This function is also available as a global function named `selector-unify()`.

