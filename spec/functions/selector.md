# Selector Functions

## Table of Contents

* [Procedures](#procedures)
  * [Parse a Selector From a SassScript Object](#parse-a-selector-from-a-sassscript-object)

## Procedures

### Parse a Selector From a SassScript Object

This algorithm takes a SassScript object `selector` and returns an abstract
representation of a CSS selector.

* Set `text` to an empty string.

* If `selector` is a list:

  * If `selector` is bracketed, throw an error.

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
