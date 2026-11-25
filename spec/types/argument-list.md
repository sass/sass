# Strings

## Table of Contents

* [Types](#types)

## Types

The value type known as an *argument list* has two components:

* A list of values known called its *positional arguments*.
* A mapping from the [semantic values] of argument names those those arguments'
  values, called its *named arguments*.

[semantic values]: string.md#getting-the-semantic-value-of-a-string

An argument list counts as an unbracketed, comma-separated [list] whose values
are its positional arguments unless otherwise specified.

[list]: list.md

> Attempting to create an argument list one of whose named arguments is an
> [anti-identifier] will throw an error.

[anti-identifier]: string.md#anti-identifier
