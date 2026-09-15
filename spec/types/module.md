# Modules

## Table of Contents

* [Types](#types)
  * [Operations](#operations)
    * [Equality](#equality)
  * [Serialization](#serialization)

## Types

The value type known as a "module" is a [module object].

[module object]: ../modules.md#module

### Operations

The only operations permitted for module objects are `not` and `==`. All other
operations throw an error.

#### Equality

Module values use identity equality.

> In practice, this means that any two module values with the same canonical URL
> are equal, because Sass's module-loading operation will return an
> already-loaded module if it exists.

### Serialization

To serialize a module:

* If the value is not being inspected, throw an error.

* Let `namespace` be the result of [determining the namespace] for a `@use` rule
  whose URL is the module's canonical URL.

* If `namespace` is null, emit `"get-module()"`.

* Otherwise:

  * Emit `"get-module("`.

  * Emit `namespace`, serialized as a quoted string.

  * Emit `")"`.

[determining the namespace]: ../at-rules/use.md#determining-a-use-rules-namespace
