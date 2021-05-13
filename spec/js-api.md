# JavaScript API

Sass implementations that are available for use via JavaScript must expose the
following JavaScript API. As with the rest of this specification, they must not
add custom extensions that aren't shared across all implementations.

> Having a shared, consistent API makes it easy for users to move between Sass
> implementations with minimal disruption, and for build system plugins to
> seamlessly work with multiple implementations.

As with other sections of this specification, the specification of the JS API is
incomplete, and is added to *lazily*. This means that portions of the spec are
only written when they're necessary as background for new API proposals.

## Table of Contents

* [Importers](#importers)
  * [`this` Context](#this-context)

## Importers

### `this` Context

When running an importer callback, JavaScript's `this` must refer to an object
with the following fields:

* An `options` field that's an object with the following fields:
  * `file`: The `file` option passed to the `render()` call.
  * `data`: The `data` option passed to the `render()` call.
  * `includePaths`: A string that contains the current working directory
    followed by strings passed in the `includePaths` option, separated by `";"`
    on Windows and `":"` elsewhere.
  * `precision`: The number 10.
  * `style`: An integer. The specific semantics of this are left up to the
    implementation. (The reference implementation always returns 1.)
  * `indentType`: The number 1 if the `indentType` option was `tab`. The number
    0 otherwise.
  * `indentWidth`: An integer indicating the number of spaces or tabs emitted by
    the compiler for each level of indentation.
  * `linefeed`: A string indicating the linefeed character or character sequence
    emitted by the compiler at the end of each line.
  * `result`: An object with a `stats` field, whose value is an object with the
    following fields:
    * `start`: The number of milliseconds since the Unix epoch (1 January 1970
      00:00:00 UT) at the point at which the user called `render()`.
    * `entry`: The `file` option passed to the `render()` call, or the string
      `"data"` if no file was passed.

* A `fromImport` field that's `true` if the import came from an `@import`
  statement and `false` otherwise.

  > This allows importers to look for `.import.scss` stylesheets if and only if
  > an `@import` is being resolved.
