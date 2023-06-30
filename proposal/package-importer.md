# Package Importer

*([Issue](https://github.com/sass/sass/issues/2739))*

This proposal adds Sass support for a standard package importer, introducing a
`pkg` URL scheme to indicate Sass package imports in an implementation-agnostic
format.

## Table of contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
* [Semantics](#semantics)
* [Deprecation Process](#deprecation-process)

## Background

> This section is non-normative.

Historically, Sass has not specified a standard method for using packages from
dependencies. A number of domain-specific solutions exist using custom importers
or by specifying a load path. This can lead to Sass code being written in a way
that is tied to a specific domain and make it difficult to rely on dependencies.

## Summary

> This section is non-normative.

Sass users often need to use styles from a dependency to customize an existing
theme or access styling utilities.

This proposal defines a `pkg` URL scheme for usage with `@use` that directs an
implementation to resolve a URL within a dependency. The implementation will
resolve the dependency URL using the standard resolution for that environment.
Once resolved, this URL will be loaded in the same way as any other `file:` URL.

For example, `@use 'pkg:bootstrap';` would be able to resolve to the path of a
library-defined export within the `bootstrap` dependency. In Node, that would be
resolved within `node_modules`, using the [node resolution algorithm]. In Dart,
that would be resolved within `pub-cache`, using [package-config].

[node resolution algorithm]: https://nodejs.org/api/packages.html
[package-config]: https://pub.dev/packages/package_config

There will be tension between the different path resolution algorithms that are
used. To address this, a `pkg:` url will only use the environment's resolution
initially in order to resolve a starting path. After that, it will be handed off
to the Sass algorithm, which works directly with the filesystem, to resolve
index files and partials.

To better understand and allow for testing against the recommended algorithm, a
[Sass pkg: test] repository has been made with a rudimentary implementation of
the algorithm.

[Sass pkg: test]: https://github.com/oddbird/sass-pkg-test

### Design Decisions

We could use the `~` popularized by Webpack's `load-sass` format, but this has
been deprecated since 2021. In addition, since this creates a URL that is
syntactically a relative URL, it does not make it clear to the implementation or
the reader where to find the file.

While the Dart Sass implementation allows for the use of the `package:` url
scheme, a similar standard doesn't exist in Node. We could add `node_modules` to
the load path, but that again suffers from lack of clarity to the implementation
and the reader.

Some npm packages have adopted a convention of declaring their Sass entrypoints
using `"style"` or `"sass"` keys in their `package.json`. This is tied to the
npm context, and would require parsing package.json to find these files.
Instead, each implementation should use the standard patterns and tooling
(`require` for Node and `package_config` for Dart) for resolving file paths.
This allows library authors control over what is public, which follows Sass's
general design.

This proposal does not add support for a [Node custom user condition]. Node
currently does not provide a native way to set a condition on a single
invocation of `require.resolve`, meaning we would need to implement our own
`package.json` parsing similar to [Rollup] or add a dependency on a community
solution like [resolve.exports]. However, this proposal will not prevent future
support.

[Node custom user condition]: https://nodejs.org/api/packages.html#community-conditions-definitions
[Rollup]: https://github.com/rollup/plugins/blob/master/packages/node-resolve/src/package/resolvePackageExports.js
[resolve.exports]: https://github.com/lukeed/resolve.exports

A potential source of confusion is in instances where a dependency defines a
default or main export that is not in the root folder. In these cases, Sass will
look for index and partial files in the same folder as the default or main
export, and not in the package root.

The `pkg` scheme will not be supported in the browser version of Dart Sass. To
support a similar functionality, a user would need to ensure that files are
served, and the loader would need to fetch the URL. In order to follow the same
algorithm for [resolving a file: URL], we would need to make many fetches. If we
instead require the browser version to have a fully resolved URL, we negate many
of this spec's benefits.

[resolving a file: URL]: ../spec/modules.md#resolving-a-file-url

## Semantics

This proposal defines a new step in the [Loading a Source File] in modules that
implementations must include.

New step for [Loading a Source File], after "if argument is a relative URL"
step:

- If `argument` is a valid URL with scheme `pkg`:
  - Let `resolved` be the result of resolving a pkg url
  - If `resolved` is not null, let `argument` equal `resolved`, and continue.
    Otherwise, return null.

[Loading a Source File]: ../spec/modules.md#loading-a-source-file

### Resolving a `pkg`: URL

This algorithm takes a URL, `url`, whose `scheme` must be `pkg` and returns
either another URL of a file or directory on disk, or null. At this point, the
URL is not guaranteed to resolve to a file or disk.

- Let `resolved` be the result of the implementation's dependency resolution
  algorithm.
- If resolution fails:
  - Let `dependencyDefault` be the result of the implementation's dependency
    resolution algorithm for the dependency name.
  - Let `urlSubpath` be the path segments of `url` that are not part of the
    dependency name.
  - Let `resolved` be `dependencyDefault` appended with `urlSubpath`.
- If `resolved` is unknown, meaning neither the full path nor the dependency
  name were resolved, return `null`.
- If `resolved` is a directory or a file with extension of `scss`, `sass`, or
  `css`, return it. Otherwise:
- If `resolved` is a file with a name that is different than the name of `url`,
  return the file's containing directory.
- Return null.

Note: The fifth step, returning the containing directory, is required to support
instances where a dependency's default export is not a `sass`, `scss` or `css`
file.

[Loading a Module]: ../spec/modules.md#loading-a-source-file

## Deprecation Process

The `package` url scheme supported in Dart is not part of the Sass spec, but is
supported by the `dart-sass` implementation when running in Dart. This should be
deprecated in favor of moving authors to the new `pkg` url scheme. Usage of the
`package` syntax will result in a deprecation message, and a future major
version of Sass should remove support.

## Examples

### Node

For Bootstrap to support `pkg` imports, they have 2 options. They can add
`_index.scss` to `dist/js`, as that is the containing folder for their main
export, and use `@forward` to expose the files.

```
@forward './scss/bootstrap.scss';
```

Alternatively, they could recommend using `@use 'pkg:bootstrap/scss';`, and add
an `exports` key to their `package.json`, using node subpath exports. Opting
into this format requires library authors to be exhaustive. Here, they are
exposing the default js library, forwarding `./scss` and also exposing
`./scss/bootstrap.scss` to not break existing imports of `@use
'bootstrap/scss/bootstrap.scss'`.

```
"exports": {
    ".": "./dist/js/bootstrap.js",
    "./scss": "./scss/bootstrap.scss",
    "./scss/bootstrap.scss": "./scss/bootstrap.scss"
}
```

### Dart

For Dart libraries to take advantage of this, they can place a file at
`lib/_index.scss`. Other files within the `lib` folder would also be available
for library consumers. For example, if a library `libraryName` contains
`lib/themes/dark/_index.scss`, a consumer could write `@use
'pkg:libraryName/themes/dark'`.

More examples can be found in the  [Sass pkg: test] example repo.

[Sass pkg: test]: https://github.com/oddbird/sass-pkg-test
