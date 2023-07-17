# Package Importer

*([Issue](https://github.com/sass/sass/issues/2739))*

This proposal adds Sass support for a standard package importer, introducing a
`pkg` URL scheme to indicate Sass package imports in an implementation-agnostic
format.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Resolution Order](#resolution-order)
  * [Examples](#examples)
    * [Node](#node)
    * [Dart](#dart)
  * [Design Decisions](#design-decisions)
    * [Using a `pkg` url scheme](#using-a-pkg-url-scheme)
    * [No built-in `pkg` resolver for browsers](#no-built-in-pkg-resolver-for-browsers)
    * [Available as an opt-in setting](#available-as-an-opt-in-setting)
    * [Node Resolution Decisions](#node-resolution-decisions)
* [Types](#types)
    * [`usePkgImporter`](#usepkgimporter)
* [Semantics](#semantics)
  * [Resolving a `pkg:` URL](#resolving-a-pkg-url)
  * [Platform-Specific Semantics](#platform-specific-semantics)
    * [Dart](#dart-1)
    * [Node](#node-1)
    * [Resolving `pkg` Root](#resolving-pkg-root)
    * [Resolving `pkg` Subpath](#resolving-pkg-subpath)
* [Deprecation Process](#deprecation-process)
* [Ecosystem Notes](#ecosystem-notes)


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

For example, `@use "pkg:bootstrap";` would resolve to the path of a
library-defined export within the `bootstrap` dependency. In Node, that would be
resolved within `node_modules`, using the [Node resolution algorithm]. In Dart,
that would be resolved within `pub-cache`, using [package-config].

[node resolution algorithm]: https://nodejs.org/api/packages.html
[package-config]: https://pub.dev/packages/package_config

### Resolution Order

This algorithm resolves in the following order:

1. `sass` condition in package.json `exports`
2. `style` condition in package.json `exports`
3. If no subpath, then find root export:
  1. `sass` key at package.json root
  2. `style` key at package.json root
  3. `index` file at package root, resolved for file extensions and partials
4. If there is a subpath, resolve that path relative to the package root, and
   resolve for file extensions and partials

### Examples

#### Node

For library creators, the recommended method is to add a `sass` conditional
export key as the first key in `package.json`.

```json
{
  "exports": {
    ".": {
      "sass": "./dist/scss/index.scss",
      "import": "./dist/js/index.mjs",
      "require": "./dist/js/index.js"
    }
  }
}
```

Then, library consumers can use the pkg syntax to get the default export.

```scss
@use "pkg:library";
```

#### Dart

For Dart libraries to take advantage of this, they can place a file at
`lib/_index.scss`. Other files within the `lib` folder would also be available
for library consumers. For example, if a library `libraryName` contains
`lib/themes/dark/_index.scss`, a consumer could write `@use
"pkg:libraryName/themes/dark";`.

More examples can be found in the [Sass pkg: test] example repo.

To better understand and allow for testing against the recommended algorithm, a
[Sass pkg: test] repository has been made with a rudimentary implementation of
the algorithm.

[Sass pkg: test]: https://github.com/oddbird/sass-pkg-test

### Design Decisions

#### Using a `pkg` url scheme
We could use the `~` popularized by Webpack's `load-sass` format, but this has
been deprecated since 2021. In addition, since this creates a URL that is
syntactically a relative URL, it does not make it clear to the implementation or
the reader where to find the file.

While the Dart Sass implementation allows for the use of the `package:` url
scheme, a similar standard doesn't exist in Node. We chose the `pkg:` url scheme
as it clearly communicates to both the user and compiler, and does not have
known conflicts in the ecosystem.

#### No built-in `pkg` resolver for browsers

Dart Sass will not provide a built-in resolver for browsers to use the `pkg`
scheme. To support a similar functionality, a user would need to ensure that
files are served, and the loader would need to fetch the URL. In order to follow
the same algorithm for [resolving a file: URL], we would need to make many
fetches. If we instead require the browser version to have a fully resolved URL,
we negate many of this spec's benefits. Users may write their own custom
importers to fit their needs.

[resolving a file: URL]: ../spec/modules.md#resolving-a-file-url

#### Available as an opt-in setting

The `pkg` import loader will be exposed through an opt-in setting as it adds the
potential for file system interaction to `compileString` and
`compileStringAsync`. Specifically, we want people who invoke Sass compilation
functions to have control over what files get accessed, and there's even a risk
of leaking file contents in error messages.

#### Node Resolution Decisions

The current recommendation for resolving packages in Node is to add
`node_modules` to the load paths. We could add `node_modules` to the load paths
by default, but that lacks clarity to the implementation and the reader. In
addition, a file may have access to multiple `node_modules` directories, and
different files may have access to different `node_modules` directories in the
same compilation.

There are a variety of methods currently in use for specifying a location of the
default Sass export for npm packages. For the most part, packages contain both
JavaScript and styles, and use the `main` or `module` root keys to define the
JavaScript entry point. Some packages use the `"sass"` key at the root of their
`package.json`. Other packages have adopted [conditional exports], largely
driven by [Vite], which resolves Sass paths using the `"sass"` and the `"style"`
custom conditions.

[conditional exports]: https://nodejs.org/api/packages.html#conditional-exports
[Vite]: https://github.com/vitejs/vite/pull/7817

Because use of conditional exports is flexible and recommended for modern
packages, this will be the primary method used within Node. We will support both
the `"sass"` and the `"style"` conditions, as Sass can also use the CSS exports
exposed through `"style"`. While in practice, `"style"` tends to be used solely
for `css` files, we will support `scss`, `sass` and `css` files for either
`"sass"` or `"style"`.

## Types

#### `usePkgImporter`

If true, the compiler will use the built-in package importer to resolve any
url with the `pkg` scheme. This importer follows Node.js logic to locate Sass files. 

Defaults to false.

```ts
declare module '../spec/js-api' {
  interface Options<sync extends 'sync' | 'async'> {
    /**
     * Whether or not to enable the built-in package importer to resolve any url
     * with the `pkg` scheme. This importer follows Node.js resolution logic.
     *
     * @defaultValue `false`
     * @category Input
     */
    usePkgImporter?: boolean;
  }
}
```


## Semantics

This proposal defines a new importer that implementations should make available.
It will be disabled by default. The loader will handle URLs:

- with the scheme `pkg`
- followed by a package name
- optionally followed by a path, with path segments separated with a forward
  slash.

### Resolving a `pkg:` URL

The `pkg importer` will provide a method to canonicalize a `pkg:` URL, and
extend the implementation's existing File Importer.

### Platform-Specific Semantics

#### Dart

Given `url` with the format `pkg:.*`:

- Let `path` be `url`'s path
- Use [package-config] to resolve `path`

[package-config]: https://pub.dev/packages/package_config

#### Node

Given `url` with the format `pkg:.*`:

- Let `fullPath` be `url`'s path
- Let `resolved` be the result of using [resolve.exports] to resolve `fullPath`
  with the `sass` condition set
  - If `resolved` has the scheme `file:` and an extension of `sass`, `scss` or
    `css`, return it.
- Let `resolved` be the result of using [resolve.exports] to resolve `fullPath`
  with the `style` condition set.
  - If `resolved` has the scheme `file:` and an extension of `sass`, `scss` or
    `css`, return it.
- Let `packageName` be the package identifier, and `subPath` be the path without
  the package identifier.
- If `subPath` is empty, return result of [resolving `pkg` root].
- Otherwise, return result of [resolving `pkg` subpath].

[resolve.exports]: https://github.com/lukeed/resolve.exports
[resolving pkg root]: #resolving-pkg-root
[resolving pkg subpath]: #resolving-pkg-subpath

#### Resolving `pkg` Root

- Let `packagePath` be the file path to the package root.
- Let `sassValue` be the value of `sass` in the `package.json` at the pkg root.
- If `sassValue` is a relative path with an extension of `sass`, `scss` or
  `css`, return the `packagePath` appended with `sassValue`.
- Let `styleValue` be the value of `style` in the `package.json` at the pkg
  root.
- If `styleValue` is a relative path with an extension of `css`, return the
  `packagePath` appended with `styleValue`.
- Otherwise return the result of [resolving a file url] with `packagePath`.

[resolving a file url]: ../spec/modules.md#resolving-a-file-url

#### Resolving `pkg` Subpath

- Let `packagePath` be the file path to the package root.
- Let `fullPath` be `subpath` resolved relative to `packagePath`.
- Return the result of [resolving a file url] with `fullPath`.

## Deprecation Process

The `package` url scheme supported in Dart is not part of the Sass spec, but is
supported by the `dart-sass` implementation when running in Dart. This should be
deprecated in favor of moving authors to the new `pkg` url scheme. Usage of the
`package` syntax will result in a deprecation message, and a future major
version of Sass should remove support.

## Ecosystem Notes

Vite is currently using the Legacy JS API, and has an [open issue] to update to
the modern API. They also do not expose Sass options to the user, so would need
to enable the `usePkgImporter` on their behalf or expose some configuration.

[open issue]: https://github.com/vitejs/vite/issues/7116

Webpack's [sass-loader] allows users to opt in to the modern API and exposes
Sass options to users.

For Rollup, [rollup-plugin-sass] uses the Legacy JS API. They do expose Sass
options to the user.

[rollup-plugin-sass]: https://github.com/elycruz/rollup-plugin-sass

It may be worth adding a [Community Conditions Definition] to the Node
Documentation. [WinterCG] has a [Runtime Keys proposal specification] underway
in standardizing the usage of custom conditions for runtimes, but Sass doesn't
cleanly fit into that specification.

[Community Conditions Definition]:
    https://nodejs.org/docs/latest-v20.x/api/packages.html#community-conditions-definitions
[WinterCG]: https://wintercg.org/
[Runtime Keys proposal specification]:
    https://runtime-keys.proposal.wintercg.org/#adding-a-key
