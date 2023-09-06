# Package Importer

*([Issue](https://github.com/sass/sass/issues/2739))*

This proposal introduces the semantics for a Package Importer and defines the
`pkg:` URL scheme to indicate Sass package imports in an implementation-agnostic
format. It also defines the semantics for a new built-in Node Package
Importer.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Node built-in importer](#node-built-in-importer)
  * [Design Decisions](#design-decisions)
    * [Using a `pkg:` URL scheme](#using-a-pkg-url-scheme)
    * [No built-in `pkg:` resolver for browsers](#no-built-in-pkg-resolver-for-browsers)
    * [Available as an opt-in setting](#available-as-an-opt-in-setting)
    * [Available in legacy API](#available-in-legacy-api)
    * [Node Resolution Decisions](#node-resolution-decisions)
* [Types](#types)
  * [`pkgImporter`](#pkgimporter)
* [Semantics](#semantics)
  * [Package Importers](#package-importers)
  * [Node Package Importer](#node-package-importer)
* [Procedures](#procedures)
  * [Node Algorithm for Resolving a `pkg:` URL](#node-algorithm-for-resolving-a-pkg-url)
  * [Resolving a package name](#resolving-a-package-name)
  * [Resolving the root directory for a package](#resolving-the-root-directory-for-a-package)
  * [Resolving package exports](#resolving-package-exports)
  * [Resolving package root values](#resolving-package-root-values)
  * [Export Load Paths](#export-load-paths)
* [Embedded Protocol](#embedded-protocol)
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

This proposal defines a `pkg:` URL scheme for usage with `@use` that directs an
implementation to resolve a URL within a dependency. Sass interfaces may provide
one or more implementations that  will resolve the dependency URL using the
resolution standards and conventions for that environment. Once resolved, this
URL will be loaded in the same way as any other `file:` URL.

This proposal also defines a built-in Node importer.

For example, `@use "pkg:bootstrap";` would resolve to the path of a
library-defined export within the `bootstrap` dependency. In Node, that could be
resolved within `node_modules`, using the [Node resolution algorithm].

[node resolution algorithm]: https://nodejs.org/api/packages.html

### Node built-in importer

The built-in Node importer resolves in the following order:

1. `sass`, `style`, or `default` condition in package.json `exports`.

2. If there is not a subpath, then find the root export:

   1. `sass` key at package.json root.

   2. `style` key at package.json root.

   3. `index` file at package root, resolved for file extensions and partials.

3. If there is a subpath, resolve that path relative to the package root, and
   resolve for file extensions and partials.

For library creators, the recommended method is to add a `sass` conditional
export to `package.json`. The `style` condition is an acceptable alternative,
but relying on the `default` condition is discouraged. Notably, the key order
matters, and the importer will resolve to the first value with a key that is
`sass`, `style`, or `default`.

```json
{
  "exports": {
    ".": {
      "sass": "./dist/scss/index.scss",
      "import": "./dist/js/index.mjs",
      "default": "./dist/js/index.js"
    }
  }
}
```

Then, library consumers can use the `pkg:` syntax to get the default export.

```scss
@use 'pkg:library';
```

To better understand and allow for testing against the recommended algorithm, a
[Sass pkg: test] repository has been made with a rudimentary implementation of
the algorithm.

[sass pkg: test]: https://github.com/oddbird/sass-pkg-test

### Design Decisions

#### Using a `pkg:` URL scheme

We could use the `~` popularized by Webpack's `load-sass` format, but this has
been deprecated since 2021. In addition, since this creates a URL that is
syntactically a relative URL, it does not make it clear to the implementation or
the reader where to find the file.

While the Dart Sass implementation allows for the use of the `package:` URL
scheme, a similar standard doesn't exist in Node. We chose the `pkg:` URL scheme
as it clearly communicates to both the user and compiler that the specified files
are from a dependency. The `pkg:` URL scheme also does not have known conflicts
in the ecosystem.

#### No built-in `pkg:` resolver for browsers

Dart Sass will not provide a built-in resolver for browsers to use the `pkg:`
scheme. To support a similar functionality, a user would need to ensure that
files are served, and the loader would need to fetch the URL. In order to follow
the same algorithm for [resolving a file: URL], we would need to make many
fetches. If we instead require the browser version to have a fully resolved URL,
we negate many of this spec's benefits. Users may write their own custom
importers to fit their needs.

[resolving a file: url]: ../spec/modules.md#resolving-a-file-url

#### Available as an opt-in setting

The `pkg:` import loader will be exposed through an opt-in setting as it adds the
potential for unexpected file system interaction to `compileString` and
`compileStringAsync`. Specifically, we want people who invoke Sass compilation
functions to have control over what files get accessed, and there's even a risk
of leaking file contents in error messages.

#### Available in legacy API

The built-in Node Package Importer will be added to the legacy API in order to
reduce the barrier to adoption. While the legacy API is deprecated, we
anticipate the implementation to be straightforward.

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
`package.json`.

Other packages have adopted [conditional exports], driven by build tools like
[Vite], [Parcel] and [Sass Loader for Webpack] which all resolve Sass paths
using the `"sass"` and the `"style"` custom conditions.

[conditional exports]: https://nodejs.org/api/packages.html#conditional-exports
[Vite]: https://github.com/vitejs/vite/pull/7817
[Parcel]: https://github.com/parcel-bundler/parcel/blob/2d2400ded4615375ee6bd53ef77b4857ad1591dd/packages/transformers/sass/src/SassTransformer.js#L163
[Sass Loader for Webpack]: https://github.com/webpack-contrib/sass-loader/blob/02df41203adfda96959e56abb43bd35a89ec11ba/src/utils.js#L514

Because use of conditional exports is flexible and recommended for modern
packages, this will be the primary method used for the Node package importer. We
will support both the `"sass"` and the `"style"` conditions, as Sass can also
use the CSS exports exposed through `"style"`. While in practice, `"style"`
tends to be used solely for `css` files, we will support `scss`, `sass` and
`css` files for either `"sass"` or `"style"`.

While conditional exports allows package authors to define specific aliases to internal
files, we will still use the Sass conventions for resolving file paths with
partials, extensions and indices to discover the intended export alias. However,
we will not apply that logic to the destination, and will expect library authors
to map the export to the correct place. In other words, given a `package.json`
with `exports` as below, The Node package importer will resolve a
`@use "pkg:pkgName/variables";` to the destination of the `_variables.scss` export.

```json
{
  "exports": {
    "_variables.scss": {
      "sass": "./src/sass/_variables.scss"
    }
  }
}
```

## Types

```ts
import '../spec/js-api';
```

### `pkgImporter`

If set, the compiler will use the specified built-in package importer to resolve
any URL with the `pkg:` scheme. Currently, the only available package importer
is `node`, which follows Node resolution logic to locate Sass files.

Defaults to undefined.

After the first bullet points in [`compile`] and [`compileString`] in the
Javascript Compile API, insert:

* If `options.pkgImporter` equals `'node'`:

  * Let `pkgImporter` be a [Node Package Importer] with an associated
    `entryPointURL` of `require.main.filename`.

  * Append `pkgImporter` to the `options.importers`.
  
  > Package Importers are evaluated after user-defined importers but
  > before load paths.

[`compile`]: ../spec/js-api/compile.d.ts.md#compile
[`compileString`]: ../spec/js-api/compile.d.ts.md#compilestring
[Node Package Importer]: #node-package-importer

```ts
declare module '../spec/js-api/options' {
  interface Options<sync extends 'sync' | 'async'> {
    pkgImporter?: 'node';
  }
}

export interface LegacySharedOptions<sync extends 'sync' | 'async'> {
  pkgImporter?: 'node';
}
```

## Semantics

### Package Importers

This proposal defines the requirements for Package Importers written by users or
provided by implementations. It is a type of [Importer] and, in addition to the
standard requirements for importers, it must handle only non-canonical URLs that:

* have the scheme `pkg`, and
* whose path begins with a package name, and
* optionally followed by a path, with path segments separated with a forward
  slash.

The package name will often be the first path segment, but the importer may take
into account any conventions in the environment. For instance, Node supports
scoped package names, which start with `@` followed by 2 path segments. Note
that package names that contain non-alphanumeric characters may be less portable
across different package importers.

Package Importers must reject the following patterns:

* A URL whose path begins with `/`.
* A URL with non-empty/null username, password, host, port, query, or fragment.

Package Importers must be added to the [global importer list] immediately after any
user-provided importers.

[importer]: ../spec/modules.md#importer
[global importer list]: ../spec/modules.md#global-importer-list

### Node Package Importer

The Node Package Importer is an implementation of a [Package Importer] using the
standards and conventions of the Node ecosystem. It has an associated absolute
`file:` URL named `entryPointURL`.

When the Node Package Importer is invoked with a string named `string`:

* If `string` is a relative URL, return null.

* Let `url` be the result of [parsing `string` as a URL][parsing a URL]. If this
  returns a failure, throw that failure.

* If `url`'s scheme is not `pkg:`, return null.

* If `url`'s path begins with a `/` or is empty, throw an error.

* If `url` contains a username, password, host, port, query, or fragment, throw
  an error.

* Let `sourceFile` be the canonical URL of the [current source file] that
  contained the load.

* If `sourceFile`'s scheme is `file:`, let `baseURL` be `sourceFile`.

* Otherwise, let `baseURL` be `entryPointURL`.

* Let `resolved` be the result of [resolving a `pkg:` URL as Node] with `url` and
  `baseURL`.

* If `resolved` is null, return null.

* Let `text` be the contents of the file at `resolved`.

* Let `syntax` be:

  * "scss" if `resolved` ends in `.scss`.

  * "indented" if `resolved` ends in `.sass`.

  * "css" if `resolved` ends in `.css`.

  > The algorithm for [resolving a `pkg:` URL as Node] guarantees that
  > `resolved` will have one of these extensions.

* Return `text`, `syntax`, and `resolved`.

[Package Importer]: #package-importers
[parsing a URL]: https://url.spec.whatwg.org/#concept-url-parser
[current source file]: ../spec/spec.md#current-source-file
[resolving a `pkg:` URL as Node]: #node-algorithm-for-resolving-a-pkg-url

## Procedures

### Node Algorithm for Resolving a `pkg:` URL

This algorithm takes a URL with scheme `pkg:` named `url`, and a URL `baseURL`.
It returns a canonical `file:` URL or null.

* Let `fullPath` be `url`'s path.

* Let `packageName` be the result of [resolving a package name] with `fullPath`,
  and `subpath` be `fullPath` without the `packageName`.

* Let `packageRoot` be the result of [resolving the root directory for a
  package] with `packageName` and `baseURL`.

* If a `package.json` file does not exist at `packageRoot`, throw an error.

* Let `packageManifest` be the result of parsing the `package.json` file at
  `packageRoot` as [JSON].

* Let `resolved` be the result of [resolving package exports] with
  `packageRoot`, `subpath`, and `packageManifest`.

* If `resolved` has the scheme `file:` and an extension of `sass`, `scss` or
  `css`, return it.

* Otherwise, if `resolved` is not null, throw an error.

* If `subpath` is empty, return the result of [resolving package root values].

* Let `resolved` be `subpath` resolved relative to `packageRoot`.

* Return the result of [resolving a `file:` URL] with `resolved`.

[Resolving package exports]: #resolving-package-exports
[resolving package root values]: #resolving-package-root-values
[resolving a package name]: #resolving-a-package-name
[JSON]: https://datatracker.ietf.org/doc/html/rfc8259
[resolving the root directory for a package]: #resolving-the-root-directory-for-a-package
[resolving a `file:` URL]: ../spec/modules.md#resolving-a-file-url

### Resolving a package name

This algorithm takes a string, `path`, and returns the portion that identifies
the Node package.

* If `path` starts with `@`, it is a scoped package. Return the first 2 [URL path
  segments], including the separating `/`.

* Otherwise, return the first URL path segment.

### Resolving the root directory for a package

This algorithm takes a string, `packageName`, and an absolute URL `baseURL`, and
returns an absolute URL to the root directory for the most proximate installed
`packageName`.

* Return the result of `PACKAGE_RESOLVE(packageName, baseURL)` as defined in
  the [Node resolution algorithm specification].

[Node resolution algorithm specification]: https://nodejs.org/api/esm.html#resolution-algorithm-specification

### Resolving package exports

This algorithm takes a package.json value `packageManifest`, a directory URL
`packageRoot` and a relative URL path `subpath`. It returns a file URL or null.

* Let `exports` be the value of `packageManifest.exports`.

* If `exports` is undefined, return null.

* Let `subpathVariants` be the result of [Export load paths] with `subpath`.

* Let `resolvedPaths` be a list of the results of calling
  `PACKAGE_EXPORTS_RESOLVE(packageRoot, subpathVariant, exports, ["sass",
  "style"])` as defined in the [Node resolution algorithm specification], with
  each `subpathVariants` as `subpathVariant`.

  > The PACKAGE_EXPORTS_RESOLVE algorithm always includes a `default` condition,
  > so one does not have to be passed here.

* If `resolvedPaths` contains more than one resolved URL, throw an error.

* If `resolvedPaths` contains exactly one resolved URL, return it.

* If `subpath` has an extension, return null.

* Let `subpathIndex` be `subpath` + `"/index"`.

* Let `subpathIndexVariants` be the result of [Export load paths] with `subpathIndex`.

* Let `resolvedIndexPaths` be a list of the results of calling
  `PACKAGE_EXPORTS_RESOLVE(packageRoot, subpathVariant, exports, [condition])`
  as defined in the [Node resolution algorithm specification], with each
  `subpathIndexVariants` as `subpathVariant`.

* If `resolvedIndexPaths` contains more than one resolved URL, throw an error.

* If `resolvedIndexPaths` contains exactly one resolved URL, return it.

* Return null.

> Where possible in Node, implementations can use [resolve.exports] which
> exposes the Node resolution algorithm, allowing for per-path custom
> conditions, and without needing filesystem access.

[Export load paths]: #export-load-paths
[resolve.exports]: https://github.com/lukeed/resolve.exports

### Resolving package root values

This algorithm takes a string `packagePath`, which is the root directory for a
package, and `packageManifest`, which is the contents of that package's
`package.json` file, and returns a file URL.

* Let `sassValue` be the value of `sass` in `packageManifest`.

* If `sassValue` is a relative path with an extension of `sass`, `scss` or
  `css`:

  * Return the canonicalized `file:` URL for `${packagePath}/${sassValue}`.

* Let `styleValue` be the value of `style` in `packageManifest`.

* If `styleValue` is a relative path with an extension of `sass`, `scss` or
  `css`:

  * Return the canonicalized `file:` URL for `${packagePath}/${styleValue}`.

* Otherwise return the result of [resolving a `file:` URL for extensions] with
  `packagePath + "/index"`.

[resolving a `file:` URL for extensions]: ../spec/modules.md#resolving-a-file-url-for-extensions

[URL path segments]: https://url.spec.whatwg.org/#url-path-segment

### Export Load Paths

This algorithm takes a relative URL path `subpath` and returns a list of
potential subpaths, resolving for partials and file extensions.

* Let `paths` be a list.

* If `subpath` ends in `.scss`, `.sass`, or `.css`:

  * Add `subpath` to `paths`.

* Otherwise, add `subpath` + `.scss`, `subpath` + `.sass`, and `subpath` +
  `.css` to `paths`.

* If `subpath`'s [basename] does not start with `_`, for each `item` in
  `paths`, prepend `"_"` to the basename, and add to `paths`.

* Return `paths`.

[basename]: ../spec/modules.md#basename

## Embedded Protocol

```proto
message CompileRequest {
  message Importer {
    PackageImporter packageImporter = 14;
  }
}
enum PackageImporter {
  NODE = 0
}
```

## Ecosystem Notes

It may be worth adding a [Community Conditions Definition] to the Node
Documentation. [WinterCG] has a [Runtime Keys proposal specification] underway
in standardizing the usage of custom conditions for runtimes, but Sass doesn't
cleanly fit into that specification.

[community conditions definition]: https://nodejs.org/docs/latest-v20.x/api/packages.html#community-conditions-definitions
[wintercg]: https://wintercg.org/
[runtime keys proposal specification]:  https://runtime-keys.proposal.wintercg.org/#adding-a-key
