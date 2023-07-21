# Package Importer

_([Issue](https://github.com/sass/sass/issues/2739))_

This proposal introduces the semantics for a Package Importer and defines the
`pkg` URL scheme to indicate Sass package imports in an implementation-agnostic
format. It also defines the semantics for a new built-in Node.js Package
Importer.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Node built-in importer](#node-built-in-importer)
  * [Design Decisions](#design-decisions)
    * [Using a `pkg` url scheme](#using-a-pkg-url-scheme)
    * [No built-in `pkg` resolver for browsers](#no-built-in-pkg-resolver-for-browsers)
    * [Available as an opt-in setting](#available-as-an-opt-in-setting)
    * [Node Resolution Decisions](#node-resolution-decisions)
* [Types](#types)
    * [`usePkgImporter`](#usepkgimporter)
* [Semantics](#semantics)
  * [Package Importers](#package-importers)
  * [Node Specific Semantics](#node-specific-semantics)
    * [Resolving `pkg` root values](#resolving-pkg-root-values)
    * [Resolving a package name](#resolving-a-package-name)
    * [Resolving the root directory for a package](#resolving-the-root-directory-for-a-package)
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

This proposal also defines a built-in Node.js importer.

For example, `@use "pkg:bootstrap";` would resolve to the path of a
library-defined export within the `bootstrap` dependency. In Node, that would be
resolved within `node_modules`, using the [Node resolution algorithm].

[node resolution algorithm]: https://nodejs.org/api/packages.html

### Node built-in importer

The built-in Node importer resolves in the following order:

1. `sass` condition in package.json `exports`
2. `style` condition in package.json `exports`
3. If no subpath, then find root export:
4. `sass` key at package.json root
5. `style` key at package.json root
6. `index` file at package root, resolved for file extensions and partials
7. If there is a subpath, resolve that path relative to the package root, and
   resolve for file extensions and partials

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
@use 'pkg:library';
```

More examples can be found in the [Sass pkg: test] example repo.

To better understand and allow for testing against the recommended algorithm, a
[Sass pkg: test] repository has been made with a rudimentary implementation of
the algorithm.

[sass pkg: test]: https://github.com/oddbird/sass-pkg-test

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

[resolving a file: url]: ../spec/modules.md#resolving-a-file-url

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
`package.json`.

Other packages have adopted [conditional exports], driven by build tools like
[Vite], [Parcel] and [Sass Loader for Webpack] which all resolve Sass paths
using the `"sass"` and the `"style"` custom conditions.

[conditional exports]: https://nodejs.org/api/packages.html#conditional-exports
[Vite]: https://github.com/vitejs/vite/pull/7817
[Parcel]:
    https://github.com/parcel-bundler/parcel/blob/2d2400ded4615375ee6bd53ef77b4857ad1591dd/packages/transformers/sass/src/SassTransformer.js#L163
[Sass Loader for Webpack]:
    https://github.com/webpack-contrib/sass-loader/blob/02df41203adfda96959e56abb43bd35a89ec11ba/src/utils.js#L514

Because use of conditional exports is flexible and recommended for modern
packages, this will be the primary method used for the Node package resolver. We
will support both the `"sass"` and the `"style"` conditions, as Sass can also
use the CSS exports exposed through `"style"`. While in practice, `"style"`
tends to be used solely for `css` files, we will support `scss`, `sass` and
`css` files for either `"sass"` or `"style"`.

## Types

#### `usePkgImporter`

If true, the compiler will use the built-in package importer to resolve any url
with the `pkg` scheme. This importer follows Node.js logic to locate Sass files.

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

### Package Importers

This proposal defines the requirements for Package Importers written by users or
provided by implementations. It is a type of [Filesystem Importer] and will
handle non-canonical URLs:

- with the scheme `pkg`
- followed by a package name
- optionally followed by a path, with path segments separated with a forward
  slash.

The package name will often be the first path segment, but the importer should
take into account any conventions in the environment. For instance, Node.js
supports scoped package names, which start with `@` followed by 2 path segments.

[filesystem importer]: ../spec/modules.md#filesystem-importer

### Node Specific Semantics

The Node package importer is an [importer] with an associated absolute `pkg:`
URL named `base`. When the Node package importer is invoked with a string named
`string` and a [previous url] `previousUrl`:

- Let `url` be the result of [parsing `string` as a URL][parsing a url] with
  `base` as the base URL. If this returns a failure, throw that failure.
- Let `fullPath` be `url`'s path.
- Let `packageName` be the result of [resolving a package name], and `subPath`
  be the path without the `packageName`.
- Let `packageRoot` be the result of [resolving the root directory for a
  package].
- Let `packageManifest` be the result of loading the `package.json` at
  `packageRoot`.
- If `packageManifest` is not set, throw an error.
- Let `resolved` be the result of using [resolve.exports] to resolve `fullPath`
  with the `sass` condition set, using `packageManifest`.
  - If `resolved` has the scheme `file:` and an extension of `sass`, `scss` or
    `css`, return it.
  - Otherwise, if `resolved` is not null, throw an error.
- Let `resolved` be the result of using [resolve.exports] to resolve `fullPath`
  with the `style` condition set, using `packageManifest`.
  - If `resolved` has the scheme `file:` and an extension of `sass`, `scss` or
    `css`, return it.
  - Otherwise, if `resolved` is not null, throw an error.
- If `subPath` is empty, return result of [resolving `pkg` root values].
- Let `resolved` be `subPath` resolved relative to `packageRoot`.
- Return the result of [resolving a file url] with `resolved`.

> Note that this algorithm does not automatically resolve index files, partials
> or extensions, except where specified. When using the `pkg:` url scheme,
> authors need to explicitly use the fully resolved filename.

[previous url]: ../accepted/prev-url.d.ts.md
[resolve.exports]: https://github.com/lukeed/resolve.exports
[resolving `pkg` root values]: #resolving-pkg-root-values
[resolving a package name]: #resolving-a-package-name
[parsing a url]: https://url.spec.whatwg.org/#concept-url-parser
[resolving the root directory for a package]:
    #resolving-the-root-directory-for-a-package

#### Resolving `pkg` root values

This algorithm takes a string `packagePath` which is the root directory for a
package and `packageManifest`, which is the contents of the `package.json` file,
and returns a file URL.

- Let `sassValue` be the value of `sass` in `packageManifest`.
- If `sassValue` is a relative path with an extension of `sass`, `scss` or
  `css`, return the `packagePath` appended with `sassValue`.
- Let `styleValue` be the value of `style` in `packageManifest`.
- If `styleValue` is a relative path with an extension of `css`, return the
  `packagePath` appended with `styleValue`.
- Otherwise return the result of [resolving a file url] with `packagePath`.

[resolving the root directory for a package]:
    #resolving-the-root-directory-for-a-package
[resolving a file url]: ../spec/modules.md#resolving-a-file-url

#### Resolving a package name

This algorithm takes a string, `url`, and returns the portion that identifies
the node package.

- If `url` starts with `@`, it is a scoped package. Return the first 2 [url path
  segments], including the separating `/`
- Otherwise, return the first url path segment.

[url path segments]: https://url.spec.whatwg.org/#url-path-segment

#### Resolving the root directory for a package

This algorithm takes a string, `packageName`, an absolute URL
`currentDirectory`, and an optional absolute URL `previousUrl`, and returns an
absolute URL to the root directory for the most proximate installed
`packageName`.

> We need to replicate Node's behavior, as defined in [Loading from node_modules
> > folders], of walking up the directory chain to find packages from
> `previousUrl`, so that packages can use the correct version of the packages
> that they depend on.

- If `previousUrl` is null, return the result of `require.resolve` with the
  `packageName`.
- While `rootDirectory` is undefined:
  - Remove the final path segment from `previousUrl`
  - If `node_modules` is the new final path segment of `previousUrl`, continue.
  - Let `potentialPath` be `previousUrl` appended with `node_modules/` and
    `packageName`.
  - If `potentialPath` is a directory, let `rootDirectory` be `potentialPath`.
  - Otherwise, if `previousUrl` is the root of the file system, throw an error.
- Return `rootDirectory`.

[loading from node_modules folders]:
    https://nodejs.org/api/modules.html#loading-from-node_modules-folders

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

[community conditions definition]:
    https://nodejs.org/docs/latest-v20.x/api/packages.html#community-conditions-definitions
[wintercg]: https://wintercg.org/
[runtime keys proposal specification]:
    https://runtime-keys.proposal.wintercg.org/#adding-a-key
