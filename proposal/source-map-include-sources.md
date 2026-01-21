# Source Map Include Sources: Draft 1.0

*([Issue](https://github.com/sass/dart-sass/issues/2712))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Semantics](#semantics)
  * [`sourceMapIncludeSources`](#sourcemapincludesources)
  * [`sourceMapUrl`](#sourcemapurl)
* [Deprecation Process](#deprecation-process)
  * [Phase 1](#phase-1)
  * [Phase 2](#phase-2)

## Background

> This section is non-normative.

Historically, Sass [has provided] an boolean `sourceMapIncludeSources` option to
embed the source contents into source map. However, there were a number of
oddities and inconsistencies in this option:

[has provided]: ../spec/js-api/options.d.ts.md#sourcemapincludesources

* In order to provide a convenient user experience, files loaded via custom
  importer without any explict `sourceMapUrl` would always have a `data:` URL
  in the `sources` field in the source map. It exposes the source contents even
  if the option is set to `false`. On the other hand, the source contents are
  embedded twice in `sources` field as `data:` URL and `sourcesContent` field as
  raw data if the option is set to `true`.

* The use of `data:` URL instead of the canonical URL has led to difficulties
  of identifying the name of the source files when navigating through the
  source map.

## Summary

This proposal addresses both of the issues above:

* `sourceMapIncludeSources` now accepts new values for refined control and
  consistency.

* Custom Importers use canonical URL as `sourceMapUrl` if not defined explictly.

## Semantics

### `sourceMapIncludeSources`

* If the value is `'auto'`, source contents for sources without explictly
  defined `sourceMapUrl` are included.

* If the value is `'never'`, no source contents are included.

* If the value is `'always'`, all source contents are included.

### `sourceMapUrl`

* If the `sourceMapUrl` is not explictly defined in `ImportResult`, the
  canonical URL is used as fallback.

## Deprecation Process

The deprecation process will be divided into two phases:

### Phase 1

> This phase adds no breaking changes.

Phase 1 does not throw an error for passing boolean value. Instead, it maps
boolean values for backward compatibility:

* `undefined` => `'auto'`
* `false` => `'never'`
* `true` => `'always'`

All other changes are implemented as specified.

### Phase 2

Phase 2 implements the full changes described above. Passing boolean value will
throw an error. It will be released as a breaking change in next Dart Sass major
version.
