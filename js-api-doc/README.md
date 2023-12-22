The Sass JavaScript API can be used to to drive Sass Compilations from JavaScript. It even allows an application to control {@link Options.importers |
how stylesheets are loaded} and {@link Options.functions | define custom
functions}.

The [`sass` package] on npm is a pure-JavaScript package built from the [Dart
Sass] implementation, and includes Dart Sass's [command-line interface].

The [`sass-embedded` package] on npm is a JavaScript wrapper around a Native
Dart executable, and in general is faster than `sass`.

Both `sass` and `sass-embedded` provide the same JavaScript API
using the same underlying [Dart Sass] implementation, but have speed and
platform tradeoffs.

[`sass` package]: https://www.npmjs.com/package/sass
[Dart Sass]: https://sass-lang.com/dart-sass
[command-line interface]: https://sass-lang.com/documentation/cli/dart-sass
[`sass-embedded` package]: https://www.npmjs.com/package/sass-embedded

## Usage

The JavaScript API provides two entrypoints for compiling Sass to CSS, each of
which has a synchronous variant that returns a plain {@link CompileResult} and
an asynchronous variant that returns a `Promise`. **The asynchronous variants
are much slower,** but they allow custom importers and functions to run
asynchronously.

* {@link compile} and {@link compileAsync} take a path to a Sass file and return
  the result of compiling that file to CSS. These functions accept an additional
  {@link Options} argument.

  ```js
  const sass = require('sass');

  const result = sass.compile("style.scss");
  console.log(result.css);

  const compressed = sass.compile("style.scss", {style: "compressed"});
  console.log(compressed.css);
  ```

* {@link compileString} and {@link compileStringAsync} take a string that
  represents the contents of a Sass file and return the result of compiling that
  file to CSS. These functions accept an additional {@link StringOptions}
  argument.

  ```js
  const sass = require('sass');

  const input = `
  h1 {
    font-size: 40px;
    code {
      font-face: Roboto Mono;
    }
  }`;

  const result = sass.compileString(input);
  console.log(result.css);

  const compressed = sass.compileString(input, {style: "compressed"});
  console.log(compressed.css);
  ```

## Integrations

Most popular Node.js build systems have integrations available for the JS API:

* Webpack uses the [`sass-loader` package].
* Gulp uses the [`gulp-sass` package].
* Broccoli uses the [`broccoli-sass-source-maps` package].
* Ember uses the [`ember-cli-sass` package].
* Grunt uses the [`grunt-sass` package].

[`sass-loader` package]: https://www.npmjs.com/package/sass-loader
[`gulp-sass` package]: https://www.npmjs.com/package/gulp-sass
[`broccoli-sass-source-maps` package]: https://www.npmjs.com/package/broccoli-sass-source-maps
[`ember-cli-sass` package]: https://www.npmjs.com/package/ember-cli-sass
[`grunt-sass` package]: https://www.npmjs.com/package/grunt-sass

## Legacy API

The `sass` package also supports an older API. Although this API is deprecated,
it will continue to be supported until the release of version 2.0.0 of the
`sass` package. The legacy API is also supported by the [`node-sass` package],
which is a native extension wrapper for the deprecated [LibSass] implementation.

[`node-sass` package]: https://www.npmjs.com/package/node-sass
[LibSass]: https://sass-lang.com/libsass

The legacy API has two entrypoints for compiling Sass to CSS. Each one can
compile either a Sass file by passing in {@link LegacyFileOptions} or a string
of Sass code by passing in a {@link LegacyStringOptions}.

* {@link renderSync} runs synchronously. It's **by far the fastest option** when
  using Dart Sass, but at the cost of only supporting synchronous {@link
  LegacyImporter | importer} and {@link LegacyFunction | function} plugins.

  ```js
  const sass = require('sass'); // or require('node-sass');

  const result = sass.renderSync({file: "style.scss"});
  console.log(result.css.toString());
  ```

* {@link render} runs asynchronously and calls a callback when it finishes. It's
  much slower when using Dart Sass, but it supports asynchronous {@link
  LegacyImporter | importer} and {@link LegacyFunction | function} plugins.

  ```js
  const sass = require('sass'); // or require('node-sass');

  sass.render({
    file: "style.scss"
  }, function(err, result) {
    if (err) {
      // ...
    } else {
      console.log(result.css.toString());
    }
  });
  ```

## Speed

While multiple factors go into how long Sass compilations take, there are
general speed trends that can help you minimize your compilation time.

### With the `sass` package

With the `sass` package, the synchronous calls will be faster than asynchronous
calls due to the overhead of making the entire evaluation process asynchronous.
While the {@link Compiler} and {@link AsyncCompiler} class are available, they
aren't faster than than the module level compilation methods when using `sass`.

### With the `sass-embedded` package

The `sass-embedded` package provides significant speed improvements in certain
situations, and is generally faster than `sass` for large compilations. When
using the module level compilation methods, asynchronous calls are generally
faster than synchronous ones due to the overhead of emulating synchronous
messaging with worker threads and concurrent compilations being blocked on the
main thread.

The {@link Compiler} and {@link AsyncCompiler} classes provide significant
improvements when using the `sass-embedded` package. We are able to persist and
reuse a process across multiple compilations, avoiding the need to repeatedly
start up and tear down a process. 

When compiling a single file using `sass-embedded`, there is not much difference
between the synchronous and asynchronous methods. When compiling multiple times,
an {@link AsyncCompiler} will be considerably faster than a synchronous {@link
Compiler}.

Other factors like {@link Functions}, {@link Importers} and the complexity of
your Sass files may also impact what compilation methods work best for your
particular use case.
