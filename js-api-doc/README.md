The [`sass` package] on npm is a pure-JavaScript package built from the [Dart
Sass] implementation. In addition to Dart Sass's [command-line interface], it
provides a JavaScript API that can be used to drive Sass compilations from
JavaScript. It even allows an application to control {@link Options.importers |
how stylesheets are loaded} and {@link Options.functions | define custom
functions}.

[`sass` package]: https://www.npmjs.com/package/sass
[Dart Sass]: https://sass-lang.com/dart-sass
[command-line interface]: https://sass-lang.com/documentation/cli/dart-sass

## Usage

The JavaScript API provides two entrypoints for compiling Sass to CSS, each of
which has a synchronous variant that returns a plain [[CompileResult]] and an
asynchronous variant that returns a `Promise`. **The asynchronous variants are
much slower,** but they allow custom importers and functions to run
asynchronously.

* [[compile]] and [[compileAsync]] take a path to a Sass file and return the
  result of compiling that file to CSS. 

  ```js
  const sass = require('sass');

  const result = sass.compile("style.scss");
  console.log(result.css);
  ```

* [[compileString]] and [[compileStringAsync]] take a string that represents the
  contents of a Sass file and return the result of compiling that file to CSS.

  ```js
  const sass = require('sass');

  const result = sass.compileString(`
  h1 {
    font-size: 40px;
    code {
      font-face: Roboto Mono;
    }
  }`);
  console.log(result.css);
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
compile either a Sass file by passing in [[LegacyFileOptions]] or a string of
Sass code by passing in a [[LegacyStringOptions]].

* [[renderSync]] runs synchronously. It's **by far the fastest option** when
  using Dart Sass, but at the cost of only supporting synchronous {@link
  LegacyImporter | importer} and {@link LegacyFunction | function} plugins.
  
  ```js
  const sass = require('sass'); // or require('node-sass');

  const result = sass.renderSync({file: "style.scss"});
  console.log(result.css.toString());
  ```

* [[render]] runs asynchronously and calls a callback when it finishes. It's
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
