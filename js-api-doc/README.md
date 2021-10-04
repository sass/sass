Both [Dart Sass] and [LibSass] support the same JavaScript API. Dart Sass is
distributed as the pure-Javascript [`sass` package], and LibSass is distributed
as a native extension in the [`node-sass` package].

[Dart Sass]: https://sass-lang.com/dart-sass
[LibSass]: https://sass-lang.com/libsass
[`sass` package]: https://www.npmjs.com/package/sass
[`node-sass` package]: https://www.npmjs.com/package/node-sass

## Usage

The JavaScript API has two entrypoints for compiling a Sass to CSS. Each one can
compile either a Sass file by passing in [[LegacyFileOptions]] or a string of
Sass code by passing in a [[LegacyStringOptions]].

* [[renderSync]] runs synchronously. It's **by far the fastest option** when
  using Dart Sass, but at the cost of only supporting synchronous {@link
  LegacyImporter | importer} and {@link LegacyFunction | function} plugins.
  
  ```js
  var sass = require('sass'); // or require('node-sass');

  var result = sass.renderSync({file: "style.scss"});
  console.log(result.css.toString());
  ```

* [[render]] runs asynchronously and calls a callback when it finishes. It's
  much slower when using Dart Sass, but it supports asynchronous {@link
  LegacyImporter | importer} and {@link LegacyFunction | function} plugins.

  ```js
  var sass = require('sass'); // or require('node-sass');

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

