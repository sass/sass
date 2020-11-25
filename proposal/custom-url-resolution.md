# Custom url resolution: Draft 1

_[(Issue)](https://github.com/sass/sass/issues/2535)_

## Table of Contents

- [Background](#background)
- [Summary](#summary)
  - [Example](#example)
- [Function](#function)
  - [Steps](#steps)
  - [JavaScript API](#javascript-api)
  - [Using Variables](#using-variables)
- [Presets](#presets)
  - [The inline Preset](#the-inline-preset)
    - [Inline Preset Usage](#inline-preset-usage)
    - [Inline Preset Implementation](#inline-preset-implementation)
  - [The relative Preset](#the-relative-preset)
    - [Relative Preset Usage](#relative-preset-usage)
    - [Relative Preset Implementation](#relative-preset-implementation)

## Background

> This section is non-normative.

Many css features require the use of a sass-url reference to reference resources from outside the sass files, however these files also need to exist on the eventual output directory and server. To ensure the references are valid, the sass API should allow for the user to provide a way to remap and/or inline these resources.

## Summary

> This section is non-normative.

This proposal defines a standardized way to remap the sass-url references to the final location on the server or inline reference.

This is accomplished by running sass-url references through the url rewriting plugin if a url rewriting plugin has been defined.

### Example

This example demonstrates the possibility of rewriting urls to inline assets using base64 and data URIs.

Input:

```scss
.upload-icon {
  background-url: sass-url('../icons/upload-icon.svg');
}
```

Output:

```css
.upload-icon {
  background-url: url('data:image/svg+xml;base64,Jggg==');
}
```

## Function

This proposal introduces logic for a new function: `sass-url("...")`, this function allow users to define a url that gets rewritten based on the config to ensure it can be loaded correctly from the browser.

### Steps

Whenever a sass-url reference is encountered in a sass file the following steps should be executed:

- A sass-url reference is encountered in the sass file
- From the url function we extract the parameter value
- Rewrite the parameter value if it contains any variables ([see using variables](#using-variables))
- Pass the parameter value to the url rewrite plugin along with the canonical url of the current sass file
- This plugin than returns a string or calls the done callback with a string value, if it returns anything else or nothing it should throw an error ([see JavaScript API](#javascript-api))
- The new url value that we received from the url rewrite plugin than gets used to replace the original url value

_Note: if there is no urlRewrite plugin/function none of these steps should be executed._

### JavaScript API

At the core of remapping the sass-url references is the JavaScript API which allows users to return a new sass-url reference, based on the canonical url of the sass file and original sass-url reference.

The first parameter of the urlRewrite function is an object with the following values:

- `file`: The canonical url of the sass file that references the url.
- `url`: The sass-url reference, for example with `sass-url(file://../assets/test.png)` it would be `file://../assets/test.png`.

The second parameter of the urlRewrite function is an optional done callback that is used when performing asynchronous operations.

This callback function takes in two parameters:

- `error`: This can be null or an Error object, `null` means there is no error.
- `url`: This is the `string` that gets used to replace the sass-url reference. This cannot be null or anything else, otherwise sass will throw an error.

Asynchronous example:

```TypeScript
let sassOptions = {
  // Rewrite urls asynchronously
  rewriteUrl: async (urlReference: { file: string, url: string }, done: (error: Error | null, url: string | null) => void): void => {
    if (urlReference.file) {
      content = await fs.readFile(path.join(path.dirname(file), url));
    }

    done(null, content ? `data:${base64(content)))}` : url);
  }
}
```

Synchronous example:

```TypeScript
const outDir = '/';

let sassOptions = {
  // Rewrite urls synchronously
  rewriteUrl: (urlReference: { file: string, url: string }): string | null => {
    if (!urlReference.file) {
      return url;
    }

    return path.relative(path.dirname(outDir), path.join(path.dirname(file), url));
  }
}
```

### Using Variables

Variables can be used in a sass-url reference, in this case it should get remapped based on the value that gets created after the variables have been applied to ensure we are able to remap the entire url correctly.

Examples with variables:

```Scss
sass-url("#{$asset-path}/image.png");
```

```Scss
sass-url("./folder/#{$some-var}");
```

## Presets

Besides having a way to allow any sass-url rewriting logic, this specification also defines 2 presets that handle common url rewriting logic: `inline` and `relative`.

### The inline Preset

The `inline` rewrite preset is the most fool-proof method, it inlines all references assets into the output css file using data urls.

This preset should be exposed in `sass.url.inline`.

#### Inline Preset Usage

To use this preset you can pass an instance of this preset in the urlRewrite option.

```JS
import * as sass from 'sass';

const options = {
  rewriteUrl: sass.url.inline()
}
```

#### Inline Preset Implementation

The url rewrites go through the following steps:

- Preset takes in the canonical url of the sass file and sass-url reference value and join these urls to get the location of the actual asset that should get inlined. In Node.JS this would equal to `path.join(file, url)`.
- With the location of the asset we have to ensure it exists and than read the file. If the file does not exist it should throw an error, as specified in the [JavaScript API](#javascript-api).
- To create a data url we need to detect the media type of the asset, this can be done based on the extension and/or actual content of the file.
- Once we have the file's content and the media type we can continue to create a data url from it, to do this we create a string that starts with `data:` followed by the media type, followed by the base64 encoded value of the asset content. Resulting in: `data:${mediaType};base64,${base64(assetContent)}`
- After all this we add the asset location to the `stats.includedFiles` list that will get returned at the end of the sass processing and return the data url.

Example:

```JS
function inlinePreset() {
  return async (urlReference: { file: string, url: string }, done: (error: Error | null, url: string | null) => void): void => {
    try {
      let assetLocation = path.join(file, url);
      let assetContent = await fs.readFile(assetLocation);
      let mediaType = getMimeTypeFromExtension(path.extname(assetLocation));

      return `data:${mediaType};base64,${base64(assetContent)}`;
    } catch(err) {
      done(err, null);
    }
  }
}

sass.url.inline = inlinePreset;
```

### The relative Preset

The `relative` rewrite preset rewrites sass-url references in such a way that the output css file contains relative references to these assets. This has the benefit of sending smaller css files but also is less prone to errors and might result in a longer loading time due to a lot of http requests.

This preset should be exposed in `sass.url.relative`.

#### Relative Preset Usage

To use this preset you can pass an instance of this preset in the urlRewrite option.

This preset takes in one argument that should return the

```JS
import * as path from 'path';
import * as sass from 'sass';

const outputDir = path.join(__dirname, 'output');

const relativePathCreator = (fullUrl) => {
  return path.relative(outputDir, fullUrl);
}

const options = {
  rewriteUrl: sass.url.relative(relativePathCreator)
}
```

#### Relative Preset Implementation

TODO: Write
