# Custom url resolution: Draft 1

_[(Issue)](https://github.com/sass/sass/issues/2535)_

## Table of Contents

- [Background](#background)
- [Summary](#summary)
  - [Example](#example)
- [Syntax](#syntax)
  - [Steps](#steps)
  - [JavaScript API](#javascript-api)
  - [Using Variables](#using-variables)

## Background

> This section is non-normative.

Many css features require the use of a url reference to reference resources from outside the sass files, however these files also need to exist on the eventual output directory and server. To ensure the references are valid, the sass API should allow for the user to provide a way to remap and/or inline these resources.

## Summary

> This section is non-normative.

This proposal defines a standardized way to remap the url references to the final location on the server or inline reference.

This is accomplished by running url references through the url rewriting plugin if a url rewriting plugin has been defined.

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

## Syntax

This proposal introduces logic for a new function: `sass-url("...")`, this function allow users to define a url that gets rewritten based on the config to ensure it can be loaded correctly from the browser.

### Steps

Whenever a url reference is encountered in a sass file the following steps should be executed:

- A url reference is encountered in the sass file
- From the url function we extract the parameter value
- Rewrite the parameter value if it contains any variables ([see using variables](#using-variables))
- Pass the parameter value to the url rewrite plugin along with the canonical url of the current sass file
- This plugin than returns a string or calls the done callback with a string value, if it returns anything else or nothing it should throw an error ([see JavaScript API](#javascript-api))
- The new url value that we received from the url rewrite plugin than gets used to replace the original url value

_Note: if there is no urlRewrite plugin/function none of these steps should be executed._

### JavaScript API

At the core of remapping the url references is the JavaScript API which allows users to return a new url reference, based on the canonical url of the sass file and original url reference.

The first parameter of the urlRewrite function is an object with the following values:

- `file`: The canonical url of the sass file that references the url.
- `url`: The url reference, for example with `sass-url(file://../assets/test.png)` it would be `file://../assets/test.png`.

The second parameter of the urlRewrite function is an optional done callback that is used when performing asynchronous operations.

This callback function takes in two parameters:

- `error`: This can be null or an Error object, `null` means there is no error.
- `url`: This is the `string` that gets used to replace the url reference. This cannot be null or anything else, otherwise sass will throw an error.

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

Variables can be used in a url reference, in this case it should get remapped based on the value that gets created after the variables have been applied to ensure we are able to remap the entire url correctly.

Examples with variables:

```Scss
sass-url("#{$asset-path}/image.png");
```

```Scss
sass-url("./folder/#{$some-var}");
```
