# Custom url resolution: Draft 1

_[(Issue)](https://github.com/sass/sass/issues/2535)_

## Table of Contents

- [Background](#background)
- [Summary](#summary)
- [Semantics](#semantics)
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

## Semantics

This proposal defines new behavior for `url()`, to allow users to rewrite url references to how it will be accessible on the browser.

It applies to both with and without quotation marks: `url("$url")` and `url($url)`.

### Steps

The steps of rewriting a url reference:

- Url reference gets extracted from the sass file (For more information see [Semantics](#semantics))
- The url reference gets passed into the rewrite url plugin which returns the final output url of this resource.
- This returned value gets used to replace the original url reference.

### JavaScript API

At the core of remapping the url references is the JavaScript API which allows users to return a new url reference, based on the canonical url of the sass file and original url reference.

The first parameter of the urlRewrite function is an object with the following values:

- `file`: The canonical url of the sass file that references the url.
- `url`: The url reference, for example with `url(file://../assets/test.png)` it would be `file://../assets/test.png`.

The second parameter of the urlRewrite function is an optional done callback that is used when performing asynchronous operations.

This callback function takes in two parameters:

- `error`: This can be null or an Error object, `null` means there is no error.
- `url`: This is the `string` that gets used to replace the url reference.

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

Variables can be used in a url reference, in this case it should get remapped based on the value that gets created after the variables have been applied to ensure we are able to remap the url as expected.

Examples:

```Scss
url("#{$asset-path}/image.png");
```

```Scss
url("./folder/#{$some-var}");
```
