# Custom url resolution: Draft 1

_[(Issue)](https://github.com/sass/sass/issues/2535)_

## Table of Contents

- [Background](#background)
- [Summary](#summary)
  - [Steps](#steps)
  - [JavaScript API](#javaScript-api)
- [Semantics](#semantics)
  - [Using Variables](#using-variables)
- [Deprecation Process](#deprecation-process)

## Background

> This section is non-normative.

Many css features require the use of a url reference to reference resources from outside the sass files, however these files also need to exist on the eventual output directory and server. To ensure the references are valid, the sass API should allow for the user to provide a way to remap and/or inline these resources.

## Summary

> This section is non-normative.

This proposal defines a standardized way to remap the url references to the final location on the server or inline reference.

This is accomplished by running url references through the url rewriting plugin if a url rewriting plugin has been defined.

### Steps

The steps of a url reference import:

- Url reference gets extracted from the Sass file (For more information see [Semantics](#semantics))
- These reference gets passed into the rewrite url plugin which returns the output url of this resource.

### JavaScript API

At the core of remapping the url references is the JavaScript API which allows users to return a new url reference, based on filepath and original url reference.

The first parameter of the urlRewrite function is an object with the following values:

- `file`: The file path of the sass file that references the url.
- `entry`: The entry file path that has been used to call the sass compiler.
- `url`: The original url reference, for example with `url(file://../assets/test.png)` it would return be `file://../assets/test.png`.

The second parameter of the urlRewrite function is an optional done callback that is used when performing asynchronous operations.

This callback function takes in two parameters:

- `error`: This can be null or an Error object, `null` means there is no error.
- `url`: This is the `string` that gets used to replace the url reference.

Asynchronous example:

```TypeScript
let sassOptions = {
  // Rewrite urls asynchronously
  rewriteUrl: async (importerResult: { file: string, entry: string, url: string }, done: (error: Error | null, url: string | null) => void): void => {
    if (file && !content) {
      content = await fs.readFile(path.join(path.dirname(file), url));
    }

    done(null, content ? `data:${base64(content)))}` : url);
  }
}
```

Synchronous example:

```TypeScript
let sassOptions = {
  // Rewrite urls synchronously
  rewriteUrl: (importerResult: { file: string, entry: string, url: string }): string | null => {
    if (!importerResult.file) {
      return url;
    }

    return path.relative(path.dirname(entry), path.join(path.dirname(file), url));
  }
}
```

## Semantics

This proposal defines an overload for `url()`, to allow users to rewrite url references to how it will be accessible on the browser.

It applies to both with and without quotation marks: `url("$url")` and `url($url)`.

### Using Variables

Variables can be used in a url reference, in this case it should get remapped based on the value that gets created after the variables have been applied to ensure we are able to remap the url as expected.

Examples:

```Scss
url("#{$asset-path}/image.png");
```

```Scss
url("./folder/#{$some-var}");
```

## Deprecation Process

This will not directly introduce any breaking changes as this new feature will be opt-in.

However tools like Parcel and WebPack will probably want to use this and in turn cause a breaking change as users are currently relying on the broken url logic of sass. However I strongly believe the impact of this will be very minor as in case a file does not exist the urlRewrite plugin can choose to ignore this url reference.
