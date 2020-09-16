# Custom `url(...)` resolution: Draft 1

_[(Issue)](https://github.com/sass/sass/issues/2535)_

## Table of Contents

- [Background](#background)
- [Summary](#summary)
  - [JavaScript API](#javaScript-api)
  - [CLI](#cli)
    - [Possible Values](#possible-values)
  - [Edge cases](#edge-cases)

## Background

> This section is non-normative.

Many css features require the use of a `url()` import to reference resources from outside the sass files, however these files also need to exist on the eventual output directory and server. To ensure the references are valid the sass API should allow for the user to provide a way to remap and/or inline these resources.

This will require a function in the JavaScript API as well as a cli option with some presets.

## Summary

This proposal defines a standardized way to remap the url imports to the final location on the server or inline reference as well as providing some defaults for url imports remapping in the CLI.

This is accomplished by adding a callback function to the JavaScript API options as well as a cli option.

### JavaScript API

At the core of remapping the url imports is the JavaScript API which allows users to return a new url reference based on the sass file location and url reference. This function can return either a promise or utilise the provided callback function.

Callback syntax:

```TypeScript
let sassOptions = {
  // Rewrite url references
  rewriteUrl: (url: string, filepath: string, done: (newUrl: string) => void) => {
    done(`data:${base64(url)}`);
  }
}
```

Promise syntax:

```TypeScript
let sassOptions = {
  // Rewrite url references
  rewriteUrl: async (url: string, filepath: string) => {
    done(`data:${base64(url)}`);
  }
}
```

### CLI

In the CLI some defaults can be provided, these can be configured using the `--rewrite-url` flag.

#### Possible Values

These are the possible values for the CLI flag:

- `off`: Does not do any rewriting (default value)
- `local`: Rewrites all relative url imports to be relative to the main file, rewriting all url imports from sass files that are in a different folder than the main file.
- `inline`: Inlines url imports using Base64 in a `data:...` url

### Edge cases

#### Variables in the url reference

Variables can be used in a url reference, in this case it should get remapped based on the value that gets created after the variables have been applied to ensure we are able to remap the url as expected.

Examples:

```Sass
url("#{$asset-path}/image.png");
```

```Sass
url("./folder/#{$some-var}");
```
