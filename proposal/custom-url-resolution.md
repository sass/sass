# Custom `url(...)` resolution: Draft 1

_[(Issue)](https://github.com/sass/sass/issues/2535)_

## Table of Contents

- [Background](#background)
- [Summary](#summary)
  - [JavaScript API](#javaScript-api)
  - [CLI](#cli)
    - [Possible Values](#possible-values)
  - [Edge cases](#edge-cases)
- [Breaking Changes](#breaking-changes)

## Background

> This section is non-normative.

Many css features require the use of a `url()` import to reference resources from outside the sass files, however these files also need to exist on the eventual output directory and server. To ensure the references are valid the sass API should allow for the user to provide a way to remap and/or inline these resources.

## Summary

This proposal defines a standardized way to remap the url imports to the final location on the server or inline reference as well as providing some defaults for url imports remapping in the CLI.

This is accomplished by adding a callback function to the JavaScript API options as well as a cli option.

### JavaScript API

At the core of remapping the url imports is the JavaScript API which allows users to return a new url reference based on the sass file location and url reference. This function can return either a promise or utilise the provided callback function.

Callback syntax:

```TypeScript
let sassOptions = {
  // Rewrite url references
  rewriteUrl: (url: string, filepath: string, done: (error: Error, newUrl: string) => void) => {
    done(null, `data:${base64(url)}`);
  }
}
```

Promise syntax:

```TypeScript
let entryFilePath = '/index.scss';
let sassOptions = {
  // Rewrite url references
  rewriteUrl: async (url: string, filepath: string) => {
    if (url[0] === '.') {
      return path.relative(entryFilePath, path.join(filepath, url));
    } else {
      return url;
    }
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

```Scss
url("#{$asset-path}/image.png");
```

```Scss
url("./folder/#{$some-var}");
```

#### Unknown sass filepath

In case the sass compiler does not have a filepath for the originating sass file it is impossible to remap this url reference to the proper location on the server or inline this. In this case we should leave the url as is without trying to remap it in any way.

## Breaking Changes

This will not directly introduce any breaking changes as this new feature will be opt-in. However tools like Parcel and WebPack will probably want to use this and in turn cause users that relied on the non existing relative url rewriting to end up with a broken codebase. However this can be worked around by these tools by falling back to resolving relative to the Sass entry point if the file does not exist, however this might be dangerous behavior and should log a warning.
