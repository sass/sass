# Custom url resolution: Draft 1

_[(Issue)](https://github.com/sass/sass/issues/2535)_

## Table of Contents

- [Background](#background)
- [Summary](#summary)
  - [JavaScript API](#javaScript-api)
  - [CLI](#cli)
    - [Possible Values](#possible-values)
  - [Edge cases](#edge-cases)
- [Syntax](#syntax)
- [Deprecation Process](#deprecation-process)

## Background

> This section is non-normative.

Many css features require the use of a url import to reference resources from outside the sass files, however these files also need to exist on the eventual output directory and server. To ensure the references are valid the sass API should allow for the user to provide a way to remap and/or inline these resources.

## Summary

> This section is non-normative.

This proposal defines a standardized way to remap the url imports to the final location on the server or inline reference as well as providing some defaults for url imports remapping in the CLI.

This is accomplished by adding a callback function to the JavaScript API options as well as a cli option.

### JavaScript API

At the core of remapping the url imports is the JavaScript API which allows users to return a new url reference based on the sass file location and url reference.

This function can return either a promise or utilise the provided callback function. In case an error gets returned or thrown the sass compilation should fail and return this error, this will likely only happen for files that do not exist.

When a filepath of the originating sass file is unknown, it should also still call this callback but use null as the filepath, this way all url resolution is ensured, the end user can decide whether to handle this or not.

Callback syntax:

```TypeScript
let sassOptions = {
  // Rewrite url references
  rewriteUrl: (url: string, filepath: string | null, done: (error: Error, newUrl: string) => void) => {
    done(null, filepath ? `data:${base64(fs.readFileSync(path.join(path.basename(filepath), url)))}` : url);
  }
}
```

Promise syntax:

```TypeScript
let entryFilePath = '/index.scss';
let sassOptions = {
  // Rewrite url references
  rewriteUrl: async (url: string, filepath: string | null) => {
    if (filepath && url[0] === '.') {
      return path.relative(entryFilePath, path.join(path.join(path.basename(filepath), url));
    } else {
      return url;
    }
  }
}
```

### CLI

In the CLI some defaults can be provided, these can be configured using the `--rewrite-url` flag.

This should require the sass filepath to be known to work, in case this is not defined it should throw an error.

#### Possible Values

These are the possible values for the CLI flag:

- `off`: Does not do any rewriting (default value)
- `local`: Rewrites all relative url imports to be relative to the main file, rewriting all url imports from sass files that are in a different folder than the main file.
- `inline`: Inlines relative url imports using Base64 in a `data:...` url.

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

## Syntax

This proposal applies to the `url(...)` function, both with and without the quotation marks. This ensures this applies to the entire css specification for `url(...)`.

This proposal does not introduce any new Syntax.

## Deprecation Process

This will not directly introduce any breaking changes as this new feature will be opt-in. However tools like Parcel and WebPack will probably want to use this and in turn cause users that relied on the non existing relative url rewriting to end up with a broken codebase. However this can be worked around by these tools by falling back to resolving relative to the Sass entry point if the file does not exist, however this might be dangerous behavior and should log a warning.
