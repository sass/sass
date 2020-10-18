# Custom url resolution: Draft 1

_[(Issue)](https://github.com/sass/sass/issues/2535)_

## Table of Contents

- [Background](#background)
- [Summary](#summary)
  - [Steps](#steps)
  - [JavaScript API](#javaScript-api)
  - [Edge cases](#edge-cases)
- [Syntax](#syntax)
- [Deprecation Process](#deprecation-process)

## Background

> This section is non-normative.

Many css features require the use of a url import to reference resources from outside the sass files, however these files also need to exist on the eventual output directory and server. To ensure the references are valid, the sass API should allow for the user to provide a way to remap and/or inline these resources.

## Summary

> This section is non-normative.

This proposal defines a standardized way to remap the url imports to the final location on the server or inline reference.

This is accomplished by running url references through the importer plugin(s) and url rewriting plugin(s) if any url rewriting plugin has been defined.

### Steps

The steps of a url reference import:

- Url reference gets extracted from the Sass file (For more information see [Syntax](#syntax))
- This reference gets remapped to the actual location on the filesystem by the importer plugin(s)
- The resolved reference gets passed into the rewrite url plugin pipeline which returns the output url of this resource.

If a certain resource cannot be remapped the url rewrite plugin will return `null`, if all rewrite plugins return `null` it should fallback to the original value of the url reference.

### JavaScript API

At the core of remapping the url imports is the JavaScript API which allows users to return a new url reference based on the filepath or content.

The first parameter this rewriteUrl function gets passed in is the importerResult, which is an object containing an optional `file` and `content` field.

- The `file` field is the location on disk of the given url resource, this can be used to create relative mappings or to simply read the file.
- The `content` field is a `Blob` which contains the content of the url resource, in this case the url rewriter should not try to read the file manually and use this instead as this is what the importer returned.

This function should call the second parameter, the `done` callback, whenever it is finished rewriting the url.

This callback has two parameters, `error` and `url`:

- The `error` parameter can be `null` or an `Error` object, this is used when an error occurred when trying to rewrite the url.
- The `url` parameter can be `null` or a `string`, this should be the new url as it can be found in the browser.

```TypeScript
// Rewrite url plugin/function
const rewriteUrlPlugin = async (importerResult: { file?: string, content?: Blob }, done: (error: Error | null, url: string | null) => void): void => {
  if (file && !content) {
    content = await fs.readFile(file);
  }

  done(null, content ? `data:${base64(content)))}` : null);
}

let sassOptions = {
  // An array of all rewrite url plugins
  rewriteUrl: [rewriteUrlPlugin]
}
```

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

This will not directly introduce any breaking changes as this new feature will be opt-in.

However tools like Parcel and WebPack will probably want to use this and in turn cause a breaking change as users are currently relying on the broken url logic of sass. However I strongly believe the impact of this will be very minor as in case a file does not exist the urlRewrite plugin can choose to just ignore this url reference.
