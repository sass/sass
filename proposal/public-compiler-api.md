# Public Compiler API

## Background

> This section is non-normative.

Sass compiler implementations tend to assume projects will have few entrypoint Sass files
such that running the compiler against each without a cache is "fast enough".
However, for projects with substantially more entrypoints, ones that depend
on a bundler (like webpack) to do final concatenation, or using [css-modules][], there can be a significant cost to reparsing and processing unchanged files across the project.

In common case of projects with many entrypoints, each entry likely imports from a
large set of shared configuration, funtions and mixins. These dependencies often change infrequently
and a significant amount of compile time is spent compiling the same files in the
context of a new entrypoint file.

In addition, without an ability to reuse previous work, certain project patterns with Sass's
module system are also impossible. Consider this case using webpack:

**App.js**

```js
import 'styles.scss'
```

**styles.scss**;

```scss
@use 'bootstrap' with (
  $btn-color: red
)

html {
  font-size: 10px;
}
```

Further down in the app, a component may wish to reference the previously configured
library

**button.scss**

```scss
@use 'bootstrap/buttons' as buttons

.my-btn {
  color: buttons.$btn-color;
}
```

And the js file using webpack's `import` syntax for styles:

**Button.js**

```js
import 'button.scss'

// some web, vue, react, etc component
```

If the user is used module caching behavior like in Node.js, they would be suprised
that `$btn-color` is its default not `red`.

[css-modules]: https://github.com/css-modules/css-modules

## Summary

Sass implementations should provide a constructable Compiler class whose instance exposes `render` and `renderSync` methods for compiling Scss. Method calls providing a `file` have their results (as well as the results of and dependencies) cached by absolute filename on the compiler instance. Further calls to `render(Sync)` will return the cached value if not invalidated. Calls providing `data` would only have dependencies cached. Calls providing **both** `data` and `file`
would process `data` using the resolved absolute path of `file` as it's cache key.

The compiler instance would also expose an `invalidate` method which marks a file
and all of it's dependents as needing recompilation.

## Design choices

> This section is non-normative.

It's important that the compiler API allow delegating cache control to the consumer.
Bundlers like webpack, or Parcel provide their own mechanisms for caching, watching, invalidating and topographically rebuilding entrypoints based on file changes. The Sass API
should allow integration into those mechanisms.

This does not preclude the possibility of Sass implementations providing their own
watching mechanisms. This feature could be built on top of the proposed API

## API

```ts
interface CompilerClass {
  new (): Compiler
}

interface Compiler {
  render(options: { data?: string, file?: string, ... }): Promise<string>

  renderSync(options: { data?: string, file?: string, ... }): string

  invalidate(file: string): void
}

```

## Semantics

- When `compiler.render(Sync)` is called

  - Let `import-cache` be a `Map<string, string>` on `compiler`

  - Let `module-cache` be a `Map<string, module>` on `compiler`

  - If options provides only `file`

    - Let `data` be the contents of `file`

  - If `file` is set

    - Let `filepath` be the absolute path of `file`

  - If `filepath` and `import-cache` has key `filepath`

    - Return string value for key `filepath`

  - Overwise compile `data`

    - For each `@import` rule

      - Let `filepath` be the absolute path of the import

      - If `import-cache` has key `filepath`

        - Use string value as the compilation result

      - Otherwise compile `filepath` using the same and set key `filepath`
        on `import-cache` to the result

    - For each `@use` rule

      - Let `filepath` be the absolute path of the import

      - If `module-cache` has key `filepath`

        - Use `module` to resolve local references

      - Otherwise compile using the same logic and set key `filepath` on
        `module-cache` to the result

    - Let `result` be the string value of the compilation

    - If `filepath`

      - Let `members` be the `module` value of `data`

      - Set key `filepath` on `import-cache` to `result`

      - Set key `filepath` on `module-cache` to `members`

    - Return `result`

* When `compiler.invalidate(file)` is called

  - Let `filepath` be the absolute path the `invalidate` argument `file`

  - Let `dependents` be a list of the file paths of all direct and transitive **dependents** of `file`

  - Delete `filepath` from `import-cache`

  - Delete `filepath` from `module-cache`

  - For each `dependent-filepath` in `dependents`

    - Remove `dependent-filepath` from `import-cache`

    - Remove `dependent-filepath` from `module-cache`
