# Shared Resources in JavaScript API

*([Issue](https://github.com/sass/sass/issues/3296))*

This proposal adds an API design for sharing resources across multiple
invocations of the Sass compiler's JavaScript API. This will provide Sass's
users with a more efficient way of running Sass compilations across multiple
files.

## Table of Contents

* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Parity across JavaScript API interfaces](#parity-across-javascript-api-interfaces)
    * [No shared state](#no-shared-state)
* [API](#api)
  * [Types](#types)
    * [Compiler](#compiler)
    * [dispose()](#dispose)

## Summary

Currently, the JavaScript API for Sass only accommodates a single compilation
per process. In practice, we have observed build tools are compiling multiple
times in response to a single user action. For instance, Vue.js authors using
Vite will see a Sass compilation for each `<style lang="scss">` tag that appears
in their codebase.

While processes can be spun up and down quickly, the combined time can add up to
a noticeable impact on performance. The embedded client supports long running
processes, and this proposal adds a method for the embedded host to manage the
lifecycle of these processes through a Compiler interface.

### Design Decisions

#### Parity across JavaScript API interfaces

When using the Compiler interface, users may still want to use portions of the
JavaScript API unrelated to compilation. The Compiler interface will be nearly
identical to the top level Sass interface. This allows users to replace
instances of the imported `sass` class with an instance of the compiler.

In environments without access to a long-running compiler -- for instance, the
Dart Sass implementation -- the Compiler interface will continue to perform a
single compilation per process.

Notable differences are that the Compiler interface will have additional methods
to manage the process lifecycle, and will not contain the legacy API.

#### No shared state

This proposal does not change how a single compilation is done, and no state is
shared across compilations. Options and importers must be set for each
compilation. Future enhancements may introduce [shared state], but this proposal
only adds the ability to run multiple compilations on a single process.

[shared state]: https://github.com/sass/sass/issues/3296

This also means that the proposal makes no assertions about whether file content
has changed. It is also up to the user to determine when to start and stop a
long-running compiler process.

## API

### Types

#### Compiler

* If an environment supports long-running processes:

  * If an internal reference of `runningProcess` exists, throw an error.

  * Start the process.

  * Store an internal reference to the process as `runningProcess`.

* Return an instance of the Sass JS API.

```ts
export class Compiler {
```

#### dispose()

* If an environment supports long-running processes:

  * If no internal reference of `runningProcess` exists, throw an error.

  * Stop the process referred to by `runningProcess`.

  * Remove the internal reference to `runningProcess`.

* Replace all members of Compiler with getters or methods that throw an error.

* Return `true`.

```ts
  dispose(): Boolean;
}
```
