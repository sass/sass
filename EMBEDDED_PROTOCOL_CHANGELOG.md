## 1.2.0

* Have the compiler treat several user-generated invalid responses as
  compilation errors rather than `ProtocolError`s:

  * Invalid function signatures in `CompileRequest.global_functions`.

  * Non-absolute URLs in `CanonicalizeResponse.result.url`,
    `ImportSuccess.source_map_url`, and `FileImportResponse.result.file_url`.

* Clarify that an invalid signature in a `HostFunction` should treat the current
  function as failing, rather than the `HostFunction`.

## 1.1.0

* Add a `charset` option that controls whether or not Sass emits a
  `@charset`/BOM for non-ASCII stylesheets.

## 1.0.0

* First stable release.

## 1.0.0-beta.18

* Add a `CompileRequest.source_map_include_sources` field that tells the
  compiler to embed the contents of all source files in the generated source
  maps.

## 1.0.0-beta.17

* Mark `ImportResponse.result` as optional. Importers should be able to return
  `null` to indicate that a file wasn't found.

## 1.0.0-beta.16

* Mark `CompileFailure.span` as mandatory. There's no instance where a
  compilation itself should fail without having a source to point to.

* Make it the compiler's responsibility to verify `HostFunction.signature`.
  This ensures that the host doesn't have to parse Sass code.

## 1.0.0-beta.15

* Pluralize `Calculation.arguments`.

* Explicitly document how hosts should handle calculations.

## 1.0.0-beta.14

* Add support for calculation values.

## 1.0.0-beta.13

* Add the `Value.HwbColor` type.

* Explicitly specify that compilers may choose which color types to use to
  represent each color.

## 1.0.0-beta.12

* Add the `Value.ArgumentList` type, as well as
  `FunctionCallResponse.accessed_argument_lists` to track which argument lists
  had their keywords accessed.

## 1.0.0-beta.11

* **Breaking change:** We now follow the [protocol buffer style guide]. This means:
  * Field names are now all underscore-separated rather than lower camel case.
  * Enums are now at the top-level with prefixes rather than surrounded in
    enclosing messages.

* Add `CompileRequest.quiet_deps` and `CompileRequest.verbose` flags to control
  how the compiler emits compilation warnings.

* Add a `CompileSuccess.loaded_urls` field that indicates the URLs that were
  loaded by a compilation.

* Clarify that `CompileRequest.StringInput.url` must be a canonical URL.

* Fix the documentation of `CanonicalizeRequest` to avoid referring to the
  outmoded `CanonicalizeResponse.result.file` field.

[protocol buffer style guide]: https://developers.google.com/protocol-buffers/docs/style

## 1.0.0-beta.10

* Add `VersionRequest.id` and `VersionResponse.id`.

## 1.0.0-beta.9

* Added `CanonicalizeRequest.fromImport` and `FileImportRequest.fromImport`
  fields to allow importers to correctly handle import-only files.

## 1.0.0-beta.8

* Added fields to support requesting and sending formatted errors and logs.
  * `CompileRequest.alert_color`
  * `CompileRequest.alert_ascii`
  * `CompileFailure.formatted`
  * `LogEvent.formatted`

* Remove `OutputStyle.NESTED` and `OutputStyle.COMPACT`. It's unlikely that any
  host would support those any time soon.

## 1.0.0-beta.7

* Use `4294967295` as the special ID for error messages that aren't caused by a
  specific request, since `-1` isn't representable as a `uint32`.

## 1.0.0-beta.6

* Changed `CompileResponse.id` and `ProtocolError.id` from `int32` to `uint32`
  to match the type of all other ID fields.

* Added protocol versions and created this changelog.

* Added the `VersionRequest` and `VersionResponse` messages.

* Delimit messages with varints rather than fixed-size integers.
