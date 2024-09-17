## 3.1.0

* Add the `EvaluateRequest` and `EvaluateResponse` messages.

## 3.0.0

* Add `Color` SassScript value.

* Remove `RgbColor`, `HslColor` and `HwbColor` SassScript values.

## 2.7.1

* Allow deprecation versions to be value of `fatal_deprecation` field.

## 2.7.0

* Add `containing_url_unused` field to `CanonicalizeResponse` and
  `FileImportResponse`.

## 2.6.0

* Add `fatal_deprecation`, `silence_deprecation`, and `future_deprecation`
  fields to `CompileRequest` and `deprecation_type` to `LogEvent`, implementing
  the [deprecations API].

[deprecations API]: accepted/deprecations-api.md

## 2.5.0

* Add `NodePackageImporter` as a built-in Package Importer, resolving `pkg:`
  URLs using the standards and conventions of the Node ecosystem.

## 2.4.0

* Add `CompileRequest.silent` option that suppresses all `LogEvent`s.

## 2.3.0

* Add a `Value.CompilerMixin` value type to represent first-class mixins.

## 2.2.0

* Deprecate the `Value.Calculation.CalculationValue.value.interpolation` option,
  and change how it's interpreted by the compiler.

## 2.1.0

* Use the Java package `com.sass_lang.embedded_protocol` and generate multiple
  Java files when generating Java code. This doesn't affect any other languages.

  Note: although this is technically a breaking change for Java users, 2.0.0 is
  so new that it's being rolled into that breakage rather than releasing 3.0.0
  immediately.

## 2.0.0

* The compilation ID for each `CompileRequest` and the various outbound requests
  associated with a given compilation is now encoded as part of the wire
  protocol, rather than being protobuf fields. This makes it easier for embedded
  compilers to efficiently dispatch requests across multiple threads without
  reparsing protocol buffers.

  * The `CompileRequest.id`, `CompileResponse.id`, `LogEvent.compilation_id`,
    `CanonicalizeRequest.compilation_id`, `ImportRequest.compilation_id`,
    `FileImportRequest.compilation_id`, and `FunctionCallRequest.compilation_id`
    fields have been removed.

* The following fields are now explicitly declared as proto3 `optional` fields:
  `ImportSuccess.source_map_url`, `LogEvent.span`, `SourceSpan.end`. These
  should no longer be considered unset when they have the default value, but
  only when they're unset at the protocol level. All non-`oneof` fields are
  mandatory. (`oneof` fields are still described as "optional" or "mandatory" in
  the specification text.)

* `CompileSuccess.loaded_urls` has been moved to `CompileResponse.loaded_urls`
  so it's available even when compilation fails.

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
