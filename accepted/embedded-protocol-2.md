# Embedded Protocol Version 2: Draft 1.1

*([Issue 1](https://github.com/sass/sass/issues/3579), [Issue
2](https://github.com/sass/sass/issues/3575), [Issue
3](https://github.com/sass/sass/issues/3577),
[Changelog](embedded-protocol-2.changes.md))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Length Before Compilation ID](#length-before-compilation-id)
    * [Cross-Compilation State](#cross-compilation-state)
    * [Outbound Request IDs](#outbound-request-ids)
* [Overview](#overview)
  * [Packet Structure](#packet-structure)
* [RPCs](#rpcs)
  * [Type Definitions](#type-definitions)
  * [ID Requirements](#id-requirements)
  * [Optional and Mandatory Fields](#optional-and-mandatory-fields)
* [Error Handling](#error-handling)
* [Protocol Buffer](#protocol-buffer)
  * [ID Fields](#id-fields)
  * [Optional Fields](#optional-fields)
  * [Loaded URLs](#loaded-urls)

## Background

> This section is non-normative.

Since the [embedded Sass protocol] was first released, a number of improvements
have been suggested by embedded host authors that would either be
backwards-incompatible, or would require cumbersome redundancies in the protocol
to make them backwards-compatible.

[embedded Sass protocol]: ../spec/embedded-protocol.md

In the meantime, to provide an efficient Sass CLI along with embedded hosts, the
Dart Sass embedded compiler was merged into the main Dart Sass executable.
Because this is effectively also a breaking change in how hosts obtain and
invoke the compiler, it seemed like a good opportunity to make the other
breaking protocol changes that had been piling up.

## Summary

> This section is non-normative.

This proposal makes three breaking changes to the embedded Sass protocol:

* Replace the compilation IDs in the protobufs with one directly encoded in the
  wire protocol so that compiler implementations can efficiently route requests
  to different workers without reparsing messages.

* Use optional fields as defined in protocol buffers 3.15.0 instead of treating
  default field values as absent.

* Move `CompileSuccess.loaded_urls` to `CompileResponse.loaded_urls` so it's
  available even when compilation fails.

### Design Decisions

#### Length Before Compilation ID

This proposal places the compilation ID for each request *after* the length. The
length is defined as the length of the protocol buffer plus the length of the
compilation ID.

Another possible approach would be to have the compilation ID first, then the
length, and have the length list only the length of the protocol buffer itself.
This approach would have the benefit of being somewhat easier to decode, since
the only length-delimited chunk of input would be parsed as a unit by the
protocol buffer parser rather than needing to be sliced out of the (compilation
ID, protocol buffer) pair.

However, the embedded protocol is intended to be transport-independent, and not
all transports will necessarily require an explicit length encoding at all.
Although today all use of the embedded protocol is over stdin/stdout which
requires a length delimiter, it's likely that we'll eventually add WASM support
as well which won't.

We want to make it easy for the same compiler and host codebases to support
multiple transports. Making the length-delimiting process a layer that can be
transparently applied to the same message blobs that are used in other transport
layers makes it easier for the underlying endpoint logic to just work with those
blobs regardless of where they came from.

For languages that can parse protocol buffers from a subsequence of binary
content without copying, it should be easy to slice off the compilation ID and
parse the remaining binary data. For those that can't, the true length of the
protocol buffer can be determined by taking the given length and subtracting the
length of the compilation ID, which is given by the following table:

| compilation ID          | length in bytes |
|-------------------------|-----------------|
| [0, 128)                | 1               |
| [128, 16384)            | 2               |
| [16384, 2097152)        | 3               |
| [2097152, 268435456)    | 4               |
| [268435456, 4294967296) | 5               |

#### Cross-Compilation State

We have a [future goal] to (optionally) share state *across* compilations, to
more efficiently compile projects with many small entrypoints where the bulk of
the complexity is in static shared libraries. If/when we support this, there
could be two broad implementation strategies for a compiler with worker-based
parallelism like Dart Sass:

[future goal]: https://github.com/sass/sass/issues/2745

1. Run each compilation in a separate worker and keep shared state in one or
   more shared workers. This allows for more parallelism between compilations,
   but requires state (or requests for information about state) to be serialized
   across worker boundaries, adding a potentially substantial amount of overhead.

2. Run all compilations that share state in a single worker, allowing
   zero-overhead access to the shared state but requiring those compilations to
   run serially rather than in parallel.

It's not clear which of these will be more efficient in which circumstances,
although option 1 is certainly substantially more complex to implement. The
protocol as listed here—without an explicit `CompilationRequest.id` field—is
only compatible with option 1, assuming that each compilation ID corresponds to
a separate worker as intended.

However, this isn't a fatal flaw. It would be a non-breaking change to add
`CompilationRequest.id` (and `*.compilation_id`) back later on if we decide to
support option 2. Hosts that were built to target the current version of the
protocol wouldn't set `CompilationRequest.id`, which means it would default to
zero, which will work fine since they're already ensuring each
`CompilationRequest` has a different wire-level compilation ID.

#### Outbound Request IDs

Given that each compilation is expected to run single-threaded in and of itself,
there's theoretically no more need for fields like `ImportRequest.id`. Each
compilation ID will only have one request at a time, so we could just declare
that any response with a given compilation ID is for the single outstanding
request.

However, the *expectation* that each compilation be single-threaded isn't a
*requirement*. One could imagine a multithreaded Sass compiler that actually is
capable of fielding multiple concurrent requests as it compiles independent
chunks of a given stylesheet or resolves loads eagerly. We don't want to cut off
this possibility, so we retain the outbound request IDs.

## Overview

### Packet Structure

Replace the last paragraph of the [embedded protocol overview] with:

[embedded protocol overview]: ../spec/embedded-protocol.md#overview

Each message in the embedded protocol is sent as a *packet* which contains two
values: an unsigned [varint] up to 32 bits long known as the "compilation ID",
and a protocol buffer that contains the protobuf message. For streams (like
standard input and output) that don't have built-in message boundaries, every
packet must begin with another unsigned varint indicating the length in bytes of
the remaining message (*including the compilation ID*). This matches the best
practice described in [the protocol buffer documentation].

Because JavaScript can't easily represent integers larger than 2^53 - 1, the
length may be no more than 2^53 - 1. Because it's so unlikely that this will
come up in practice, implementations are not required to verify it.

[varint]: https://developers.google.com/protocol-buffers/docs/encoding#varints
[the protocol buffer documentation]: https://developers.google.com/protocol-buffers/docs/techniques#streaming

For a length-delimited stream, each packet has the following structure:

```
╔══════════╦══════════════════╗
║ varint   ║ Length           ║
╠══════════╬══════════════════╣
║ varint   ║ Compilation ID   ║
╠══════════╬══════════════════╣
║ protobuf ║ Protobuf Message ║
╚══════════╩══════════════════╝
```

## RPCs

### Type Definitions

Replace the following RPC type definitions:

* *Requests* usually include a mandatory `uint32 id` field so that the other
  endpoint can respond, except for `CompileRequest` which uses the [compilation
  ID] as its ID. All request message types end in `Request`.

  [compilation ID]: #packet-structure

* *Responses* usually include a mandatory `uint32 id` field whose value must be
  the same as their associated request's `id`, except for `CompileResponse`
  which uses the compilation ID as its ID. All response message types begin with
  the corresponding request name and end with `Response`.

### ID Requirements

Replace the paragraph that beings "Implementations must guarantee that they use
a unique `id` for every request" with:

Each endpoint must guarantee that each request's `id` doesn't match the `id` of
any other outstanding request with the same [compilation ID] from that endpoint.
The same `id` may be used for an inbound request and an outbound request, and
the same `id` may be used for two requests with different compilation IDs. The
host must similarly guarantee that a `CompileRequest`'s compilation ID doesn't
match the compilation ID of any other outstanding `CompileRequest`. The compiler
must ensure that all outbound requests' compilation IDs match that of the
`CompileRequest` that triggered its associated compilation.

The compilation ID 0 is reserved for `VersionRequest` and `VersionResponse`,
since they're not specific to any individual compilation.

The compilation ID and normal request `id` `4294967295` is reserved for [error
handling]. (This is the maximum number representable by a `uint32`.)

[error handling]: #error-handling

### Optional and Mandatory Fields

Replace the paragraphs about optional and mandatory fields with:

If a field is not optional, the the endpoint that sends that message must
guarantee that it's set to a meaningful value, and the endpoint that receives it
must reject the message if it's not set. Because protocol buffers allow all
`oneof` fields to be unset, the spec describes those that require values as
"mandatory".

## Error Handling

Append to the first paragraph:

The [compilation ID] must match the compilation ID of the request or response
that triggered the error.

## Protocol Buffer

### ID Fields

Remove the following fields:

* `CompileRequest.id`
* `CompileResponse.id`
* `LogEvent.compilation_id`
* `CanonicalizeRequest.compilation_id`
* `ImportRequest.compilation_id`
* `FileImportRequest.compilation_id`
* `FunctionCallRequest.compilation_id`

### Optional Fields

Mark the following fields as optional, and update each one's specification to
check if they're unset instead of the default values:

* `ImportSuccess.source_map_url`
* `LogEvent.span`
* `SourceSpan.end`

### Loaded URLs

Remove `CompileSuccess.loaded_urls` and add `CompileResponse.loaded_urls` with
the same specification.
