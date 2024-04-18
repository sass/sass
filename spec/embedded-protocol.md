# The Embedded Sass Protocol

The Embedded Sass Protocol is a bidirectional protocol for communicating between
a Sass implementation and a host environment. It allows the host environment to
invoke the Sass compiler on source files, and to define custom functions and
importers in the host language.

Sass implementations are *not* required to support the embedded protocol.
However, if they do, they must adhere to the specification given in this file
and [`embedded_sass.proto`] for the compiler endpoint.

[`embedded_sass.proto`]: embedded_sass.proto

## Table of Contents

* [Overview](#overview)
  * [Packet Structure](#packet-structure)
* [RPCs](#rpcs)
  * [Type Definitions](#type-definitions)
  * [ID Requirements](#id-requirements)
  * [Optional and Mandatory Fields](#optional-and-mandatory-fields)
* [Error Handling](#error-handling)
* [Host Language API](#host-language-api)
  * [Immutability](#immutability)
  * [Indexing](#indexing)
  * [Assertions](#assertions)
  * [Strings](#strings)
  * [Numbers](#numbers)
  * [Colors](#colors)
  * [Lists](#lists)
  * [Maps](#maps)
  * [Booleans](#booleans)
  * [Null](#null)
  * [Calculations](#calculations)
  * [Functions](#functions)
* [Versioning](#versioning)

## Overview

This protocol operates between two endpoints over a bidirectional stream. One of
these endpoints, the *compiler*, is responsible for compiling Sass stylesheets
to CSS. The other, the *host*, is responsible for telling the compiler what to
compile and for providing implementations of custom importers and functions.

Messages are sent between the host and the compiler in the form of [protocol
buffers][], using a custom RPC system [defined below][]. The messages and
services that comprise this protocol are defined in [the `.proto` file][]
included in this repository. Most messages are *requests* which require the
other endpoint to produce a *response*, but some are *events* which require no
response.

[protocol buffers]: https://developers.google.com/protocol-buffers/
[defined below]: #rpcs
[the `.proto` file]: embedded_sass.proto

In principle this protocol can work over any bidirectional stream capable of
carrying protocol buffers. However, it's expected that most hosts will invoke
the compiler as a subprocess and communicate using binary protocol buffers over
its standard input and output streams.

### Packet Structure

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

All RPCs are wrapped in an outer message that indicates the RPC's type using [a
oneof field][]. There are two wrapper messages:

[a oneof field]: https://developers.google.com/protocol-buffers/docs/proto3#oneof

* `InboundMessage` is sent from the host to the compiler.
* `OutboundMessage` is sent from the compiler to the host.

The host must only send `InboundMessage`s to the compiler, and the compiler must
only send `OutboundMessage`s to the host.

Each wrapper message contains exactly one RPC. This protocol defines four types
of RPC:

* *Requests* usually include a mandatory `uint32 id` field so that the other
  endpoint can respond, except for `CompileRequest` which uses the [compilation
  ID] as its ID. All request message types end in `Request`.

  [compilation ID]: #packet-structure

* *Responses* usually include a mandatory `uint32 id` field whose value must be
  the same as their associated request's `id`, except for `CompileResponse`
  which uses the compilation ID as its ID. All response message types begin with
  the corresponding request name and end with `Response`.

* *Events* may not be responded to and include no `id` field. All event message
  types end with `Event`.

* The `ProtocolError` message, which is sent when one endpoint detects that the
  other is doing something invalid. See [Error Handling](#error-handling) below.

The protocol also defines some messages whose names don't end with `Request`,
`Response`, or `Event`. These are used as structures shared between different
RPCs.

### ID Requirements

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

### Optional and Mandatory Fields

If a field is not optional, the the endpoint that sends that message must
guarantee that it's set to a meaningful value, and the endpoint that receives it
must reject the message if it's not set.

## Error Handling

When the compiler detects that the host is violating this protocol, it must send
a `ProtocolError` message to the host. If the error was detected when processing
a request, the `ProtocolError` must have its `id` field set to the request's id.
Otherwise, even if the error was detected while processing a response with an
id, the `id` field must be set to `4294967295` (the maximum number representable
by a `uint32`). The [compilation ID] must match the compilation ID of the
request or response that triggered the error.

When the host detects that the compiler is violating this protocol, it does not
need to send a `ProtocolError` message to the compiler. Instead, it should
expose an error to the host's consumers and close the connection with the
compiler.

An error occurs whenever any requirements set out by this protocol (including
the documentation in `embedded_sass.proto`) are violated. This includes, but is
not limited to:

* Sending data that can't be parsed as an `InboundMessage` (for the compiler) or
  an `OutboundMessage` (for the host).

* Sending a request with an ID that's in use by another in-flight request.

* Sending a response with an ID that doesn't correspond to an in-flight
  request's ID.

* Sending a response with an ID that corresponds to the ID of an in-flight
  request ID of the incorrect type.

* Sending a message with a `null` value for a mandatory field.

The `ProtocolError` message must *not* be used to report Sass language errors.

## Host Language API

Although not strictly part of the protocol, the host language will presumably
provide an API for reading and manipulating SassScript values so that custom
functions can be written in the host language. In order to ensure that custom
functions will behave consistently with built-in Sass functions, the host
language should provide APIs that meet the following guidelines.

The [Dart `Value` API][] is a good example of an object-oriented API that
follows these guidelines.

[Dart `Value` API]: https://pub.dartlang.org/documentation/sass/latest/sass/Value-class.html

### Immutability

All SassScript values are immutable, and the API should preserve that fact. No
API calls should be able to modify any SassScript values, including collections
like lists and maps. Instead, API calls should be provided to return new values
with adjusted contents or to copy values into mutable host-language objects.

If API calls are provided that return a new versions of an object with adjusted
contents, metadata for the returned object (such as the type of list separator
or a number's units) should match that of the original object.

### Indexing

SassScript values use index 1 to refer to the first element and -1 to refer to
the final element. The index 0 is invalid. Furthermore, indexes in Sass strings
refer to [Unicode code points][], not bytes or UTF-16 code units. The API should
provide a means to convert between Sass's indexing scheme and the host
language's indexing scheme, and should encourage authors to treat any indexes
they're passed as Sass-style indexes rather than host-language-style indexes.

[Unicode code points]: https://en.wikipedia.org/wiki/Code_point

### Assertions

The API should provide an easy means to assert that values are the expected type
and to produce a useful error if they aren't. They should *not* provide a means
to assert that a value is a list, though, since all Sass values should be
treated as lists (see below).

### Strings

API users should be encouraged to return quoted strings unless there's a
particular reason not to.

Two strings are equal if they have the same text, regardless of whether either
is quoted or not.

### Numbers

The API should provide additional assertions for numbers:

* that the number doesn't have any units;
* that the number's units are [compatible][] with given expected units;
* that the number is an integer, which for the purposes of Sass numbers means
  that its numeric value is within 1e-11 of an integer;
* that the number is in a given range, where being within 1e-11 of the top or
  bottom of that range is considered being equal to the top or bottom.

[compatible]: https://www.w3.org/TR/css-values-4/#compat

The API should also provide means of converting a number to the equivalent
number with different-but-compatible units, and for returning it as the host
language's integer type if it is an integer.

Two numbers are equal if they have [compatible][] units, and if their numerical
value (with normalized units) are within 1e-11 of one another. A hash code with
the same equality semantics can be generated for a number `x` by rounding
`x * 1e11` to the nearest integer and taking the hash code of the result.

### Colors

Colors are represented by the `Color` value type. The API should provide means
of changing one or more channels of a color while leaving other channels as-is.

Two [legacy colors] are equal if their RGB forms have the same red, green, blue
channels and alpha channels equal rounded to the nearest 1e-11.

Two non-legacy colors are equal if they are in the same color space and their
channel and alpha values are equal rounded to the nearest 1e-11.

[legacy colors]: ../accepted/color-4-new-spaces.md#legacy-color

### Lists

In Sass, every value counts as a list. Maps count as unbracketed comma-separated
lists of two-element unbracketed space-separated key-value lists, and all other
non-list values count as lists that contain that value. The API should make it
easy to treat every value as a list, and should discourage treating values
passed as `Value.List`s specially.

API users should be encouraged to return unbracketed comma-separated lists
unless there's a particular reason not to.

Two lists are equal if they have the same elements, separator, and if they're
both bracketed or both unbracketed. An empty list is equal to an empty map.

`Value.ArgumentList`s should be exposed the same way as any other list, except
that it should also provide access to the keyword map. For object-oriented host
languages, an argument list's class should be a subtype of normal list's. It
should be considered equal to a list with the same elements, regardless of its
keywords.

### Maps

Although maps are transferred as lists of pairs, they should be exposed to the
host language as maps that can be indexed by key, using the notions of equality
described for each type.

Two maps are equal if they have equal keys that map to equal values, regardless
of the order of the keys in the map. An empty map is equal to an empty list.

### Booleans

The `True` and `False` messages are each singletons representing the Sass values
`true` and `false`, respectively. In Sass, all values other than `false` and
`null` can be used to represent truth, so the API should provide an easy way to
tell if a value is "truthy" (one of those values) or "falsey" (`false` or
`null`). It should encourage users to check this rather than directly testing
for `true` or `false`.

Two booleans are equal if they're both `true` or both `false`.

### Null

The `Null` message is a singleton representing the Sass `null` value. It should
*not* be represented as the host language's native `null` value, so that it can
expose Sass-specific APIs like the [assertions](#assertions) described above.

`null` is only equal to `null`.

### Calculations

Calculations are represented similarly to their representation in the Sass
specification, as a tree of binary operations and other calculations that
terminates in numbers or strings. This tree structure may be exposed to the host
language, or the host may choose to keep the structure of calculations opaque.

Two calculations are equal if their names are equal and each of their arguments
are equal. Two `Calculation.CalculationOperation`s are equal if they have the
same operator and their left and right values are equal, respectively.

Note that this protocol chooses *not* to require host implementations to
simplify calculations as they're constructed, for the sake of simplicity of
implementation (although hosts *may* do so). This means that a host can
construct calculations like `calc(1 + 1)` which, in Sass, would simplify to 2.
The host is not required to take simplification into account when determining
equality.

### Functions

The protocol allows first-class functions defined in the compiler to be passed
to the host (as `Value.CompilerFunction`s) and vice-versa (as
`Value.HostFunctions)`. It allows the compiler to invoke functions defined in
the host. The host API should hide the distinction between the two function
types as much as possible, but it may refuse to allow host-defined functions to
be invoked on the host, since doing so correctly would require parsing those
functions' signatures.

Two first-class functions are equal if they have the same ID and they're either
both `CompilerFunction`s or both `HostFunction`s.

## Versioning

This protocol is versioned according to [semver 2.0.0]. The current version is
indicated by the `EMBEDDED_PROTOCOL_VERSION` file. If this file has a `-dev`
prerelease string, that indicates that the currently checked in version is in
development, is not considered a release version, and must not be used by
released versions of compilers or hosts. All release versions will also have
GitHub tags for their version numbers of the form `embedded-protocol-x.y.z`.

A "breaking change" is defined as per [the protocol buffer rules for updating a
message type]. Compatibility is considered from the perspective of the host. For
example, if a new `InboundMessage` type is added, that's considered a "backwards
compatible" change because older hosts can simply opt not to use it, even though
from the perspective of the compiler a new message type would be a breaking
change.

[the protocol buffer rules for updating a message type]: https://protobuf.dev/programming-guides/proto3/#updating

Hosts are generally expected to be responsible for installing appropriate
compiler versions as part of their installation process, which should limit the
potential for incompatible versions between the two. For this reason, version
numbers are intended to be primarily an advisory for humans as to the degree of
change over time.

In some cases, the version number will be marked as "pending". This indicates
that the next version of the protocol is still under active development, and may
be waiting for additional pull requests before it's finalized. Hosts and
compilers should never cut releases that target pending protocol versions.

[semver 2.0.0]: https://semver.org/spec/v2.0.0.html
