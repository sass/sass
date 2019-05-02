## The Embedded Sass Protocol

* [Overview](#overview)
* [RPCs](#rpcs)
* [Error Handling](#error-handling)
* [Host Language APIs](#host-language-apis)
  * [Immutability](#immutability)
  * [Indexing](#indexing)
  * [Assertions](#assertions)
  * [Strings](#strings)
  * [Lists](#lists)

This repository defines a bidirectional protocol for communicating between a
Sass implementation and a host environment. It allows the host environment to
invoke the Sass compiler on source files, and to define custom functions and
importers in the host language.

Disclaimer: this is not an official Google product.

### Overview

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

For streams (like standard input and output) that don't have built-in message
boundaries, every message must begin with a 4-byte (32-bit) unsigned
[little-endian][] integer indicating the length in bytes of the remaining
message. This matches the best practice described in [the protocol buffer
documentation][].

[little-endian]: https://en.wikipedia.org/wiki/Endianness#Little
[the protocol buffer documentation]: https://developers.google.com/protocol-buffers/docs/techniques#streaming

### RPCs

All RPCs are wrapped in an outer message that indicates the RPC's type using [a
oneof field][]. There are two wrapper messages:

[a oneof field]: https://developers.google.com/protocol-buffers/docs/proto3#oneof

* `InboundMessage` is sent from the host to the compiler.
* `OutboundMessage` is sent from the compiler to the host.

The host must only send `InboundMessage`s to the compiler, and the compiler must
only send `OutboundMessage`s to the host.

Each wrapper message contains exactly one RPC. This protocol defines four types
of RPC:

* *Requests* always include a mandatory `uint32 id` field so that the other
  endpoint can respond. All request message types end in `Request`.
* *Responses* include a mandatory `uint32 id` field whose value must be the same
  as their associated request's `id`. All response message types begin with the
  corresponding request name and end with `Response`.
* *Events* may not be responded to and include no `id` field. All event message
  types end with `Event`.
* The `ProtocolError` message, which is sent when one endpoint detects that the
  other is doing something invalid. See [Error Handling][#error-handling] below.

The protocol also defines some messages whose names don't end with `Request`,
`Response`, or `Event`. These are used as structures shared between different
RPCs.

Implementations must guarantee that they use a unique `id` for every request,
although the same `id` may be used for an inbound request and an outbound
request.

All message-typed fields are documented as either "optional" or "mandatory". If
a field is mandatory, the endpoint that sends that message must guarantee that
it's set to a meaningful value, and the endpoint that receives it must reject
the message if it's not set.

Some scalar-typed fields may also be documented as "mandatory", which means that
the endpoint that sends the message must guarantee that its value is meaningful.
However, since the default value may be meaningful in some cases, the endpoint
that receives the message is not required to reject it based on mandatory
scalar-typed fields.

### Error Handling

When one endpoint detects that the other is violating this protocol, it must
send a `ProtocolError` message to the other endpoint. If the error was detected
when processing a request, the `ProtocolError` must have its `id` field set to
the request's id. Otherwise, the `id` field must be set to `-1`.

A `ProtocolError` must be sent whenever any requirements set out by this
protocol (including the documentation in `embedded_sass.proto`) are violated.
This includes, but is not limited to:

* Sending data that can't be parsed as an `InboundMessage` (for the compiler) or
  an `OutboundMessage` (for the host).

* Sending a request with an ID that's in use by another in-flight request.

* Sending a response with an ID that doesn't correspond to an in-flight
  request's ID.

* Sending a message with a `null` value for a mandatory field.

The `ProtocolError` message must *not* be used to report Sass errors or errors
running custom functions or importers.

### Host Language API

Although not strictly part of the protocol, the host language will presumably
provide an API for reading and manipulating SassScript values so that custom
functions can be written in the host language. In order to ensure that custom
functions will behave consistently with built-in Sass functions, the host
language should provide APIs that meet the following guidelines.

The [Dart `Value` API][] is a good example of an object-oriented API that
follows these guidelines.

[Dart `Value` API]: https://pub.dartlang.org/documentation/sass/latest/sass/Value-class.html

#### Immutability

All SassScript values are immutable, and the API should preserve that fact. No
API calls should be able to modify any SassScript values, including collections
like lists and maps. Instead, API calls should be provided to return new values
with adjusted contents or to copy values into mutable host-language objects.

If API calls are provided that return a new versions of an object with adjusted
contents, metadata for the returned object (such as the type of list separator
or a number's units) should match that of the original object.

#### Indexing

SassScript values use index 1 to refer to the first element and -1 to refer to
the final element. The index 0 is invalid. Furthermore, indexes in Sass strings
refer to [Unicode code points][], not bytes or UTF-16 code units. The API should
provide a means to convert between Sass's indexing scheme and the host
language's indexing scheme, and should encourage authors to treat any indexes
they're passed as Sass-style indexes rather than host-language-style indexes.

[Unicode code points]: https://en.wikipedia.org/wiki/Code_point

#### Assertions

The API should provide an easy means to assert that values are the expected type
and to produce a useful error if they aren't. They should *not* provide a means
to assert that a value is a list, though, since all Sass values should be
treated as lists (see below).

#### Strings

API users should be encouraged to return quoted strings unless there's a
particular reason not to.

#### Numbers

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

#### Lists

In Sass, every value counts as a list. Maps count as unbracketed comma-separated
lists of two-element unbracketed space-separated key-value lists, and all other
non-list values count as lists that contain that value. The API should make it
easy to treat every value as a list, and should discourage treating values
passed as `Value.List`s specially.

API users should be encouraged to return unbracketed comma-separated lists
unless there's a particular reason not to.
