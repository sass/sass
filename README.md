## The Embedded Sass Protocol

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

* *Requests* always include a `uint32 id` field so that the other endpoint can
  respond. All request message types end in `Request`.
* *Responses* include the same `uint32 id` field as their associated request.
  All response message types begin with the corresponding request name and end
  with `Response`.
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
