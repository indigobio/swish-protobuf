# Protocol Buffers for Swish

This repository provides a [Swish](https://github.com/becls/swish)
implementation of Google's [Protocol
Buffers](https://developers.google.com/protocol-buffers/).

#### Message definitions

`(define-message type (field-name field-type field-number) ...)`

The following field types are supported:
- Integer: `int32`, `int64`, `uint32`, `uint64`, `sint32`, `sint64`, `fixed32`, `fixed64`, `sfixed32`, and `sfixed64`
- Enumeration: `(enum enum-type)`
- Floating-point: `float` and `double`
- Boolean: `bool`
- String: `string`
- Bytevector: `bytes`
- Message: `(message message-type)`
- List of any of the above: `(repeated type)`
- Map: `(map key-type value-type)`

Each message definition produces a corresponding tuple
definition:

`(define-tuple type (field-name ...))`

#### Enumeration definitions

`(define-enum type (name value) ...)`

Each enumeration definition produces a macro that converts the member
name to its int32 value. Each enum field stores the int32 value, not a
symbol. Use `(type name)` to express the int32 value of member _name_ in
enumeration _type_. We don't use symbols because there is not a
guaranteed one-to-one mapping from values to symbols.


#### Limitations

- Message and enumeration definitions are automatically exported. As a result, the `define-message` and `define-enum` forms must be used within a library or module. The message definitions use `define-property` to associate the procedures that read, size, merge, and write them with the name of the message.
- One-of fields are treated like regular fields.
- Groups and extensions are not supported.
- The writer does not honor the `[packed=false]` option, but the reader supports both packed and unpacked repeated scalar numeric fields.
- Definitions are not validated.
- The reader does not eliminate duplicate map keys.

#### Representation

- Integer fields are represented by exact integers (fixnums and
  bignums). The default value is 0.
- Enum fields are represented by exact integers. The default value is 0.
- Floating-point fields are represented by flonums. The default value is 0.0.
- Boolean fields are represented by booleans. The default value is #f.
- String fields are represented by strings with the UTF-8 encoding. The default value is the empty string.
- Bytes fields are represented by bytevectors. The default value is the empty bytevector.
- Message fields are represented by a tuple or #f for missing. The default value is #f.
- Repeated fields are represented by a list of the underlying type. The default value is the empty list.
- Map fields types are represented by a list of pairs. For each pair, the _car_ is the key, and the _cdr_ is the value. The default value is the empty list.

#### Reader

`(read-message type ip [limit])`

This macro reads a message of _type_ from binary input port or
bytevector _ip_. When _limit_ is present, it specifies the maximum
number of bytes to read.

#### Writer

`(write-message type e)`

This macro writes a message _e_ of _type_ to a bytevector.

`(write-message type e op)`

This macros writes a message _e_ of _type_ to binary output port _op_.

`(write-message type e op size)`

This macro calls procedure _size_ with the number of bytes required to
write message _e_ of _type_ and then writes message _e_ to binary
output port _op_.

Because the protobuf format uses length prefixes for nested fields,
the size of a nested object must be known before it can be
streamed. The simple approach of buffering the nested object results
in quadratic allocation for deeply nested messages. To eliminate this
inefficiency, the writer first traverses the message and caches in an
eq-hash-table the sizes of nested messages, packed scalars, and pairs
in maps. It can then generate the output without any intermediate
buffers.

## Scheme Protocol Buffer Compiler Plugin

[scheme_generator.cc](https://github.com/burgerrg/swish-protobuf/blob/master/scheme_generator.cc)
is the source code for the proto-gen-scheme protoc plugin. It
generates S-expression definitions of messages and enumerations from
protobuf text definitions.

### Build proto-gen-scheme

A C++11 compiler and the cmake and protobuf packages are required.  On
macOS, install the cmake and protobuf Homebrew formulas. On Ubuntu,
install the cmake, g++, and libprotoc-dev packages.

1. `cmake .`
2. `make`

### Use proto-gen-scheme

<pre>
protoc --plugin=<i>path-to-proto-gen-scheme</i> --scheme_out=<i>OUT_DIR</i> <i>PROTO_FILES</i>
</pre>
