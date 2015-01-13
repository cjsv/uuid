uuid
====

This Erlang module provides a mechanism for creating UUIDs as defined
by RFC-4122, ITU-T X.667 (09/2004), and ISO/IEC 9834-8:2005.
Additional functions are provided for comparing names against those
UUIDs derived from them, and for splitting UUIDs into their
format-dependent components, etc. A server is used to ensure
monotonicity of timestamps within clock sequence, for variants and
versions where time is relevant.

Within your program, a UUID may be represented either as a string
(such as "f81d4fae-7dec-11d0-a765-00a0c91e6bf6") or as an integer.

Binaries used by functions in this module are used for bit-level
operations on the integer form. Binaries containing a string formatted
UUID, such as you may get from decoding JSON, are likely to cause
confusion and/or errors.

This version of the Erlang module has been updated to account for the
deprecation of older crypto module functions. An additional function
is provided to determine the age of a time-based UUID in seconds.
The API is defined by the export list in uuid.erl.

Each UUID belongs to a variant, distinguished by the most significant
bits in the octet 9 of the UUID (counting from 1). Not every pattern
of 128 bits is a valid UUID.

In this document, octets are numbered left-to-right from 1 to 16; bits
are not numbered.

This module is Copyright (c) 2012-2015 by Christopher Vance; portions
are Copyright (c) 2012-2013 by Into Science Pty Ltd.

Variant 0
---------

Variant 0, represented by most significant bit of octet 9 having value
0, was used for NCS UUIDs, originally defined by Apollo
Computer. These UUIDs can be generated, read, and understood by this
Erlang module, in both original and new presentation formats, but they
are no longer used in practice.

Historical documentation was found at
[http://bitsavers.trailing-edge.com/bits/Apollo/SR10.4/] and
[https://www.dcerpc.org/trac/browser/dcerpc/uuid/uuid.c].

Many other parts of variant 0 UUIDs were restricted in practice. Octet
9 had a maximum value of 13, so the most significant bit was always 0,
and the value was in practice almost always either 2 or 13. In
addition, octets 7 and 8 were universally zero, not even expressed in
the old format. Octets 1-6 represented a time, octet 9 an address
family, and octets 10-16 a machine address within that family.

Variant 1
---------

Variant 1, represented by the most significant two bits of octet 9
having values 10, is used for this standard in its various editions,
and includes 5 versions. The most significant four bits of octet 7
specify the version.

All five versions can be generated, read, and understood by this
Erlang module, although they have varying prevalences in practice.

* Version 1 is used for time-based UUIDs. Apart from the bits reserved
  in octets 9 and 7 for variant and version, octets 1-8 represented a
  time, octets 9-10 a clock sequence, and octets 11-16 a machine
  address.

* Version 2 is used for DCE security UUIDs representing users or
  groups. Mostly identical to version 1, octets 1-4 were instead used
  to represent a user or group, and octet 10 to specify which.

* Version 3 is used for namespace-based UUIDs using MD5 hashing. Apart
  from the bits reserved in octets 9 and 7 for variant and version,
  the remaining parts held the relevant hash values determined from a
  name.

  The standard describes usage of version 3 only in the context of
  specified name spaces, although name spaces could perhaps be omitted
  in the creation method.

* Version 4 is used for pseudo-random UUIDs. Apart from bits reserved
  in octets 9 and 7 for variant and version, the remaining octets are
  pseudo-random.

* Version 5 is identical to version 3, except for the use of the SHA-1
  algorithm, rather than MD5. Trailing bits of the hash are discarded
  to keep only the leading 128 bits.

* Version 0 and versions 6-15 are not yet defined, so UUIDs claiming
  to be variant 1, but one of these versions, are invalid.

UUIDs defined for name spaces include

* DNS (FQDN)		6ba7b810-9dad-11d1-80b4-00c04fd430c8
* URL			6ba7b811-9dad-11d1-80b4-00c04fd430c8
* OID			6ba7b812-9dad-11d1-80b4-00c04fd430c8
* X.500 DN (DER)	6ba7b814-9dad-11d1-80b4-00c04fd430c8

For namespace use, a UUID can also be expressed as
urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6
2.25.329800735698586629295641978511506172918 (OID).

Variant 2
---------

Variant 2, represented by the most significant three bits of octet 9
having values 110, is reserved for Microsoft. Apart from three bits
expressing the variant, there are presumably 125 bits available for
Microsoft to define. The author of this Erlang module has no knowledge
of any internal meaning within variant 2 UUIDs. This module does not
attempt to handle variant 2 UUIDs.

Variant 3
---------

Variant 3, represented by the most significant three bits of octet 9
having values 111, is reserved for future definition. The usage of the
remaining 125 bits is unspecified, resulting in currently invalid
UUIDs. This Erlang module does not attempt to handle variant 3 UUIDs.
