uuid
====

This module provides a mechanism for creating UUIDs as defined by
RFC-4122, ITU-T X.667 (09/2004), and ISO/IEC 9834-8:2005. Additional
functions are provided for comparing names against those UUIDs derived
from them, and for splitting UUIDs into their format-dependent
components, etc. A server is used to ensure monotonicity of timestamps
within clock sequence, for variants and versions where time is
relevant.

Each UUID belongs to a variant, distinguished by the most significant
bits in the byte 9 of the UUID (counting from 1). Not every pattern of
128 bits is a valid UUID.

Variant 0
---------

Variant 0, represented by topmost bit of byte 9 having value 0, was
used for NCS UUIDs, originally defined by Apollo Computer. These UUIDs
can be generated, read, and understood by this module, in both
original and new presentation formats, but they are no longer used in
practice.

Historical documentation was found in
[http://bitsavers.trailing-edge.com/bits/Apollo/SR10.4/] and
[https://www.dcerpc.org/trac/browser/dcerpc/uuid/uuid.c]

Many other parts of variant 0 UUIDs were restricted in practice. Byte
9 had a maximum value of 13, so the top bit was always 0, and the
value was in practice almost always either 2 or 13. In addition, bytes
7 and 8 were universally zero, not even expressed in the old
format. Bytes 1-6 represented a time, byte 9 an address family, and
bytes 10-16 a machine address within that family.

Variant 1
---------

Variant 1, represented by the topmost two bits of byte 9 having values
10, is used for this standard in its various editions, and includes 5
versions. The topmost four bits of byte 7 specify the version.

All five versions can be generated, read, and understood by this
module, although they have varying prevalences in practice.

* Version 1 is used for time-based UUIDs. Apart from the bits reserved
  in bytes 9 and 7 for variant and version, bytes 1-8 represented a
  time, bytes 9-10 a clock sequence, and bytes 11-16 a machine
  address.

* Version 2 is used for DCE security UUIDs representing users or
  groups. Mostly identical to version 1, bytes 1-4 were instead used
  to represent a user or group, and byte 10 to specify which.

* Version 3 is used for namespace-based UUIDs using MD5 hashing. Apart
  from the bits reserved in bytes 9 and 7 for variant and version, the
  remaining parts held the relevant hash values determined from a
  name.

UUIDs defined for name spaces include

dns (fqdn)	6ba7b810-9dad-11d1-80b4-00c04fd430c8
url		6ba7b811-9dad-11d1-80b4-00c04fd430c8
oid		6ba7b812-9dad-11d1-80b4-00c04fd430c8
x500 dn (der)	6ba7b814-9dad-11d1-80b4-00c04fd430c8

The standard describes usage of version 3 only in the context of
specified name spaces, although the spaces could perhaps be omitted in
the creation method.

* Version 4 is used for pseudo-random UUIDs. Apart from bits reserved
  in bytes 9 and 7 for variant and version, the remaining bytes are
  random or pseudo-random.

* Version 5 is identical to version 3, except for the use of the SHA-1
  algorithm, rather than MD5.

* Version 0 and versions 6-15 are not yet defined, so UUIDs claiming
  to be variant 1, but one of these versions, are invalid.

Variant 2
---------

Variant 2, represented by the topmost three bits of byte 9 having
values 110, is reserved for Microsoft. Apart from three bits
expressing the variant, there are presumably 125 bits available for
Microsoft to define. The author of this module has no knowledge of any
internal meaning within variant 2 UUIDs. This module does not attempt
to handle variant 2 UUIDs.

Variant 3
---------

Varient 3, represented by the topmost three bits of byte 9 having
values 111, is reserved for future definition. The usage of the
remaining 125 bits is unspecified, resulting in currently invalid
UUIDs. This module does not attempt to handle variant 3 UUIDs.
