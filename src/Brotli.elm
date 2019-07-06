module Brotli exposing (decode)

{-| A [brotli](https://github.com/google/brotli) decoder in elm!

Brotli is a compression algorithm much like the one zip archives use, but better: it is faster, more compact, and specifically geared towards web content and English text.
This package decodes `Bytes` sequences that are compressed with the brotli algorithm:

    import Bytes exposing Bytes
    import Bytes.Encode as Encode
    import Brotli

    bytes : List String
    bytes =
        [ 27, 14, 0, 248, 37, 20, 82, 144, 66, 20, 169, 91, 100, 234, 20, 193 ]

    buffer : Bytes
    buffer =
        bytes
            |> List.map Encode.unsignedInt8
            |> Encode.sequence
            |> Encode.encode


    Brotli.decode buffer
        --> Ok "this is a test\n"


## Decode

@docs decode

-}

import Bytes exposing (Bytes)
import Internal


{-| Decode a brotli-encoded bytes sequence

If you ever run into an error, that probably means you've hit a bug. If you can, please open an issue with the input that caused the issue.

-}
decode : Bytes -> Result String Bytes
decode =
    Internal.decode
