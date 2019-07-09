module Brotli exposing (decode)

{-| A [brotli](https://github.com/google/brotli) decoder in elm!

Brotli is a compression algorithm much like the one zip archives use, but better: it is faster, more compact, and specifically geared towards web content and English text.
This package decodes `Bytes` sequences that are compressed with the brotli algorithm:

import Bytes exposing (Bytes)
import Bytes.Encode as Encode
import Bytes.Decode as Decode
import Brotli

bytes : List Int
bytes = 
    [ 27, 14, 0, 248, 37, 20, 82, 144, 66, 20, 169, 91, 100, 234, 20, 193 ]

buffer : Bytes
buffer = 
    bytes 
        |> List.map Encode.unsignedInt8 
        |> Encode.sequence
        |> Encode.encode

result = 
    Brotli.decode buffer 
        |> Result.toMaybe
        |> Maybe.andThen (\v -> Decode.decode (Decode.string (Bytes.width v)) v)
        --> Just "this is a test\n"            

## Decode

@docs decode

-}

import Bytes exposing (Bytes)
import Internal


{-| Decode a brotli-encoded bytes sequence

If you ever run into an error, that probably means you've hit a bug. If you can, please open an issue with the input that caused the issue.

-}
decode : Bytes -> Result String Bytes
decode buffer =
    case Internal.decode buffer of 
        Err e -> 
            Err ( renderError e) 
        Ok v -> 
            Ok v

renderError : Internal.Error -> String
renderError error = 
    case  error of
        Internal.MaxDistanceTooSmall { actual, minimal } -> "maxDistance is too small: it should be at least" ++ String.fromInt minimal ++ ", but it is only " ++ String.fromInt actual
        Internal.StateNotInitialized-> "State not initialized"
        Internal.ReadAfterEnd-> "Read after end"
        Internal.UnusedByteAfterEnd-> "Unused byte after end"
        Internal.ExuberantNibble-> "Exuberant nibble"
        Internal.ExuberantByte-> "Exuberant byte"
        Internal.CorruptedReservedBit-> "Corrupted reserved bit"
        Internal.CorruptedHuffmanHistogram-> "Corrupted Huffman histogram"
        Internal.CustomError message -> message
        Internal.UnusedSpace-> "Unused space"
        Internal.NoHuffmanCode symbol -> "There is no huffman code for symbol " ++ String.fromInt symbol ++ ", it is outside of the alphabet!"
        Internal.CorruptedContextMap-> "Corrupted ContextMap"
        Internal.InvalidWindowBits-> "Invalid window bits"
        Internal.InvalidMetablockLength-> "invalid MetaBlock length"
        Internal.InvalidBackwardReference message -> message
        Internal.UnalignedCopyBytes-> "Unaligned copy bytes"
        Internal.CorruptedPaddingBits-> "Corrupted padding bits"

