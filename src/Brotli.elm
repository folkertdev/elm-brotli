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
    |> Maybe.andThen (\\v -> Decode.decode (Decode.string (Bytes.width v)) v)
    --> Just "this is a test\\n"


## Decode

@docs decode

-}

import Bytes exposing (Bytes)
import Internal
import State


{-| Decode a brotli-encoded bytes sequence

If you ever run into an error, that probably means you've hit a bug. If you can, please open an issue with the input that caused the issue.

-}
decode : Bytes -> Result String Bytes
decode buffer =
    case Internal.decode buffer of
        Err e ->
            Err (renderError e)

        Ok v ->
            Ok v


renderError : State.Error -> String
renderError error =
    case error of
        State.MaxDistanceTooSmall { actual, minimal } ->
            "maxDistance is too small: it should be at least" ++ String.fromInt minimal ++ ", but it is only " ++ String.fromInt actual

        State.StateNotInitialized ->
            "State not initialized"

        State.ReadAfterEnd ->
            "Read after end"

        State.UnusedByteAfterEnd ->
            "Unused byte after end"

        State.ExuberantNibble ->
            "Exuberant nibble"

        State.ExuberantByte ->
            "Exuberant byte"

        State.CorruptedReservedBit ->
            "Corrupted reserved bit"

        State.CorruptedHuffmanHistogram ->
            "Corrupted Huffman histogram"

        State.CustomError message ->
            message

        State.UnusedSpace ->
            "Unused space"

        State.NoHuffmanCode symbol ->
            "There is no huffman code for symbol " ++ String.fromInt symbol ++ ", it is outside of the alphabet!"

        State.CorruptedContextMap ->
            "Corrupted ContextMap"

        State.InvalidWindowBits ->
            "Invalid window bits"

        State.InvalidMetablockLength ->
            "Invalid MetaBlock length"

        State.InvalidBackwardReference message ->
            message

        State.UnalignedCopyBytes ->
            "Unaligned copy bytes"

        State.CorruptedPaddingBits ->
            "Corrupted padding bits"
