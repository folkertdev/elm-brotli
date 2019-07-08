# Elm Brotli

A [brotli](https://github.com/google/brotli) decoder in elm!

Brotli is a compression algorithm much like the one zip archives use, but better: it is faster, more compact, and specifically geared towards web content and English text.
This package decodes `Bytes` sequences that are compressed with the brotli algorithm: 

```elm
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
        --> "this is a test\n"            
```

## Technical details 

This is an awful format to implement, especially when porting c-style javascript code to a functional language like elm.
Therefore: 

* the implementation is slow 
* there are probably still bugs (likely with 1-character fixes)

If you run into these issues with a specific input, let me know.

