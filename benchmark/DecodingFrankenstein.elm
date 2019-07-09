module DecodingFrankenstein exposing (main)

import Bytes.Encode as Encode
import Brotli
import Gutenberg.Frankenstein as Frankenstein
import Html
import Browser
data =
                     Frankenstein.bytes
    -- [ 27, 14, 0, 248, 37, 20, 82, 144, 66, 20, 169, 91, 100, 234, 20, 193 ]
                        |> List.map Encode.unsignedInt8
                        |> Encode.sequence
                        |> Encode.encode


main  = 
    Browser.element
    { init = \() -> ((), Cmd.none)
    , update = \_ _ -> ((), Cmd.none)
    , view = \_ ->
            case Brotli.decode data of
                Err e ->
                    Html.text ("failed to decode" ++ e)

                Ok v ->
                        Html.text "decoded succesfully"
    , subscriptions = \_ -> Sub.none       
    }

