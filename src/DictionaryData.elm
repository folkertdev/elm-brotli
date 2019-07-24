module DictionaryData exposing (DictionaryData, dictionary, get, sliceFoldl)

import Array exposing (Array)
import Bitwise
import DictionaryDataRaw


type DictionaryData
    = DictionaryData (Array Int)


get : Int -> DictionaryData -> Int
get index (DictionaryData data) =
    case Array.get (index // 4) data of
        Nothing ->
            0

        Just int32 ->
            case index |> modBy 4 of
                0 ->
                    Bitwise.and 0xFF (Bitwise.shiftRightBy 24 int32)

                1 ->
                    Bitwise.and 0xFF (Bitwise.shiftRightBy 16 int32)

                2 ->
                    Bitwise.and 0xFF (Bitwise.shiftRightBy 8 int32)

                _ ->
                    Bitwise.and 0xFF int32


sliceFoldl : Int -> Int -> (Int -> b -> b) -> b -> DictionaryData -> b
sliceFoldl start end folder default array =
    sliceFoldLManualLoop start end folder default array


sliceFoldLManualLoop : Int -> Int -> (Int -> b -> b) -> b -> DictionaryData -> b
sliceFoldLManualLoop i n folder accum ((DictionaryData array) as data) =
    if (i |> modBy 4) == 0 && i + 4 < n then
        case Array.get (i // 4) array of
            Nothing ->
                accum

            Just int32 ->
                let
                    b1 =
                        Bitwise.and 0xFF (Bitwise.shiftRightBy 24 int32)

                    b2 =
                        Bitwise.and 0xFF (Bitwise.shiftRightBy 16 int32)

                    b3 =
                        Bitwise.and 0xFF (Bitwise.shiftRightBy 8 int32)

                    b4 =
                        Bitwise.and 0xFF int32

                    new =
                        accum
                            |> folder b1
                            |> folder b2
                            |> folder b3
                            |> folder b4
                in
                sliceFoldLManualLoop (i + 4) n folder new data

    else if i < n then
        -- sliceFoldLManualLoop (i + 1) n folder (folder (Array.get i array |> Maybe.withDefault 0) accum) data
        sliceFoldLManualLoop (i + 1) n folder (folder (get i data) accum) data

    else
        accum


dictionary : DictionaryData
dictionary =
    DictionaryData (Array.fromList DictionaryDataRaw.raw)
