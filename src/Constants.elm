module Constants exposing (block_length_n_bits, block_length_offset, calculateOffsets, charCodeAt, cmd_lookup, code_length_code_order, copy_length_n_bits, dictionary_data, dictionary_offsets_by_length, dictionary_size_bits_by_length, distance_short_code_index_offset, distance_short_code_value_offset, fixed_table, insert_length_n_bits, lookup, max_huffman_table_size, max_length, outputLength, skipFlipAlt, toUsAsciiBytes, unpackCommandLookupTable, unpackDictionaryData, unpackLookupTable)

import Array exposing (Array)
import Array.Helpers
import Bitwise
import Bytes
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import DictionaryData


outputLength : Int
outputLength =
    16384


max_length : Int
max_length =
    15


code_length_code_order : Array Int
code_length_code_order =
    Array.fromList [ 1, 2, 3, 4, 0, 5, 17, 6, 16, 7, 8, 9, 10, 11, 12, 13, 14, 15 ]


fixed_table : Array Int
fixed_table =
    Array.fromList [ 0x00020000, 0x00020004, 0x00020003, 0x00030002, 0x00020000, 0x00020004, 0x00020003, 0x00040001, 0x00020000, 0x00020004, 0x00020003, 0x00030002, 0x00020000, 0x00020004, 0x00020003, 0x00040005 ]


dictionary_offsets_by_length =
    Array.fromList [ 0, 0, 0, 0, 0, 4096, 9216, 21504, 35840, 44032, 53248, 63488, 74752, 87040, 93696, 100864, 104704, 106752, 108928, 113536, 115968, 118528, 119872, 121280, 122016 ]


dictionary_data =
    unpackDictionaryData DictionaryData.data0 DictionaryData.data1


dictionary_size_bits_by_length =
    Array.fromList [ 0, 0, 0, 0, 10, 10, 11, 11, 10, 10, 10, 10, 10, 9, 9, 8, 7, 7, 8, 7, 7, 6, 6, 5, 5 ]


distance_short_code_index_offset =
    Array.fromList [ 0, 3, 2, 1, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3 ]


distance_short_code_value_offset =
    Array.fromList [ 0, 0, 0, 0, -1, 1, -2, 2, -3, 3, -1, 1, -2, 2, -3, 3 ]


block_length_offset =
    Array.fromList [ 1, 5, 9, 13, 17, 25, 33, 41, 49, 65, 81, 97, 113, 145, 177, 209, 241, 305, 369, 497, 753, 1265, 2289, 4337, 8433, 16625 ]


block_length_n_bits =
    Array.fromList [ 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 7, 8, 9, 10, 11, 12, 13, 24 ]


max_huffman_table_size =
    Array.fromList [ 256, 402, 436, 468, 500, 534, 566, 598, 630, 662, 694, 726, 758, 790, 822, 854, 886, 920, 952, 984, 1016, 1048, 1080 ]


insert_length_n_bits =
    Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x02, 0x02, 0x03, 0x03, 0x04, 0x04, 0x05, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0C, 0x0E, 0x18 ]


copy_length_n_bits =
    Array.fromList [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x02, 0x02, 0x03, 0x03, 0x04, 0x04, 0x05, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x18 ]


lookup =
    unpackLookupTable (Array.repeat 2048 0)


toUsAsciiBytes : String -> Array Int
toUsAsciiBytes str =
    -- @optimize this is inefficient! is a String.map Char.toCode possible?
    str
        |> Encode.string
        |> Encode.encode
        |> (\buffer -> Decode.decode (Array.Helpers.decodeArray (Bytes.width buffer) Decode.unsignedInt8) buffer)
        |> Maybe.withDefault Array.empty


skipFlipAlt =
    Array.fromList [ 1783, 37, 396, 39, 84, 37, 133, 39, 87, 37, 215, 37, 79, 37, 103, 37, 166, 38, 403, 37, 485, 38, 62, 38, 42, 38, 39, 38, 94, 38, 136, 376, 3134, 38, 429, 38, 402, 38, 41, 38, 94, 38, 37, 38, 39, 38, 130, 38, 80, 38, 49, 38, 177, 38, 51, 38, 93, 38, 109, 38, 117, 38, 69, 38, 116, 38, 67, 38, 207, 38, 86, 38, 86, 38, 47, 38, 62, 38, 54, 38, 3958, 6012, 111, 38, 112, 38, 64, 38, 69, 38, 77, 38, 80, 38, 120, 38, 64, 38, 70, 38, 101, 38, 204, 38, 55, 38, 58, 38, 40, 38, 68, 38, 48, 38, 67, 38, 41, 38, 46, 38, 70, 38, 45, 38, 49, 38, 40, 38, 76, 38, 70, 38, 49, 606, 42, 1002, 8691, 38, 4978, 38, 75, 38, 59, 38, 41, 38, 69, 38, 72, 38, 80, 38, 48, 38, 63, 38, 57, 38, 86, 38, 129, 38, 45, 38, 118, 38, 97, 38, 44, 38, 69, 38, 41, 38, 63, 38, 61, 38, 39, 38, 39, 38, 66, 38, 3374, 38, 1283, 38, 790, 42, 38, 42, 56, 38, 37, 38, 37, 38, 38, 38, 37, 44, 41, 38, 154, 38, 62, 38, 134, 38, 55, 38, 93, 38, 70, 38, 50, 38, 62, 38, 74, 38, 54, 38, 110, 38, 50, 38, 37, 38, 63, 38, 142, 38, 50, 38, 54, 38, 74, 38, 103, 38, 45, 38, 48, 38, 44, 38, 42, 38, 74, 38, 42, 38, 79, 38, 41, 38, 54, 38, 40, 38, 60, 38, 66, 38, 78, 38, 46, 38, 80, 38, 64, 38, 50, 38, 46, 38, 87, 38, 77, 38, 37, 1340, 132, 40, 44, 40, 60, 38, 44, 38, 986, 38, 6343, 38, 45, 38, 44, 40, 37, 38, 40, 38, 37, 38, 40, 315, 48, 38, 88, 38, 68, 38, 129, 38, 106, 38, 39, 38, 74, 38, 40, 38, 46, 38, 66, 38, 51, 38, 90, 38, 82, 38, 104, 38, 51, 38, 69, 38, 69, 38, 60, 198, 45, 864, 7923, 38, 37, 56, 63, 38, 64, 38, 44, 38, 90, 38, 64, 38, 48, 38, 74, 38, 44, 38, 94, 38, 120, 38, 95, 38, 54, 38, 67, 38, 54, 38, 67, 1836, 10789, 38, 102, 38, 45, 38, 45, 38, 45, 38, 45, 38, 44, 38, 74, 38, 50, 38, 56, 38, 122, 38, 56, 38, 67, 38, 89, 38, 56, 38, 45, 38, 100, 38, 7800, 204, 45, 38, 55, 38, 49, 38, 70, 38, 55, 38, 116, 38, 87, 38, 55, 38, 73, 38, 46, 38, 46, 38, 94, 38, 61, 3996, 6611, 38, 56, 40, 62, 38, 47, 38, 47, 38, 1915, 39, 41, 39, 4197, 39, 41, 39, 37, 64, 47, 38, 48, 38, 37, 1086, 2496, 42, 38, 42, 64, 38, 67, 1341, 1492, 628, 1515, 52, 3543, 1818, 1233, 54, 3460, 38, 47, 376, 771, 90, 38, 42, 37, 582, 1023, 38, 308, 38, 49, 168, 1204, 372 ]


unpackDictionaryData data0 data1 =
    let
        dict =
            toUsAsciiBytes (data0 ++ data1)

        n =
            -- String.length skipFlip
            Array.length skipFlipAlt

        go i offset accum =
            if i < n then
                let
                    skip =
                        (Array.get i skipFlipAlt |> Maybe.withDefault 0) - 36

                    flip =
                        Maybe.withDefault 0 (Array.get (i + 1) skipFlipAlt) - 36

                    ( newOffset, newAccum ) =
                        innerLoop 0 flip (offset + skip) accum
                in
                go (i + 2) newOffset newAccum

            else
                accum

        innerLoop j flip offset accum =
            if j < flip then
                innerLoop (j + 1) flip (offset + 1) (Array.Helpers.update offset (\v -> Bitwise.or v 0x80) accum)

            else
                ( offset, accum )
    in
    go 0 0 dict


unpackLookupTable =
    let
        characterMap =
            "         !!  !                  \"#$##%#$&'##(#)#++++++++++((&*'##,---,---,-----,-----,-----&#'###.///.///./////./////./////&#'# "

        rle =
            "A/*  ':  & : $  \u{0081} @"

        loop1 i arr =
            if i < 256 then
                loop1 (i + 1)
                    (arr
                        |> Array.set i (Bitwise.and i 0x3F)
                        |> Array.set (512 + i) (Bitwise.shiftRightBy 2 i)
                        |> Array.set (1792 + i) (2 + Bitwise.shiftRightBy 6 i)
                    )

            else
                arr

        loop2 i arr =
            if i < 128 then
                case charCodeAt i characterMap of
                    Nothing ->
                        arr

                    Just value ->
                        loop2 (i + 1) (Array.set (i + 1024) (4 * (value - 32)) arr)

            else
                arr

        loop3 i arr =
            if i < 64 then
                loop3 (i + 1)
                    (arr
                        |> Array.set (1152 + i) (Bitwise.and i 1)
                        |> Array.set (1216 + i) (Bitwise.and i 1 + 2)
                    )

            else
                arr

        loop4 k offset arr =
            if k < 19 then
                let
                    value =
                        Bitwise.and k 3

                    rep =
                        (charCodeAt k rle |> Maybe.withDefault 0) - 32

                    go i currentOffset currentLookup =
                        if i < rep then
                            go (i + 1) (currentOffset + 1) (Array.set currentOffset value currentLookup)

                        else
                            ( currentOffset, currentLookup )

                    ( newOffset, newLookup ) =
                        go 0 offset arr
                in
                loop4 (k + 1) newOffset newLookup

            else
                arr

        loop5 i arr =
            if i < 16 then
                loop5 (i + 1)
                    (arr
                        |> Array.set (1792 + i) 1
                        |> Array.set (2032 + i) 6
                    )

            else
                arr

        loop6 i arr =
            if i < 256 then
                let
                    value =
                        Bitwise.shiftLeftBy 3 (Array.Helpers.unsafeGet (1792 + i) arr)
                in
                loop6 (i + 1) (Array.set (1536 + i) value arr)

            else
                arr
    in
    loop1 0
        >> loop2 0
        >> loop3 0
        >> loop4 0 1280
        >> loop5 0
        >> (\arr ->
                arr
                    |> Array.set 1792 0
                    |> Array.set 2047 7
           )
        >> loop6 0


charCodeAt : Int -> String -> Maybe Int
charCodeAt n str =
    case String.uncons (String.dropLeft n str) of
        Nothing ->
            Nothing

        Just ( c, _ ) ->
            Just (Char.toCode c)


cmd_lookup =
    -- new Int16Array(2816);
    unpackCommandLookupTable (Array.repeat 2816 0)


unpackCommandLookupTable arr =
    let
        ( insertLengthOffsets, copyLengthOffsets ) =
            calculateOffsets

        loop cmdCode cmdLookup =
            if cmdCode < 704 then
                let
                    ( rangeIdx, distanceContextOffset ) =
                        if Bitwise.shiftRightZfBy 6 cmdCode >= 2 then
                            ( Bitwise.shiftRightZfBy 6 cmdCode - 2
                            , 0
                            )

                        else
                            ( Bitwise.shiftRightZfBy 6 cmdCode, -4 )

                    insertCode =
                        Bitwise.or
                            (Bitwise.shiftLeftBy 3 (Bitwise.and (Bitwise.shiftRightZfBy (rangeIdx * 2) 0x00029850) 0x03))
                            (Bitwise.and (Bitwise.shiftRightZfBy 3 cmdCode) 7)

                    copyCode =
                        Bitwise.or (Bitwise.shiftLeftBy 3 (Bitwise.and (Bitwise.shiftRightZfBy (rangeIdx * 2) 0x00026244) 0x03)) (Bitwise.and cmdCode 7)

                    copyLengthOffset =
                        Array.Helpers.unsafeGet copyCode copyLengthOffsets

                    distanceContext =
                        distanceContextOffset
                            + (if copyLengthOffset > 4 then
                                3

                               else
                                copyLengthOffset - 2
                              )

                    index =
                        cmdCode * 4
                in
                loop (cmdCode + 1)
                    (cmdLookup
                        |> Array.set (index + 0) (Bitwise.or (Array.Helpers.unsafeGet insertCode insert_length_n_bits) (Bitwise.shiftLeftBy 8 (Array.Helpers.unsafeGet copyCode copy_length_n_bits)))
                        |> Array.set (index + 1) (Array.Helpers.unsafeGet insertCode insertLengthOffsets)
                        |> Array.set (index + 2) (Array.Helpers.unsafeGet copyCode copyLengthOffsets)
                        |> Array.set (index + 3) distanceContext
                    )

            else
                cmdLookup
    in
    loop 0 arr


calculateOffsets =
    let
        initialInsertLengthOffsets =
            Array.repeat 24 0

        initialCopyLengthOffsets =
            Array.repeat 24 0
                |> Array.set 0 2

        go i insert copy =
            if i < 23 then
                go
                    (i + 1)
                    (Array.set (i + 1) (Array.Helpers.unsafeGet i insert + Bitwise.shiftLeftBy (Array.Helpers.unsafeGet i insert_length_n_bits) 1) insert)
                    (Array.set (i + 1) (Array.Helpers.unsafeGet i copy + Bitwise.shiftLeftBy (Array.Helpers.unsafeGet i copy_length_n_bits) 1) copy)

            else
                ( insert, copy )
    in
    go 0 initialInsertLengthOffsets initialCopyLengthOffsets
