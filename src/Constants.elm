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
    {-
       str
           |> Encode.string
           |> Encode.encode
           -- |> (\buffer -> Decode.decode (Array.Helpers.decodeArray (Bytes.width buffer) Decode.unsignedInt8) buffer)
           |> (\buffer -> Decode.decode (Array.Helpers.decodeByteArray (Bytes.width buffer)) buffer)
           |> Maybe.withDefault Array.empty
    -}
    String.foldl (\c -> Array.push (Char.toCode c)) Array.empty str


skipFlipAlt =
    Array.fromList [ 1747, 1, 360, 3, 48, 1, 97, 3, 51, 1, 179, 1, 43, 1, 67, 1, 130, 2, 367, 1, 449, 2, 26, 2, 6, 2, 3, 2, 58, 2, 100, 340, 3098, 2, 393, 2, 366, 2, 5, 2, 58, 2, 1, 2, 3, 2, 94, 2, 44, 2, 13, 2, 141, 2, 15, 2, 57, 2, 73, 2, 81, 2, 33, 2, 80, 2, 31, 2, 171, 2, 50, 2, 50, 2, 11, 2, 26, 2, 18, 2, 3922, 5976, 75, 2, 76, 2, 28, 2, 33, 2, 41, 2, 44, 2, 84, 2, 28, 2, 34, 2, 65, 2, 168, 2, 19, 2, 22, 2, 4, 2, 32, 2, 12, 2, 31, 2, 5, 2, 10, 2, 34, 2, 9, 2, 13, 2, 4, 2, 40, 2, 34, 2, 13, 570, 6, 966, 8655, 2, 4942, 2, 39, 2, 23, 2, 5, 2, 33, 2, 36, 2, 44, 2, 12, 2, 27, 2, 21, 2, 50, 2, 93, 2, 9, 2, 82, 2, 61, 2, 8, 2, 33, 2, 5, 2, 27, 2, 25, 2, 3, 2, 3, 2, 30, 2, 3338, 2, 1247, 2, 754, 6, 2, 6, 20, 2, 1, 2, 1, 2, 2, 2, 1, 8, 5, 2, 118, 2, 26, 2, 98, 2, 19, 2, 57, 2, 34, 2, 14, 2, 26, 2, 38, 2, 18, 2, 74, 2, 14, 2, 1, 2, 27, 2, 106, 2, 14, 2, 18, 2, 38, 2, 67, 2, 9, 2, 12, 2, 8, 2, 6, 2, 38, 2, 6, 2, 43, 2, 5, 2, 18, 2, 4, 2, 24, 2, 30, 2, 42, 2, 10, 2, 44, 2, 28, 2, 14, 2, 10, 2, 51, 2, 41, 2, 1, 1304, 96, 4, 8, 4, 24, 2, 8, 2, 950, 2, 6307, 2, 9, 2, 8, 4, 1, 2, 4, 2, 1, 2, 4, 279, 12, 2, 52, 2, 32, 2, 93, 2, 70, 2, 3, 2, 38, 2, 4, 2, 10, 2, 30, 2, 15, 2, 54, 2, 46, 2, 68, 2, 15, 2, 33, 2, 33, 2, 24, 162, 9, 828, 7887, 2, 1, 20, 27, 2, 28, 2, 8, 2, 54, 2, 28, 2, 12, 2, 38, 2, 8, 2, 58, 2, 84, 2, 59, 2, 18, 2, 31, 2, 18, 2, 31, 1800, 10753, 2, 66, 2, 9, 2, 9, 2, 9, 2, 9, 2, 8, 2, 38, 2, 14, 2, 20, 2, 86, 2, 20, 2, 31, 2, 53, 2, 20, 2, 9, 2, 64, 2, 7764, 168, 9, 2, 19, 2, 13, 2, 34, 2, 19, 2, 80, 2, 51, 2, 19, 2, 37, 2, 10, 2, 10, 2, 58, 2, 25, 3960, 6575, 2, 20, 4, 26, 2, 11, 2, 11, 2, 1879, 3, 5, 3, 4161, 3, 5, 3, 1, 28, 11, 2, 12, 2, 1, 1050, 2460, 6, 2, 6, 28, 2, 31, 1305, 1456, 592, 1479, 16, 3507, 1782, 1197, 18, 3424, 2, 11, 340, 735, 54, 2, 6, 1, 546, 987, 2, 272, 2, 13, 132, 1168, 336 ]


{-| A more compact encoding of the skipFlip data, that uses the upper bits 14..28 for the skip and 0..14 for the flip
This means we can do an Array.foldl instead of repeated Array.get
-}
skipFlipAlt2 =
    Array.fromList [ 28622849, 5898243, 786433, 1589251, 835585, 2932737, 704513, 1097729, 2129922, 6012929, 7356418, 425986, 98306, 49154, 950274, 1638740, 50757634, 6438914, 5996546, 81922, 950274, 16386, 49154, 1540098, 720898, 212994, 2310146, 245762, 933890, 1196034, 1327106, 540674, 1310722, 507906, 2801666, 819202, 819202, 180226, 425986, 294914, 64264024, 1228802, 1245186, 458754, 540674, 671746, 720898, 1376258, 458754, 557058, 1064962, 2752514, 311298, 360450, 65538, 524290, 196610, 507906, 81922, 163842, 557058, 147458, 212994, 65538, 655362, 557058, 213562, 99270, 141803522, 80969730, 638978, 376834, 81922, 540674, 589826, 720898, 196610, 442370, 344066, 819202, 1523714, 147458, 1343490, 999426, 131074, 540674, 81922, 442370, 409602, 49154, 49154, 491522, 54689794, 20430850, 12353542, 32774, 327682, 16386, 16386, 32770, 16392, 81922, 1933314, 425986, 1605634, 311298, 933890, 557058, 229378, 425986, 622594, 294914, 1212418, 229378, 16386, 442370, 1736706, 229378, 294914, 622594, 1097730, 147458, 196610, 131074, 98306, 622594, 98306, 704514, 81922, 294914, 65538, 393218, 491522, 688130, 163842, 720898, 458754, 229378, 163842, 835586, 671746, 17688, 1572868, 131076, 393218, 131074, 15564802, 103333890, 147458, 131076, 16386, 65538, 16386, 65815, 196610, 851970, 524290, 1523714, 1146882, 49154, 622594, 65538, 163842, 491522, 245762, 884738, 753666, 1114114, 245762, 540674, 540674, 393378, 148284, 129220610, 16404, 442370, 458754, 131074, 884738, 458754, 196610, 622594, 131074, 950274, 1376258, 966658, 294914, 507906, 294914, 509704, 176177154, 1081346, 147458, 147458, 147458, 147458, 131074, 622594, 229378, 327682, 1409026, 327682, 507906, 868354, 327682, 147458, 1048578, 127205544, 147458, 311298, 212994, 557058, 311298, 1310722, 835586, 311298, 606210, 163842, 163842, 950274, 413560, 107724802, 327684, 425986, 180226, 180226, 30785539, 81923, 68173827, 81923, 16412, 180226, 196610, 17434, 40304646, 32774, 458754, 509209, 23855696, 24231952, 57460470, 19611666, 56098818, 180564, 12042294, 32774, 16930, 16171010, 4456450, 213124, 19136848 ]


unpackDictionaryData data0 data1 =
    let
        dict =
            toUsAsciiBytes (data0 ++ data1)

        folder e ( offset, accum ) =
            let
                -- upper 14 bits
                skip =
                    Bitwise.shiftRightBy 14 e

                -- lower 14 bits
                flip =
                    -- 2 ^ 14 - 1
                    Bitwise.and 0x3FFF e
            in
            innerLoop 0 flip (offset + skip) accum

        innerLoop j flip offset accum =
            if j < flip then
                innerLoop (j + 1) flip (offset + 1) (Array.Helpers.update offset (\v -> Bitwise.or v 0x80) accum)

            else
                ( offset, accum )
    in
    Array.foldl folder ( 0, dict ) skipFlipAlt2
        |> Tuple.second


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
