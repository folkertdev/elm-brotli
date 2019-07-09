module Internal exposing (array, buildHuffmanTable, calculateDistanceAlphabetSize, calculateDistanceLut, calculateOffsets, charCodeAt, cmd_lookup, decode, decompress, dictionary_data, encodeByteArray, generateCount, generateOffsets, inverseMoveToFrontTransform, lookup, moveToFront, nextTableBitSize, phase1, readComplexHuffmanCodeHelp, readFewBits, replicateValue, sortSymbols, topUpAccumulator)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Step(..))
import Bytes.Encode as Encode
import DictionaryData
import Transforms


log =
    \_ -> identity


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
    str
        |> Encode.string
        |> Encode.encode
        |> (\buffer -> Decode.decode (array (Bytes.width buffer) Decode.unsignedInt8) buffer)
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
                innerLoop (j + 1) flip (offset + 1) (update offset (\v -> Bitwise.or v 0x80) accum)

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
                        Bitwise.shiftLeftBy 3 (unsafeGet (1792 + i) arr)
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
                    (Array.set (i + 1) (unsafeGet i insert + Bitwise.shiftLeftBy (unsafeGet i insert_length_n_bits) 1) insert)
                    (Array.set (i + 1) (unsafeGet i copy + Bitwise.shiftLeftBy (unsafeGet i copy_length_n_bits) 1) copy)

            else
                ( insert, copy )
    in
    go 0 initialInsertLengthOffsets initialCopyLengthOffsets


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
                        unsafeGet copyCode copyLengthOffsets

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
                        |> Array.set (index + 0) (Bitwise.or (unsafeGet insertCode insert_length_n_bits) (Bitwise.shiftLeftBy 8 (unsafeGet copyCode copy_length_n_bits)))
                        |> Array.set (index + 1) (unsafeGet insertCode insertLengthOffsets)
                        |> Array.set (index + 2) (unsafeGet copyCode copyLengthOffsets)
                        |> Array.set (index + 3) distanceContext
                    )

            else
                cmdLookup
    in
    loop 0 arr


type alias Error =
    String


type alias Decoder a =
    State -> Result Error ( State, a )


type alias State =
    { ringBuffer : Array Int
    , contextModes : Array Int
    , contextMap : Array Int
    , distContextMap : Array Int
    , distExtraBits : Array Int
    , output : Array Int
    , byteBuffer : Array Int
    , shortBuffer : Array Int
    , intBuffer : Array Int
    , rings : Array Int
    , blockTrees : Array Int
    , literalTreeGroup : Array Int
    , commandTreeGroup : Array Int
    , distanceTreeGroup : Array Int
    , distOffset : Array Int
    , runningState : Int
    , nextRunningState : Int
    , accumulator32 : Int
    , bitOffset : Int
    , halfOffset : Int
    , tailBytes : Int
    , endOfStreamReached : Bool
    , metaBlockLength : Int
    , inputEnd : Bool
    , isUncompressed : Bool
    , isMetadata : Bool
    , literalBlockLength : Int
    , numLiteralBlockTypes : Int
    , commandBlockLength : Int
    , numCommandBlockTypes : Int
    , distanceBlockLength : Int
    , numDistanceBlockTypes : Int
    , pos : Int
    , maxDistance : Int
    , distRbIdx : Int
    , trivialLiteralContext : Int
    , literalTreeIdx : Int
    , commandTreeIdx : Int
    , j : Int
    , insertLength : Int
    , contextMapSlice : Int
    , distContextMapSlice : Int
    , contextLookupOffset1 : Int
    , contextLookupOffset2 : Int
    , distanceCode : Int
    , numDirectDistanceCodes : Int
    , distancePostfixMask : Int
    , distancePostfixBits : Int
    , distance : Int
    , copyLength : Int
    , maxBackwardDistance : Int
    , maxRingBufferSize : Int
    , ringBufferSize : Int
    , expectedTotalSize : Int
    , isEager : Bool
    , isLargeWindow : Bool
    , input : InputStream
    , written : { fromRingBuffer : Int, toOutput : Int }
    }


defaultState : Bytes -> State
defaultState buffer =
    { ringBuffer = Array.empty
    , contextModes = Array.empty
    , contextMap = Array.empty
    , distContextMap = Array.empty
    , distExtraBits = Array.empty
    , output = Array.empty
    , byteBuffer = Array.empty
    , shortBuffer = Array.empty
    , intBuffer = Array.empty
    , rings = Array.fromList [ 16, 15, 11, 4, 0, 0, 0, 0, 0, 0 ]
    , blockTrees = Array.empty
    , literalTreeGroup = Array.empty
    , commandTreeGroup = Array.empty
    , distanceTreeGroup = Array.empty
    , distOffset = Array.empty
    , runningState = 0
    , nextRunningState = 0
    , accumulator32 = 0
    , bitOffset = 0
    , halfOffset = 0
    , tailBytes = 0
    , endOfStreamReached = False
    , metaBlockLength = 0
    , inputEnd = False
    , isUncompressed = False
    , isMetadata = False
    , literalBlockLength = 0
    , numLiteralBlockTypes = 0
    , commandBlockLength = 0
    , numCommandBlockTypes = 0
    , distanceBlockLength = 0
    , numDistanceBlockTypes = 0
    , pos = 0
    , maxDistance = 0
    , distRbIdx = 0
    , trivialLiteralContext = 0
    , literalTreeIdx = 0
    , commandTreeIdx = 0
    , j = 0
    , insertLength = 0
    , contextMapSlice = 0
    , distContextMapSlice = 0
    , contextLookupOffset1 = 0
    , contextLookupOffset2 = 0
    , distanceCode = 0
    , numDirectDistanceCodes = 0
    , distancePostfixMask = 0
    , distancePostfixBits = 0
    , distance = 0
    , copyLength = 0
    , maxBackwardDistance = 0
    , maxRingBufferSize = 0
    , ringBufferSize = 0
    , expectedTotalSize = 0
    , isEager = False
    , isLargeWindow = False
    , input = { offset = 0, buffer = buffer }
    , written = { fromRingBuffer = 0, toOutput = 0 }
    }


initState : State -> Result Error State
initState s =
    if s.runningState /= 0 then
        Err "State MUST be uninitialized"

    else
        calculateDistanceAlphabetLimit 0x7FFFFFFC 3 (Bitwise.shiftLeftBy 3 15)
            |> Result.map
                (\maxDistanceAlphabetLimit ->
                    { s
                        | blockTrees = Array.append (Array.fromList [ 7 ]) (Array.repeat 3090 0)
                        , distRbIdx = 3
                        , distExtraBits = Array.repeat maxDistanceAlphabetLimit 0
                        , distOffset = Array.repeat maxDistanceAlphabetLimit 0
                    }
                )
            |> Result.andThen initBitReader
            |> Result.map (updateRunningState 1)


calculateDistanceAlphabetSize : Int -> Int -> Int -> Int
calculateDistanceAlphabetSize npostfix ndirect maxndistbits =
    16 + ndirect + 2 * Bitwise.shiftLeftBy npostfix maxndistbits


calculateDistanceAlphabetLimit maxDistance npostfix ndirect =
    if maxDistance < ndirect + Bitwise.shiftLeftBy npostfix 2 then
        Err "maxDistance is too small"

    else
        let
            offset =
                Bitwise.shiftRightBy npostfix (maxDistance - ndirect) + 4

            ndistbits =
                log2floor offset - 1

            group =
                Bitwise.or (Bitwise.shiftLeftBy 1 (ndistbits - 1)) (Bitwise.and (Bitwise.shiftRightBy ndistbits offset) 1)
        in
        Ok (Bitwise.shiftLeftBy npostfix (group - 1) + Bitwise.shiftLeftBy npostfix 1 + ndirect + 16)


log2floor v =
    let
        go i step result =
            if step > 0 then
                if Bitwise.shiftRightZfBy step i /= 0 then
                    go (Bitwise.shiftRightZfBy step i) (Bitwise.shiftRightBy 1 step) (result + step)

                else
                    go i (Bitwise.shiftRightBy 1 step) result

            else
                result + i
    in
    go v 16 -1


initBitReader : State -> Result Error State
initBitReader s =
    { s
        | byteBuffer = Array.repeat 4160 0
        , accumulator32 = 0
        , shortBuffer = Array.repeat 2080 0
        , bitOffset = 32
        , halfOffset = 2048
        , endOfStreamReached = False
    }
        |> prepare


prepare : State -> Result Error State
prepare unverified =
    let
        s =
            if unverified.halfOffset > 2030 then
                doReadMoreInput unverified

            else
                Ok unverified
    in
    s
        |> Result.andThen (checkHealth False)
        |> Result.map putOnAccumulator
        |> Result.map putOnAccumulator


reload s =
    if s.bitOffset == 32 then
        prepare s

    else
        Ok s


checkHealth : Bool -> State -> Result Error State
checkHealth endOfStream s =
    if not s.endOfStreamReached then
        Ok s

    else
        let
            byteOffset =
                Bitwise.shiftLeftBy 1 s.halfOffset + Bitwise.shiftRightBy 3 (s.bitOffset + 7) - 4
        in
        if byteOffset > s.tailBytes then
            Err "Read after end"

        else if endOfStream && (byteOffset /= s.tailBytes) then
            Err "Unused bytes after end"

        else
            Ok s


topUpAccumulator : State -> State
topUpAccumulator s =
    if s.bitOffset >= 16 then
        putOnAccumulator s

    else
        s


putOnAccumulator : State -> State
putOnAccumulator s =
    let
        next =
            Array.get s.halfOffset s.shortBuffer
                |> Maybe.withDefault 0
                |> Bitwise.shiftLeftBy 16
    in
    updateAccumulator (s.bitOffset - 16) (s.halfOffset + 1) (Bitwise.or next (Bitwise.shiftRightZfBy 16 s.accumulator32)) s


type alias InputStream =
    { offset : Int
    , buffer : Bytes
    }


map : (a -> b) -> Decoder a -> Decoder b
map f g =
    \s ->
        case g s of
            Err e ->
                Err e

            Ok ( s2, v ) ->
                Ok ( s2, f v )


succeed : a -> Decoder a
succeed v =
    \s -> Ok ( s, v )


fail : Error -> Decoder a
fail e =
    \s -> Err e


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f x =
    \s ->
        case x s of
            Err e ->
                Err e

            Ok ( s2, v ) ->
                f v s2


readNextMetablockHeader : State -> Result Error State
readNextMetablockHeader s =
    if s.inputEnd then
        Ok
            { s
                | nextRunningState = 10
                , runningState = 13
            }

    else
        let
            s2 =
                { s
                    | literalTreeGroup = Array.empty
                    , commandTreeGroup = Array.empty
                    , distanceTreeGroup = Array.empty
                }
        in
        maybeReadMoreInput 2030 s2
            |> Result.andThen decodeMetaBlockLength
            |> Result.andThen
                (\s4 ->
                    if s4.metaBlockLength == 0 && s4.isMetadata == False then
                        Ok s4

                    else
                        (if s4.isUncompressed || s4.isMetadata then
                            case jumpToByteBoundary s4 of
                                Err e ->
                                    Err e

                                Ok s5 ->
                                    Ok
                                        (updateRunningState
                                            (if s5.isMetadata then
                                                5

                                             else
                                                6
                                            )
                                            s5
                                        )

                         else
                            Ok (updateRunningState 3 s4)
                        )
                            |> Result.andThen
                                (\s6 ->
                                    if s6.isMetadata then
                                        Ok s6

                                    else
                                        let
                                            temp =
                                                s6.expectedTotalSize + s6.metaBlockLength

                                            s7 =
                                                { s6
                                                    | expectedTotalSize =
                                                        if temp > Bitwise.shiftLeftBy 30 1 then
                                                            Bitwise.shiftLeftBy 30 1

                                                        else
                                                            temp
                                                }
                                        in
                                        if s7.ringBufferSize < s7.maxRingBufferSize then
                                            maybeReallocateRingBuffer s7

                                        else
                                            Ok s7
                                )
                )


maybeReallocateRingBuffer : State -> Result Error State
maybeReallocateRingBuffer s =
    let
        newSize =
            let
                initialSize =
                    s.maxRingBufferSize
            in
            if initialSize > s.expectedTotalSize then
                let
                    minimalNewSize =
                        s.expectedTotalSize

                    calculate1 size =
                        if Bitwise.shiftRightBy 1 size > minimalNewSize then
                            calculate1 (Bitwise.shiftRightBy 1 size)

                        else
                            size

                    calculate2 size =
                        if s.inputEnd == False && size < 16384 && s.maxRingBufferSize >= 16384 then
                            16384

                        else
                            size
                in
                initialSize |> calculate1 |> calculate2

            else
                initialSize
    in
    if newSize < s.ringBufferSize then
        Ok s

    else
        let
            ringBufferSizeWithSlack =
                newSize + 37

            newBuffer =
                if Array.length s.ringBuffer == 0 then
                    Array.repeat ringBufferSizeWithSlack 0

                else
                    Array.append (Array.slice 0 s.ringBufferSize s.ringBuffer) (Array.repeat (ringBufferSizeWithSlack - s.ringBufferSize) 0)
        in
        Ok { s | ringBuffer = newBuffer, ringBufferSize = newSize }


decodeMetaBlockBytes : State -> Result Error State
decodeMetaBlockBytes s_ =
    let
        byteLoop i sizeBytes state_ =
            if i < sizeBytes then
                case readFewBitsSafe 8 state_ of
                    ( s2, bits ) ->
                        if bits == 0 && i + 1 == sizeBytes && sizeBytes > 1 then
                            Err "Exuberant byte"

                        else
                            -- TODO remove this record update
                            byteLoop (i + 1) sizeBytes { s2 | metaBlockLength = Bitwise.or s2.metaBlockLength (Bitwise.shiftLeftBy (i * 8) bits) }

            else
                Ok state_

        s =
            { s_ | isMetadata = True }
    in
    case readFewBits 1 s of
        ( s1, reserved ) ->
            if reserved /= 0 then
                Err "Corrupted reserved bit"

            else
                case readFewBits 2 s1 of
                    ( s2, sizeBytes ) ->
                        if sizeBytes == 0 then
                            Ok s2

                        else
                            byteLoop 0 sizeBytes s2


decodeMetaBlockNibbles : Int -> State -> Result Error State
decodeMetaBlockNibbles numberOfNibbles s =
    let
        nibbleLoop i sizeNibbles state_ =
            if i < sizeNibbles then
                case readFewBitsSafe 4 state_ of
                    ( s2, bits ) ->
                        if bits == 0 && i + 1 == sizeNibbles && sizeNibbles > 4 then
                            Err (log "error" "Exuberant nibble")

                        else
                            -- TODO remove record update
                            nibbleLoop (i + 1) sizeNibbles { s2 | metaBlockLength = Bitwise.or s2.metaBlockLength (Bitwise.shiftLeftBy (i * 4) bits) }

            else
                Ok state_
    in
    nibbleLoop 0 numberOfNibbles s


decodeMetaBlockLength : State -> Result Error State
decodeMetaBlockLength s_ =
    case readFewBitsSafe 1 s_ of
        ( s3, inputEnd ) ->
            let
                s =
                    { s3
                        | inputEnd =
                            if inputEnd == 0 then
                                False

                            else
                                True
                        , metaBlockLength = 0
                        , isUncompressed = False
                        , isMetadata = False
                    }

                continue s0 =
                    case readFewBits 2 s0 of
                        ( s5, sizeNibbles_ ) ->
                            let
                                sizeNibbles =
                                    sizeNibbles_ + 4

                                step =
                                    if sizeNibbles == 7 then
                                        decodeMetaBlockBytes s5

                                    else
                                        decodeMetaBlockNibbles sizeNibbles s5
                            in
                            case step of
                                Err e ->
                                    Err e

                                Ok s6 ->
                                    if s6.inputEnd == False then
                                        case readFewBits 1 s6 of
                                            ( s8, uncompressed ) ->
                                                Ok { s8 | isUncompressed = uncompressed == 1, metaBlockLength = s6.metaBlockLength + 1 }

                                    else
                                        Ok { s6 | metaBlockLength = s6.metaBlockLength + 1 }
            in
            if s.inputEnd then
                case readFewBits 1 s of
                    ( s1, bit ) ->
                        if bit /= 0 then
                            Ok s1

                        else
                            continue s1

            else
                continue s


decodeWindowBits : State -> ( State, Int )
decodeWindowBits initial =
    let
        largeWindowEnabled =
            initial.isLargeWindow

        newState =
            let
                ( s1, v ) =
                    readFewBitsSafe 1 initial

                s2 =
                    { s1 | isLargeWindow = False }
            in
            if v == 0 then
                ( s2, 16 )

            else
                let
                    ( s3_, firstN ) =
                        readFewBits 3 s2
                in
                if firstN /= 0 then
                    ( s3_, 17 + firstN )

                else
                    let
                        ( s3, n ) =
                            readFewBits 3 s3_
                    in
                    if n /= 0 then
                        if n == 1 then
                            if largeWindowEnabled == False then
                                ( s3, -1 )

                            else
                                let
                                    s4 =
                                        { s3 | isLargeWindow = True }

                                    ( s5, w ) =
                                        readFewBits 1 s4
                                in
                                if w == 1 then
                                    ( s5, -1 )

                                else
                                    let
                                        ( s6, m ) =
                                            readFewBits 6 s5
                                    in
                                    if m < 10 || m > 30 then
                                        ( s6, -1 )

                                    else
                                        ( s6, m )

                        else
                            ( s3, 8 + n )

                    else
                        ( s3, 17 )
    in
    newState


decodeVarLenUnsignedByte : State -> ( State, Int )
decodeVarLenUnsignedByte s_ =
    case readFewBitsSafe 1 s_ of
        ( s, 0 ) ->
            ( s, 0 )

        ( s, _ ) ->
            case readFewBits 3 s of
                ( s2, n ) ->
                    if n == 0 then
                        ( s2, 1 )

                    else
                        case readFewBits n s2 of
                            ( s3, v ) ->
                                ( s3, v + Bitwise.shiftLeftBy n 1 )


readMetablockPartition : Int -> Int -> Decoder Int
readMetablockPartition treeType numBlockTypes s =
    let
        offset =
            s.blockTrees |> Array.get (2 * treeType) |> Maybe.withDefault 0
    in
    if numBlockTypes <= 1 then
        Ok
            ( { s | blockTrees = s.blockTrees |> Array.set (2 * treeType + 1) offset |> Array.set (2 * treeType + 2) offset }
            , Bitwise.shiftLeftBy 28 1
            )

    else
        let
            blockTypeAlphabetSize =
                numBlockTypes + 2
        in
        case readHuffmanCode blockTypeAlphabetSize blockTypeAlphabetSize s.blockTrees (2 * treeType) s of
            Err e ->
                Err e

            Ok ( s2, r1 ) ->
                let
                    offset2 =
                        offset + r1.total_size

                    s3 =
                        { s2 | blockTrees = Array.set (2 * treeType + 1) offset2 r1.tableGroup }

                    blockLengthAlphabetSize =
                        26
                in
                case readHuffmanCode blockLengthAlphabetSize blockLengthAlphabetSize s3.blockTrees (2 * treeType + 1) s3 of
                    Err e ->
                        Err e

                    Ok ( s4, r2 ) ->
                        let
                            offset3 =
                                offset2 + r2.total_size

                            s5 =
                                { s4 | blockTrees = Array.set (2 * treeType + 2) offset3 r2.tableGroup }
                        in
                        readBlockLength s5.blockTrees (2 * treeType + 1) s5
                            |> Ok


readSymbol : Array Int -> Int -> State -> ( State, Int )
readSymbol tableGroup tableIdx s =
    let
        readTableGroup index =
            unsafeGet index tableGroup

        offset =
            readTableGroup tableIdx + Bitwise.and val 0xFF

        val =
            Bitwise.shiftRightZfBy s.bitOffset s.accumulator32

        bits =
            Bitwise.shiftRightBy 16 (readTableGroup offset)

        sym =
            Bitwise.and (readTableGroup offset) 0xFFFF
    in
    if bits <= 8 then
        ( updateBitOffset (s.bitOffset + bits) s, sym )

    else
        let
            offset2 =
                offset + sym

            mask =
                Bitwise.shiftLeftBy bits 1 - 1

            offset3 =
                offset2 + Bitwise.shiftRightZfBy 8 (Bitwise.and val mask)

            newBitOffset =
                s.bitOffset + 8 + Bitwise.shiftRightBy 16 (readTableGroup offset3)

            result =
                Bitwise.and (readTableGroup offset3) 0xFFFF
        in
        ( updateBitOffset newBitOffset s, result )


readHuffmanCode : Int -> Int -> Array Int -> Int -> Decoder { tableGroup : Array Int, total_size : Int }
readHuffmanCode alphabetSizeMax alphabetSizeLimit tableGroup tableIdx s_ =
    let
        checkEnoughRead =
            maybeReadMoreInput 2030 s_
    in
    case checkEnoughRead of
        Err e ->
            Err e

        Ok s ->
            case readFewBitsSafe 2 s of
                ( s2, 1 ) ->
                    readSimpleHuffmanCode alphabetSizeMax alphabetSizeLimit tableGroup tableIdx s2

                ( s2, skip ) ->
                    readComplexHuffmanCode alphabetSizeLimit skip tableGroup tableIdx s2


code_length_code_order : Array Int
code_length_code_order =
    Array.fromList [ 1, 2, 3, 4, 0, 5, 17, 6, 16, 7, 8, 9, 10, 11, 12, 13, 14, 15 ]


fixed_table : Array Int
fixed_table =
    Array.fromList [ 0x00020000, 0x00020004, 0x00020003, 0x00030002, 0x00020000, 0x00020004, 0x00020003, 0x00040001, 0x00020000, 0x00020004, 0x00020003, 0x00030002, 0x00020000, 0x00020004, 0x00020003, 0x00040005 ]


readComplexHuffmanCodeHelp :
    Int
    -> Array Int
    -> Int
    -> Int
    -> State
    -> ( State, ( Int, Int, Array Int ) )
readComplexHuffmanCodeHelp i codeLengthCodeLengths space numCodes s =
    if i < 18 && space > 0 then
        let
            codeLenIdx =
                unsafeGet i code_length_code_order

            s1 =
                topUpAccumulator s

            p =
                Bitwise.shiftRightZfBy s1.bitOffset s1.accumulator32
                    |> Bitwise.and 15

            s2 =
                updateBitOffset (s1.bitOffset + Bitwise.shiftRightBy 16 (unsafeGet p fixed_table)) s1

            v =
                unsafeGet p fixed_table
                    |> Bitwise.and 0xFFFF
        in
        if v /= 0 then
            readComplexHuffmanCodeHelp (i + 1) (Array.set codeLenIdx v codeLengthCodeLengths) (space - Bitwise.shiftRightBy v 32) (numCodes + 1) s2

        else
            readComplexHuffmanCodeHelp (i + 1) (Array.set codeLenIdx v codeLengthCodeLengths) space numCodes s2

    else
        ( s, ( space, numCodes, codeLengthCodeLengths ) )


readComplexHuffmanCode : Int -> Int -> Array Int -> Int -> State -> Result Error ( State, { tableGroup : Array Int, total_size : Int } )
readComplexHuffmanCode alphabetSizeLimit skip tableGroup tableIdx state =
    let
        codeLengths =
            Array.repeat alphabetSizeLimit 0

        codeLengthCodeLengths =
            Array.repeat 18 0
    in
    state
        |> readComplexHuffmanCodeHelp skip codeLengthCodeLengths 32 0
        |> (\( s, ( space, numCodes, newCodelengthCodelengths ) ) ->
                if space /= 0 && numCodes /= 1 then
                    Err ("Corrupted Huffman code histogram: space =" ++ String.fromInt space ++ " (should be 0), and numCodes=" ++ String.fromInt numCodes ++ " (should be 1)!")

                else
                    case readHuffmanCodeLengths newCodelengthCodelengths alphabetSizeLimit codeLengths s of
                        Err e ->
                            Err e

                        Ok ( s1, newCodeLengths ) ->
                            Ok ( s1, buildHuffmanTable tableGroup tableIdx 8 newCodeLengths )
           )


unsafeGet : Int -> Array Int -> Int
unsafeGet i arr =
    case Array.get i arr of
        Nothing ->
            0

        Just v ->
            v


readHuffmanCodeLengths : Array Int -> Int -> Array Int -> State -> Result Error ( State, Array Int )
readHuffmanCodeLengths codeLengthCodeLengths numSymbols initialCodeLengths state =
    let
        -- TODO could use Array.push instead of Array.set, as elements are created order
        go :
            Array Int
            -> Int
            -> Int
            -> Int
            -> Int
            -> Int
            -> Array Int
            -> State
            -> Result Error ( State, ( Array Int, Int, Int ) )
        go table symbol space repeat repeatCodeLen prevCodeLen codeLengths s =
            if symbol < numSymbols && space > 0 then
                case maybeReadMoreInput 2030 s |> Result.map topUpAccumulator of
                    Err e ->
                        Err e

                    Ok s1 ->
                        let
                            p =
                                Bitwise.and (Bitwise.shiftRightZfBy s1.bitOffset s1.accumulator32) 31

                            s2 =
                                updateBitOffset (s1.bitOffset + Bitwise.shiftRightBy 16 (unsafeGet p table)) s1

                            codeLen =
                                unsafeGet p table
                                    |> Bitwise.and 0xFFFF
                        in
                        if codeLen < 16 then
                            let
                                newCodeLengths =
                                    Array.set symbol codeLen codeLengths
                            in
                            if codeLen /= 0 then
                                go table (symbol + 1) (space - Bitwise.shiftRightBy codeLen 32768) 0 repeatCodeLen codeLen newCodeLengths s2

                            else
                                go table (symbol + 1) space 0 repeatCodeLen prevCodeLen newCodeLengths s2

                        else
                            let
                                extraBits =
                                    codeLen - 14

                                newLen =
                                    if codeLen == 16 then
                                        prevCodeLen

                                    else
                                        0

                                ( repeat1, repeatCodeLen1 ) =
                                    if repeatCodeLen /= newLen then
                                        ( 0, newLen )

                                    else
                                        ( repeat, repeatCodeLen )

                                oldRepeat =
                                    repeat1

                                repeat2 =
                                    if repeat1 > 0 then
                                        Bitwise.shiftLeftBy extraBits (repeat1 - 2)

                                    else
                                        repeat1

                                s3 =
                                    topUpAccumulator s2
                            in
                            case readFewBits extraBits s3 of
                                ( s4, v ) ->
                                    let
                                        repeat3 =
                                            repeat2 + v + 3

                                        repeatDelta =
                                            repeat3 - oldRepeat
                                    in
                                    if symbol + repeatDelta > numSymbols then
                                        Err "symbol + repeatDelta > numSymbols"

                                    else
                                        let
                                            go2 i currentCodeLengths currentSymbol =
                                                if i < repeatDelta then
                                                    go2 (i + 1) (Array.set currentSymbol repeatCodeLen1 currentCodeLengths) (currentSymbol + 1)

                                                else
                                                    ( currentCodeLengths, currentSymbol )
                                        in
                                        case go2 0 codeLengths symbol of
                                            ( newCodeLengths, newSymbol ) ->
                                                if repeatCodeLen1 /= 0 then
                                                    go table newSymbol (space - Bitwise.shiftLeftBy (15 - repeatCodeLen1) repeatDelta) repeat3 repeatCodeLen1 prevCodeLen newCodeLengths s4

                                                else
                                                    go table newSymbol space repeat3 repeatCodeLen1 prevCodeLen newCodeLengths s4

            else
                Ok ( s, ( codeLengths, space, symbol ) )
    in
    let
        symbol =
            0

        prevCodeLen =
            8

        repeat =
            0

        repeatCodeLen =
            0

        space =
            32768

        initialTable =
            Array.repeat (32 + 1) 0

        tableIdx =
            Array.length initialTable - 1

        -- length is 18 hardcoded, might be a problem ?
        -- case buildHuffmanTable initialTable tableIdx 5 codeLengthCodeLengths 18 of
        table =
            buildHuffmanTable initialTable tableIdx 5 codeLengthCodeLengths
    in
    case go table.tableGroup symbol space repeat repeatCodeLen prevCodeLen initialCodeLengths state of
        Err e ->
            Err e

        Ok ( s1, ( codeLengths, newSpace, newSymbol ) ) ->
            if newSpace /= 0 then
                Err ("unused space! the space is " ++ String.fromInt newSpace ++ ", but the symbol is " ++ String.fromInt newSymbol)

            else
                Ok ( s1, fill 0 newSymbol numSymbols codeLengths )


readSimpleHuffmanCode : Int -> Int -> Array Int -> Int -> State -> Result Error ( State, { tableGroup : Array Int, total_size : Int } )
readSimpleHuffmanCode alphabetSizeMax alphabetSizeLimit tableGroup tableIdx s0 =
    let
        maxBits =
            1 + log2floor (alphabetSizeMax - 1)

        go i numSymbols s_ acc =
            if i < numSymbols then
                case readFewBitsSafe maxBits s_ of
                    ( newS, symbol ) ->
                        if symbol >= alphabetSizeLimit then
                            Err "can't readHuffmanCode"

                        else
                            go (i + 1) numSymbols newS (Array.push symbol acc)

            else
                Ok ( s_, acc )

        readHistogramId numSymbols s =
            if numSymbols == 4 then
                case readFewBits 1 s of
                    ( newS, extra ) ->
                        ( newS, extra + numSymbols )

            else
                ( s, numSymbols )
    in
    case readFewBits 2 s0 of
        ( s1, n ) ->
            let
                numSymbols =
                    n + 1
            in
            case go 0 numSymbols s1 Array.empty of
                Err e ->
                    Err e

                Ok ( s2, symbols0 ) ->
                    case checkDupes symbols0 of
                        Err e ->
                            Err e

                        Ok symbols ->
                            case readHistogramId numSymbols s2 of
                                ( s3, histogramId ) ->
                                    let
                                        readSymbols index =
                                            Array.get index symbols |> Maybe.withDefault 0

                                        codeLengths =
                                            case histogramId of
                                                1 ->
                                                    Array.repeat alphabetSizeLimit 0
                                                        |> Array.set (readSymbols 0) 1

                                                2 ->
                                                    Array.repeat alphabetSizeLimit 0
                                                        |> Array.set (readSymbols 0) 1
                                                        |> Array.set (readSymbols 1) 1

                                                3 ->
                                                    Array.repeat alphabetSizeLimit 0
                                                        |> Array.set (readSymbols 0) 1
                                                        |> Array.set (readSymbols 1) 2
                                                        |> Array.set (readSymbols 2) 2

                                                4 ->
                                                    Array.repeat alphabetSizeLimit 0
                                                        |> Array.set (readSymbols 0) 2
                                                        |> Array.set (readSymbols 1) 2
                                                        |> Array.set (readSymbols 2) 2
                                                        |> Array.set (readSymbols 3) 2

                                                5 ->
                                                    Array.repeat alphabetSizeLimit 0
                                                        |> Array.set (readSymbols 0) 1
                                                        |> Array.set (readSymbols 1) 2
                                                        |> Array.set (readSymbols 2) 3
                                                        |> Array.set (readSymbols 3) 3

                                                _ ->
                                                    Array.repeat alphabetSizeLimit 0
                                    in
                                    Ok ( s3, buildHuffmanTable tableGroup tableIdx 8 codeLengths )


{-| Returns reverse(reverse(key, len) + 1, len), where reverse(key, len) is the
bit-wise reversal of the len least significant bits of key.
-}
getNextKey : Int -> Int -> Int
getNextKey key len =
    let
        go step =
            if Bitwise.and key step /= 0 then
                go (Bitwise.shiftRightBy 1 step)

            else
                step

        initialStep =
            Bitwise.shiftLeftBy (len - 1) 1

        finalStep =
            go initialStep
    in
    Bitwise.and key (finalStep - 1) + finalStep


update : Int -> (a -> a) -> Array a -> Array a
update index f arr =
    case Array.get index arr of
        Nothing ->
            arr

        Just v ->
            Array.set index (f v) arr


max_length : Int
max_length =
    15


generateCount : Array Int -> Int -> Array Int
generateCount codeLengths codeLengthsSize =
    let
        go symbol acc =
            if symbol < codeLengthsSize then
                case Array.get symbol codeLengths of
                    Nothing ->
                        -- just abort; this should never happen
                        acc

                    Just idx ->
                        go (symbol + 1) (update idx (\x -> x + 1) acc)

            else
                acc
    in
    go 0 (Array.repeat 16 0)


generateOffsets : Array Int -> Array Int
generateOffsets count =
    let
        go len offset =
            if len < max_length then
                let
                    newValue =
                        unsafeGet len offset + unsafeGet len count
                in
                go (len + 1) (Array.push newValue offset)

            else
                offset
    in
    go 1 (Array.repeat 2 0)


sortSymbols : { codeLengthsSize : Int, codeLengths : Array Int, offset : Array Int } -> { sorted : Array Int, offset : Array Int }
sortSymbols init =
    let
        codeLengthsSize =
            init.codeLengthsSize

        codeLengths =
            init.codeLengths

        go symbol sorted offset =
            if symbol < codeLengthsSize then
                case unsafeGet symbol codeLengths of
                    0 ->
                        go (symbol + 1) sorted offset

                    idx ->
                        let
                            idx2 =
                                unsafeGet idx offset
                        in
                        go (symbol + 1) (Array.set idx2 symbol sorted) (update idx (\x -> x + 1) offset)

            else
                { offset = offset, sorted = sorted }
    in
    go 0 (Array.repeat init.codeLengthsSize 0) init.offset


phase1 rootBits tableOffset tableSize sorted =
    let
        loop1 currentCount len key symbol step currentTableGroup =
            if len <= rootBits then
                case loop2 currentCount len key symbol step (unsafeGet len currentCount) currentTableGroup of
                    ( ( newCount, newKey ), newSymbol, newTableGroup ) ->
                        loop1 newCount (len + 1) newKey newSymbol (Bitwise.shiftLeftBy 1 step) newTableGroup

            else
                ( ( currentCount, key ), symbol, currentTableGroup )

        loop2 currentCount len key symbol step iterationsRemaining currentTableGroup =
            if unsafeGet len currentCount > 0 then
                let
                    newTableGroup =
                        replicateValue currentTableGroup (tableOffset + key) step tableSize (Bitwise.or (Bitwise.shiftLeftBy 16 len) (unsafeGet symbol sorted))

                    newSymbol =
                        symbol + 1

                    newKey =
                        getNextKey key len
                in
                loop2 (update len (\v -> v - 1) currentCount) len newKey newSymbol step (iterationsRemaining - 1) newTableGroup

            else
                ( ( currentCount, key ), symbol, currentTableGroup )
    in
    loop1


buildHuffmanTable : Array Int -> Int -> Int -> Array Int -> { tableGroup : Array Int, total_size : Int }
buildHuffmanTable tableGroup tableIdx rootBits codeLengths_ =
    let
        codeLengthsSize =
            Array.length codeLengths_

        count =
            generateCount codeLengths_ codeLengthsSize

        initialOffsets =
            generateOffsets count

        { sorted, offset } =
            sortSymbols { codeLengthsSize = codeLengthsSize, codeLengths = codeLengths_, offset = initialOffsets }

        tableBits =
            rootBits

        tableSize =
            Bitwise.shiftLeftBy tableBits 1

        totalSize =
            tableSize

        tableOffset =
            unsafeGet tableIdx tableGroup
    in
    if unsafeGet 15 offset == 1 then
        let
            go : Int -> Array Int -> Array Int
            go key arr =
                if key < totalSize then
                    go (key + 1) (Array.set (tableOffset + key) (unsafeGet 0 sorted) arr)

                else
                    arr
        in
        { tableGroup = go 0 tableGroup, total_size = totalSize }

    else
        let
            ( ( newCount, newKey ), newSymbol, newTableGroup ) =
                phase1 rootBits tableOffset tableSize sorted count 1 0 0 2 tableGroup

            mask =
                totalSize - 1

            low =
                -1

            currentOffset =
                tableOffset

            varstate =
                { count = newCount
                , len = rootBits + 1
                , key = newKey
                , symbol = newSymbol
                , step = 2
                , currentTableGroup = newTableGroup
                , currentOffset = currentOffset
                , tableBits = tableBits
                , tableSize = tableSize
                , totalSize = totalSize
                , low = low
                }
        in
        -- { (rootBits + 1) newKey newSymbol 2 newTableGroup currentOffset tableBits tableSize totalSize low
        huffmanTableLoop3 sorted mask tableOffset rootBits varstate


nextTableBitSize count initialLen rootBits =
    let
        initialLeft =
            Bitwise.shiftLeftBy (initialLen - rootBits) 1

        go len left =
            if len < 15 then
                let
                    newLeft =
                        left - unsafeGet len count
                in
                if newLeft <= 0 then
                    len - rootBits

                else
                    go (len + 1) (Bitwise.shiftLeftBy 1 newLeft)

            else
                len - rootBits
    in
    go initialLen initialLeft


huffmanTableLoop3 sorted mask tableOffset rootBits state =
    if state.len <= 15 then
        let
            newState =
                huffmanTableLoop4 sorted mask tableOffset rootBits state
        in
        huffmanTableLoop3 sorted mask tableOffset rootBits { newState | len = newState.len + 1, step = Bitwise.shiftLeftBy 1 newState.step }

    else
        { tableGroup = state.currentTableGroup, total_size = state.totalSize }


huffmanTableLoop4 sorted mask tableOffset rootBits ({ count, len, key, symbol, step, currentTableGroup, currentOffset, tableBits, tableSize, totalSize, low } as state) =
    if unsafeGet len count > 0 then
        if Bitwise.and key mask /= low then
            let
                newCurrentOffset =
                    currentOffset + tableSize

                newTableBits =
                    nextTableBitSize count len rootBits

                newTableSize =
                    Bitwise.shiftLeftBy newTableBits 1

                newTotalSize =
                    totalSize + newTableSize

                newLow =
                    Bitwise.and key mask

                value =
                    Bitwise.or (Bitwise.shiftLeftBy 16 (newTableBits + rootBits)) (newCurrentOffset - tableOffset - newLow)

                newTableGroup =
                    Array.set (tableOffset + newLow) value currentTableGroup
            in
            huffmanTableLoop4
                sorted
                mask
                tableOffset
                rootBits
                (finalize sorted
                    rootBits
                    { state
                        | currentOffset = newCurrentOffset
                        , tableBits = newTableBits
                        , tableSize = newTableSize
                        , totalSize = newTotalSize
                        , low = newLow
                        , currentTableGroup = newTableGroup
                    }
                )

        else
            huffmanTableLoop4 sorted mask tableOffset rootBits (finalize sorted rootBits state)

    else
        state


finalize sorted rootBits ({ count, len, key, symbol, step, currentTableGroup, currentOffset, tableBits, tableSize, totalSize, low } as state) =
    { state
        | currentTableGroup =
            replicateValue
                currentTableGroup
                (currentOffset + Bitwise.shiftRightBy rootBits key)
                step
                tableSize
                (Bitwise.shiftLeftBy 16 (len - rootBits) |> Bitwise.or (unsafeGet symbol sorted))
        , symbol = symbol + 1
        , key = getNextKey key len
        , count = update len (\x -> x - 1) count
    }


{-| TODO test
-}
replicateValue table offset step end item =
    let
        newEnd =
            end - step

        newTable =
            Array.set (offset + newEnd) item table
    in
    if newEnd > 0 then
        replicateValue newTable offset step newEnd item

    else
        newTable


checkDupes : Array Int -> Result String (Array Int)
checkDupes symbols =
    -- TODO optimize
    let
        go list =
            case list of
                [] ->
                    Ok symbols

                x :: xs ->
                    if List.member x xs then
                        Err "Duplicate simple huffman code symbol"

                    else
                        go xs
    in
    go (Array.toList symbols)


readBlockLength : Array Int -> Int -> State -> ( State, Int )
readBlockLength tableGroup tableIdx s0 =
    let
        s1 =
            topUpAccumulator s0
    in
    case readSymbol tableGroup tableIdx s1 of
        ( s2, code ) ->
            let
                n =
                    unsafeGet code block_length_n_bits
            in
            case
                if n <= 16 then
                    readFewBitsSafe n s2

                else
                    readManyBits n (topUpAccumulator s2)
            of
                ( s4, result ) ->
                    ( s4, unsafeGet code block_length_offset + result )


map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f d1 d2 s =
    case d1 s of
        Err e ->
            Err e

        Ok ( s1, x1 ) ->
            case d2 s1 of
                Err e ->
                    Err e

                Ok ( s2, x2 ) ->
                    Ok ( s2, f x1 x2 )


pure x s =
    ( s, x )


join : Decoder (Decoder a) -> Decoder a
join f s =
    case f s of
        Err e ->
            Err e

        Ok ( s2, g ) ->
            g s2


readBlocks : State -> Result Error State
readBlocks s0 =
    let
        readLiteralBlock initial =
            case decodeVarLenUnsignedByte initial of
                ( s1, numLiteralBlockTypes ) ->
                    case readMetablockPartition 0 (numLiteralBlockTypes + 1) { s1 | numLiteralBlockTypes = numLiteralBlockTypes + 1 } of
                        Err e ->
                            Err e

                        Ok ( s2, literalBlockLength ) ->
                            Ok { s2 | literalBlockLength = literalBlockLength }

        readCommandBlock initial =
            case decodeVarLenUnsignedByte initial of
                ( s1, numCommandBlockTypes ) ->
                    case readMetablockPartition 1 (numCommandBlockTypes + 1) { s1 | numCommandBlockTypes = numCommandBlockTypes + 1 } of
                        Err e ->
                            Err e

                        Ok ( s2, commandBlockLength ) ->
                            Ok { s2 | commandBlockLength = commandBlockLength }

        readDistanceBlock initial =
            case decodeVarLenUnsignedByte initial of
                ( s1, numDistanceBlockTypes ) ->
                    case readMetablockPartition 2 (numDistanceBlockTypes + 1) { s1 | numDistanceBlockTypes = numDistanceBlockTypes + 1 } of
                        Err e ->
                            Err e

                        Ok ( s2, distanceBlockLength ) ->
                            Ok { s2 | distanceBlockLength = distanceBlockLength }
    in
    readLiteralBlock s0
        |> Result.andThen readCommandBlock
        |> Result.andThen readDistanceBlock


readMetablockHuffmanCodesAndContextMaps : State -> Result Error State
readMetablockHuffmanCodesAndContextMaps =
    let
        topUp s =
            maybeReadMoreInput 2030 s
                |> Result.map topUpAccumulator

        readContextModes : Decoder (Array Int)
        readContextModes state =
            let
                go1 i s acc =
                    if i < s.numLiteralBlockTypes then
                        let
                            limit =
                                min (i + 96) s.numLiteralBlockTypes
                        in
                        case go2 limit i s acc of
                            Err e ->
                                Err e

                            Ok ( newI, newS, newAcc ) ->
                                case maybeReadMoreInput 2030 newS of
                                    Err e ->
                                        Err e

                                    Ok newerS ->
                                        go1 newI newerS newAcc

                    else
                        Ok ( s, acc )

                go2 limit i s0 acc =
                    if i < limit then
                        let
                            s1 =
                                topUpAccumulator s0
                        in
                        case readFewBits 2 s1 of
                            ( s2, cm ) ->
                                go2 limit (i + 1) s2 (Array.push cm acc)

                    else
                        Ok ( i, s0, acc )

                readFewBits2 n m w0 =
                    let
                        ( w1, v1 ) =
                            readFewBits n w0

                        ( w2, v2 ) =
                            readFewBits m w1
                    in
                    ( w2, ( v1, v2 ) )
            in
            case readFewBits2 2 4 state of
                ( newState, ( a, b ) ) ->
                    let
                        newerState =
                            { newState
                                | distancePostfixBits = a
                                , numDirectDistanceCodes = Bitwise.shiftLeftBy a b
                                , distancePostfixMask = Bitwise.shiftLeftBy a 1 - 1
                            }
                    in
                    go1 0 newerState Array.empty

        readContextMap ( s, contextModes ) =
            let
                s0 =
                    { s | contextModes = contextModes }

                size =
                    Bitwise.shiftLeftBy 6 s.numLiteralBlockTypes
            in
            decodeContextMap size (Array.repeat size 0) s0
                |> Result.andThen
                    (\( s1, ( contextMap, numLiteralTrees ) ) ->
                        let
                            isTrivial =
                                let
                                    go j =
                                        if j < Bitwise.shiftLeftBy 6 s1.numLiteralBlockTypes then
                                            if unsafeGet j contextMap /= Bitwise.shiftRightBy 6 j then
                                                False

                                            else
                                                go (j + 1)

                                        else
                                            True
                                in
                                go 0

                            s2 =
                                { s1
                                    | contextMap = contextMap
                                    , trivialLiteralContext =
                                        if isTrivial then
                                            1

                                        else
                                            0
                                }

                            distanceSize =
                                Bitwise.shiftLeftBy 2 s.numDistanceBlockTypes
                        in
                        case decodeContextMap distanceSize (Array.repeat distanceSize 0) s2 of
                            Err e ->
                                Err (log "got an error after decoding context map" e)

                            Ok ( s3, ( distContextMap, numDistTrees ) ) ->
                                let
                                    s4 =
                                        { s3 | distContextMap = distContextMap }
                                in
                                map2 Tuple.pair
                                    (decodeHuffmanTreeGroup 256 256 numLiteralTrees)
                                    (decodeHuffmanTreeGroup 704 704 s4.numCommandBlockTypes)
                                    s4
                                    |> Result.andThen
                                        (\( s5, ( literalTreeGroup, commandTreeGroup ) ) ->
                                            let
                                                s6 =
                                                    { s5 | literalTreeGroup = literalTreeGroup, commandTreeGroup = commandTreeGroup }

                                                distanceAlphabet =
                                                    if s.isLargeWindow then
                                                        let
                                                            max =
                                                                calculateDistanceAlphabetSize s6.distancePostfixBits s6.numDirectDistanceCodes 62
                                                        in
                                                        case calculateDistanceAlphabetLimit 0x7FFFFFFC s6.distancePostfixBits s6.numDirectDistanceCodes of
                                                            Ok v ->
                                                                Ok ( max, v )

                                                            Err e ->
                                                                Err e

                                                    else
                                                        let
                                                            max =
                                                                calculateDistanceAlphabetSize s6.distancePostfixBits s6.numDirectDistanceCodes 24
                                                        in
                                                        Ok ( max, max )
                                            in
                                            case distanceAlphabet of
                                                Err e ->
                                                    Err e

                                                Ok ( distanceAlphabetSizeMax, distanceAlphabetSizeLimit ) ->
                                                    decodeHuffmanTreeGroup distanceAlphabetSizeMax distanceAlphabetSizeLimit numDistTrees s6
                                                        |> Result.andThen
                                                            (\( s7, distanceTreeGroup ) ->
                                                                case calculateDistanceLut distanceAlphabetSizeLimit { s7 | distanceTreeGroup = distanceTreeGroup } of
                                                                    s8 ->
                                                                        let
                                                                            s9 =
                                                                                { s8
                                                                                    | contextMapSlice = 0
                                                                                    , distContextMapSlice = 0
                                                                                    , contextLookupOffset1 = (Array.get 0 s8.contextModes |> Maybe.withDefault 0) * 512
                                                                                    , contextLookupOffset2 = ((Array.get 0 s8.contextModes |> Maybe.withDefault 0) * 512) + 256
                                                                                    , literalTreeIdx = 0
                                                                                    , commandTreeIdx = 0
                                                                                    , rings =
                                                                                        s8.rings
                                                                                            |> Array.set 4 1
                                                                                            |> Array.set 5 0
                                                                                            |> Array.set 6 1
                                                                                            |> Array.set 7 0
                                                                                            |> Array.set 8 1
                                                                                            |> Array.set 9 0
                                                                                }
                                                                        in
                                                                        Ok s9
                                                            )
                                        )
                    )
    in
    readBlocks
        >> Result.andThen (\s -> topUp s)
        >> Result.andThen (\s -> readContextModes s)
        >> Result.andThen readContextMap


decodeContextMap : Int -> Array Int -> Decoder ( Array Int, Int )
decodeContextMap contextMapSize contextMap s0 =
    let
        readMaxRunLengthPrefix =
            readFewBits 1
                >> (\( s, useRleForZeros ) ->
                        if useRleForZeros /= 0 then
                            case readFewBits 4 s of
                                ( newS, v ) ->
                                    ( newS, v + 1 )

                        else
                            ( s, 0 )
                   )
    in
    case maybeReadMoreInput 2030 s0 of
        Err e ->
            Err e

        Ok s1 ->
            case decodeVarLenUnsignedByte s1 of
                ( s2, v ) ->
                    let
                        numTrees =
                            v + 1
                    in
                    if numTrees == 1 then
                        Ok ( s2, ( fill 0 0 contextMapSize contextMap, numTrees ) )

                    else
                        case readMaxRunLengthPrefix (topUpAccumulator s2) of
                            ( s3, maxRunLengthPrefix ) ->
                                let
                                    alphabetSize =
                                        numTrees + maxRunLengthPrefix

                                    tableSize =
                                        Array.get (Bitwise.shiftRightBy 5 (alphabetSize + 31)) max_huffman_table_size
                                            |> Maybe.withDefault 0

                                    tableIdx =
                                        (tableSize + 1) - 1
                                in
                                case readHuffmanCode alphabetSize alphabetSize (Array.repeat (tableSize + 1) 0) tableIdx s3 of
                                    Err e ->
                                        Err (log "error after reading huffman code" e)

                                    Ok ( s4, table ) ->
                                        let
                                            go2 i reps currentContextMap =
                                                if reps /= 0 then
                                                    if i >= contextMapSize then
                                                        Err "Corrupted context map"

                                                    else
                                                        go2 (i + 1) (reps - 1) (Array.set i 0 currentContextMap)

                                                else
                                                    Ok ( i, currentContextMap )

                                            go i currentContextMap s =
                                                if i < contextMapSize then
                                                    case maybeReadMoreInput 2030 s |> Result.map (topUpAccumulator >> readSymbol table.tableGroup tableIdx) of
                                                        Err e ->
                                                            Err e

                                                        Ok ( s5, code ) ->
                                                            if code == 0 then
                                                                go (i + 1) (Array.set i 0 currentContextMap) s5

                                                            else if code <= maxRunLengthPrefix then
                                                                case readFewBitsSafe code s5 of
                                                                    ( s6, reps_ ) ->
                                                                        let
                                                                            reps =
                                                                                Bitwise.shiftLeftBy code 1 + reps_
                                                                        in
                                                                        case go2 i reps currentContextMap of
                                                                            Err e ->
                                                                                Err e

                                                                            Ok ( newI, newContextMap ) ->
                                                                                go newI newContextMap s6

                                                            else
                                                                go (i + 1) (Array.set i (code - maxRunLengthPrefix) currentContextMap) s5

                                                else
                                                    Ok ( s, ( currentContextMap, numTrees ) )
                                        in
                                        go 0 contextMap s4
                                            |> Result.map
                                                (\( s, ( currentContextMap, currentNumTrees ) ) ->
                                                    case readFewBitsSafe 1 s of
                                                        ( newState, 1 ) ->
                                                            ( newState, ( inverseMoveToFrontTransform currentContextMap contextMapSize, currentNumTrees ) )

                                                        ( newState, _ ) ->
                                                            ( newState, ( currentContextMap, currentNumTrees ) )
                                                )


moveToFront : Int -> Array Int -> Array Int
moveToFront initialIndex v =
    let
        value =
            unsafeGet initialIndex v

        go index accum =
            if index > 0 then
                go (index - 1) (Array.set index (unsafeGet (index - 1) accum) accum)

            else
                Array.set 0 value accum
    in
    go initialIndex v


inverseMoveToFrontTransform v vLen =
    let
        go i mtf accum =
            if i < vLen then
                let
                    index =
                        Bitwise.and (unsafeGet i accum) 0xFF
                in
                go (i + 1)
                    (if index /= 0 then
                        moveToFront index mtf

                     else
                        mtf
                    )
                    (Array.set i (unsafeGet index mtf) accum)

            else
                accum
    in
    go 0 (Array.fromList (List.range 0 255)) v


{-| TODO test
<https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fill>
-}
fill : a -> Int -> Int -> Array a -> Array a
fill value start end arr =
    if start < end then
        fill value (start + 1) end (Array.set start value arr)

    else
        arr


decodeHuffmanTreeGroup : Int -> Int -> Int -> State -> Result Error ( State, Array Int )
decodeHuffmanTreeGroup alphabetSizeMax alphabetSizeLimit n =
    let
        maxTableSize =
            Array.get (Bitwise.shiftRightBy 5 (alphabetSizeLimit + 31)) max_huffman_table_size
                |> Maybe.withDefault 0

        initialGroup =
            Array.repeat (n + n * maxTableSize) 0

        go i group next s =
            if i < n then
                let
                    newGroup =
                        Array.set i next group
                in
                case readHuffmanCode alphabetSizeMax alphabetSizeLimit newGroup i s of
                    Err e ->
                        Err e

                    Ok ( newS, r ) ->
                        go (i + 1) r.tableGroup (next + r.total_size) newS

            else
                Ok ( s, group )
    in
    go 0 initialGroup n


type alias LutState a =
    { a | distExtraBits : Array Int, distOffset : Array Int, distancePostfixBits : Int, numDirectDistanceCodes : Int }


calculateDistanceLut : Int -> LutState a -> LutState a
calculateDistanceLut alphabetSizeLimit state =
    let
        npostfix =
            state.distancePostfixBits

        ndirect =
            state.numDirectDistanceCodes

        postfix =
            Bitwise.shiftLeftBy npostfix 1

        go1 i j bits base upperLimit s =
            if j < upperLimit then
                go1 (i + 1)
                    (j + 1)
                    bits
                    base
                    upperLimit
                    { s
                        | distExtraBits = Array.set i bits s.distExtraBits
                        , distOffset = Array.set i (j + base) s.distOffset
                    }

            else
                ( s, i )

        go2 bits half ( s, i ) =
            if i < alphabetSizeLimit then
                let
                    base =
                        ndirect + Bitwise.shiftLeftBy npostfix (Bitwise.shiftLeftBy bits (2 + half) - 4) + 1

                    ( newS, newI ) =
                        go1 i 0 bits base postfix s
                in
                go2 (bits + half) (Bitwise.xor half 1) ( newS, newI )

            else
                s
    in
    go1 16 0 0 1 ndirect state
        |> go2 1 0


decompress : State -> Result Error ( Bytes, State )
decompress unvalidated =
    if unvalidated.runningState == 0 then
        Err "Can't decompreunvalidateds until initialized"

    else if unvalidated.runningState == 11 then
        Err "Can't decompress after close"

    else
        let
            s =
                if unvalidated.runningState == 1 then
                    case decodeWindowBits unvalidated of
                        ( newS, windowBits ) ->
                            if windowBits == -1 then
                                Err "Invalid 'windowBits' code"

                            else
                                Ok
                                    { newS
                                        | maxRingBufferSize = Bitwise.shiftLeftBy windowBits 1
                                        , maxBackwardDistance = Bitwise.shiftLeftBy windowBits 1 - 16
                                        , runningState = 2
                                    }

                else
                    Ok unvalidated

            work s2 =
                let
                    fence =
                        calculateFence s2
                in
                decompressHelp
                    { fence = fence
                    , ringBufferMask = s2.ringBufferSize - 1
                    }
                    s2
        in
        case s |> Result.andThen work of
            Err e ->
                Err e

            Ok ( r, _ ) ->
                Ok
                    ( r.output
                        |> Array.slice 0 r.written.toOutput
                        |> encodeByteArray
                        |> Encode.encode
                    , r
                    )


encodeByteArray : Array Int -> Encode.Encoder
encodeByteArray arr =
    let
        bytes =
            Array.toList arr

        {- NOTE the bytes can infact use more than the first 8 bits, so we have to mask them -}
        go remaining encoders =
            case remaining of
                x :: y :: z :: w :: rest ->
                    let
                        value =
                            Bitwise.or
                                (Bitwise.or (Bitwise.shiftLeftBy 24 x) (Bitwise.shiftLeftBy 16 (Bitwise.and 0xFF y)))
                                (Bitwise.or (Bitwise.shiftLeftBy 8 (Bitwise.and 0xFF z)) (Bitwise.and 0xFF w))
                    in
                    go rest (Encode.unsignedInt32 BE value :: encoders)

                [] ->
                    encoders

                [ x ] ->
                    Encode.unsignedInt8 x :: encoders

                [ x, y ] ->
                    let
                        value =
                            Bitwise.or (Bitwise.shiftLeftBy 8 (Bitwise.and 0xFF x)) (Bitwise.and 0xFF y)
                    in
                    Encode.unsignedInt16 BE value :: encoders

                [ x, y, z ] ->
                    let
                        value =
                            Bitwise.or (Bitwise.shiftLeftBy 8 (Bitwise.and 0xFF x)) (Bitwise.and 0xFF y)
                    in
                    Encode.unsignedInt8 z :: Encode.unsignedInt16 BE value :: encoders

        {-
           [ x, y ] ->
               let
                   value =
                       Bitwise.or (Bitwise.shiftLeftBy 8 x) y
               in
               Encode.unsignedInt16 BE value :: encoders

           [ x, y, z ] ->
               let
                   value =
                       Bitwise.or (Bitwise.shiftLeftBy 8 x) y
               in
               go [ y, z ] (Encode.unsignedInt8 x :: encoders)
           x :: y :: z :: w :: rest ->
               let
                   value =
                       Bitwise.or
                           (Bitwise.or (Bitwise.shiftLeftBy 24 x) (Bitwise.shiftLeftBy 16 y))
                           (Bitwise.or (Bitwise.shiftLeftBy 8 z) w)
               in

               go rest (Encode.unsignedInt32 BE value :: encoders)
        -}
    in
    Encode.sequence (List.reverse (go bytes []))


type alias Context =
    { fence : Int
    , ringBufferMask : Int
    }


decompressHelp : Context -> Decoder Context
decompressHelp context s =
    let
        _ =
            -- Debug.log "state" ( s.runningState, ( s.pos, s.bitOffset, s.accumulator32 ) )
            log "state" s.runningState
    in
    case s.runningState of
        10 ->
            Ok ( s, context )

        2 ->
            if s.metaBlockLength < 0 then
                Err "Invalid metablock length"

            else
                case readNextMetablockHeader s of
                    Err e ->
                        Err e

                    Ok s2 ->
                        let
                            fence =
                                calculateFence s2
                        in
                        decompressHelp { fence = fence, ringBufferMask = s2.ringBufferSize - 1 } s2

        3 ->
            case readMetablockHuffmanCodesAndContextMaps s of
                Err e ->
                    Err e

                Ok s2 ->
                    decompressHelp context (updateRunningState 4 s2)

        4 ->
            case evaluateState4 context s of
                Err e ->
                    Err e

                Ok newState ->
                    decompressHelp context newState

        7 ->
            if s.trivialLiteralContext /= 0 then
                let
                    maybeLiteral state =
                        if state.literalBlockLength == 0 then
                            decodeLiteralBlockSwitch state

                        else
                            state

                    go currentContext s0 =
                        if s0.j < s0.insertLength then
                            case maybeReadMoreInput 2030 s0 |> Result.map (maybeLiteral >> topUpAccumulator) of
                                Err e ->
                                    Err e

                                Ok s1 ->
                                    case readSymbol s1.literalTreeGroup s1.literalTreeIdx s1 of
                                        ( s2, value ) ->
                                            let
                                                newRingBuffer =
                                                    Array.set s2.pos value s2.ringBuffer

                                                newPos =
                                                    s2.pos + 1
                                            in
                                            if newPos >= context.fence then
                                                remainder7 context { s2 | nextRunningState = 7, runningState = 13, pos = s2.pos + 1, j = s2.j + 1, ringBuffer = newRingBuffer, literalBlockLength = s2.literalBlockLength - 1 }

                                            else
                                                go context (copyState7 newPos (s2.j + 1) newRingBuffer (s2.literalBlockLength - 1) s2)

                        else
                            remainder7 currentContext { s0 | literalBlockLength = s0.literalBlockLength - 1 }
                in
                case go context s of
                    Err e ->
                        Err e

                    Ok newState ->
                        decompressHelp context newState

            else
                let
                    init_prevByte1 =
                        unsafeGet (Bitwise.and (s.pos - 1) context.ringBufferMask) s.ringBuffer
                            |> Bitwise.and 0xFF

                    init_prevByte2 =
                        unsafeGet (Bitwise.and (s.pos - 2) context.ringBufferMask) s.ringBuffer
                            |> Bitwise.and 0xFF
                in
                case evaluateState7 context init_prevByte1 init_prevByte2 s of
                    Err e ->
                        Err e

                    Ok newState ->
                        decompressHelp context newState

        8 ->
            decompressHelp context (evaluateState8 context s)

        9 ->
            if s.distance > 0x7FFFFFFC then
                Err "Invalid backward reference 1"

            else if s.copyLength >= 4 && s.copyLength <= 24 then
                let
                    offset =
                        unsafeGet s.copyLength dictionary_offsets_by_length + wordIdx * s.copyLength

                    wordId =
                        s.distance - s.maxDistance - 1

                    shift =
                        unsafeGet s.copyLength dictionary_size_bits_by_length

                    mask =
                        Bitwise.shiftLeftBy shift 1 - 1

                    wordIdx =
                        Bitwise.and wordId mask

                    transformIdx =
                        Bitwise.shiftRightZfBy shift wordId
                in
                if transformIdx < 121 then
                    let
                        ( newRingBuffer, len ) =
                            Transforms.transformDictionaryWord s.ringBuffer s.pos dictionary_data offset s.copyLength Transforms.rfc_transforms transformIdx

                        newMetaBlockLength =
                            s.metaBlockLength - len

                        newPos =
                            s.pos + len
                    in
                    -- TODO curiously, using a custom update function here seems to be bad; it increases GC time a lot
                    -- with no clear gain in speed because of fewer record updates
                    -- but that is weird right, because both methods should be generating the same amount of garbage?
                    if newPos >= context.fence then
                        decompressHelp context (updateEvaluateState9 4 13 newRingBuffer (s.pos + len) newMetaBlockLength s)
                        --decompressHelp context { s | nextRunningState = 4, runningState = 13, ringBuffer = newRingBuffer, pos = s.pos + len, metaBlockLength = newMetaBlockLength }

                    else
                        -- decompressHelp context { s | runningState = 4, ringBuffer = newRingBuffer, pos = s.pos + len, metaBlockLength = newMetaBlockLength }
                        decompressHelp context (updateEvaluateState9 s.nextRunningState 4 newRingBuffer (s.pos + len) newMetaBlockLength s)

                else
                    Err "Invalid backward reference 2"

            else
                Err ("Invalid backward reference 3: copyLength is " ++ String.fromInt s.copyLength)

        6 ->
            copyUncompressedData s
                |> Result.andThen (decompressHelp context)

        12 ->
            -- should not happen, go directly to state 13
            decompressHelp context s

        13 ->
            case writeRingBuffer (min s.pos s.ringBufferSize) s of
                ( s0, 0 ) ->
                    Ok ( s0, context )

                ( s0, _ ) ->
                    let
                        newMaxDistance =
                            if s0.pos >= s0.maxBackwardDistance then
                                s0.maxBackwardDistance

                            else
                                s0.maxDistance

                        s1 =
                            s0
                    in
                    if s1.pos >= s1.ringBufferSize then
                        let
                            newRingBuffer =
                                copyWithin 0 s1.ringBufferSize s1.pos s1.ringBuffer
                        in
                        decompressHelp context
                            { s1
                                | pos = Bitwise.and s1.pos context.ringBufferMask
                                , written = { fromRingBuffer = 0, toOutput = s1.written.toOutput }
                                , runningState = s1.nextRunningState
                                , ringBuffer = newRingBuffer
                                , maxDistance = newMaxDistance
                            }

                    else
                        decompressHelp context
                            { s1
                                | runningState = s1.nextRunningState
                                , maxDistance = newMaxDistance
                            }

        _ ->
            Ok ( s, context )


evaluateState4 : Context -> State -> Result Error State
evaluateState4 context s =
    if s.metaBlockLength <= 0 then
        Ok { s | runningState = 2 }

    else
        case maybeReadMoreInput 2030 s of
            Err e ->
                Err e

            Ok s1 ->
                let
                    maybeCommandBlock =
                        if s1.commandBlockLength == 0 then
                            decodeCommandBlockSwitch s1

                        else
                            s1
                in
                case maybeCommandBlock |> topUpAccumulator of
                    s2 ->
                        case readSymbol s2.commandTreeGroup s2.commandTreeIdx s2 of
                            ( s3, v ) ->
                                let
                                    cmdCode =
                                        Bitwise.shiftLeftBy 2 v

                                    insertAndCopyExtraBits =
                                        unsafeGet (cmdCode + 0) cmd_lookup

                                    insertLengthOffset =
                                        unsafeGet (cmdCode + 1) cmd_lookup

                                    copyLengthOffset =
                                        unsafeGet (cmdCode + 2) cmd_lookup

                                    readInsertLength state =
                                        let
                                            extraBits =
                                                Bitwise.and insertAndCopyExtraBits 0xFF
                                        in
                                        readBits extraBits state

                                    readCopyLength ( state, insertLengthBits ) =
                                        let
                                            extraBits =
                                                Bitwise.shiftRightBy 8 insertAndCopyExtraBits
                                        in
                                        readBits extraBits state
                                            |> (\( s_, w ) -> ( s_, ( insertLengthBits, w ) ))
                                in
                                case s3 |> topUpAccumulator |> readInsertLength |> readCopyLength of
                                    ( s5, ( insertLengthBits, copyLengthBits ) ) ->
                                        Ok (updateEvaluateState4 0 7 (copyLengthBits + copyLengthOffset) (insertLengthBits + insertLengthOffset) (unsafeGet (cmdCode + 3) cmd_lookup) (s3.commandBlockLength - 1) s5)


evaluateState7 : Context -> Int -> Int -> State -> Result Error State
evaluateState7 context prevByte1 prevByte2 s0 =
    let
        maybeLiteral state =
            if state.literalBlockLength == 0 then
                decodeLiteralBlockSwitch state

            else
                state
    in
    if s0.j < s0.insertLength then
        case maybeReadMoreInput 2030 s0 |> Result.map (maybeLiteral >> topUpAccumulator) of
            Err e ->
                Err e

            Ok s1 ->
                let
                    -- _ = Debug.log "----> s1 " ( ( s1.pos, s1.bitOffset, s1.accumulator32 ), (prevByte1, prevByte2), (v1,v2) )
                    i1 =
                        s1.contextLookupOffset1 + prevByte1

                    i2 =
                        s1.contextLookupOffset2 + prevByte2

                    v1 =
                        unsafeGet i1 lookup

                    v2 =
                        unsafeGet i2 lookup

                    literalContext =
                        Bitwise.or v1 v2

                    literalTreeIdx =
                        unsafeGet (s1.contextMapSlice + literalContext) s1.contextMap
                            |> Bitwise.and 0xFF

                    s2 =
                        s1

                    byte2 =
                        prevByte1
                in
                case readSymbol s2.literalTreeGroup literalTreeIdx s2 of
                    ( s3, byte1 ) ->
                        let
                            newRingBuffer =
                                Array.set s3.pos byte1 s3.ringBuffer

                            newPos =
                                s3.pos + 1
                        in
                        if newPos >= context.fence then
                            remainder7 context { s3 | nextRunningState = 7, runningState = 13, pos = newPos, j = s3.j + 1, ringBuffer = newRingBuffer, literalBlockLength = s3.literalBlockLength - 1 }

                        else
                            evaluateState7 context byte1 byte2 (copyState7 newPos (s3.j + 1) newRingBuffer (s3.literalBlockLength - 1) s3)

    else
        remainder7 context s0


evaluateState8 : Context -> State -> State
evaluateState8 context s =
    let
        src =
            Bitwise.and (s.pos - s.distance) context.ringBufferMask

        dst =
            s.pos

        copyLength =
            s.copyLength - s.j

        srcEnd =
            src + copyLength

        dstEnd =
            dst + copyLength
    in
    if (srcEnd < context.ringBufferMask) && (dstEnd < context.ringBufferMask) then
        let
            newRingBuffer =
                if copyLength < 12 || (srcEnd > dst && dstEnd > src) then
                    let
                        go k currentDst currentSrc accum =
                            if k < copyLength then
                                go (k + 4)
                                    (currentDst + 4)
                                    (currentSrc + 4)
                                    (let
                                        a0 =
                                            accum

                                        a1 =
                                            Array.set (currentDst + 0) (unsafeGet (currentSrc + 0) a0) a0

                                        a2 =
                                            Array.set (currentDst + 1) (unsafeGet (currentSrc + 1) a1) a1

                                        a3 =
                                            Array.set (currentDst + 2) (unsafeGet (currentSrc + 2) a2) a2

                                        a4 =
                                            Array.set (currentDst + 3) (unsafeGet (currentSrc + 3) a3) a3
                                     in
                                     a4
                                    )

                            else
                                accum
                    in
                    go 0 dst src s.ringBuffer

                else
                    copyWithin dst src srcEnd s.ringBuffer
        in
        updateEvaluateState8
            newRingBuffer
            (s.j + copyLength)
            (s.metaBlockLength - copyLength)
            (s.pos + copyLength)
            (if s.runningState == 8 then
                4

             else
                s.runningState
            )
            s

    else
        -- NOTE this branch is untested; seems to almost never get hit
        let
            go : Int -> { ringBuffer : Array Int, metaBlockLength : Int, pos : Int, j : Int } -> { ringBuffer : Array Int, metaBlockLength : Int, pos : Int, j : Int }
            go distance state =
                let
                    s1 =
                        { ringBuffer = state.ringBuffer |> Array.set state.pos (unsafeGet (Bitwise.and (state.pos - distance) context.ringBufferMask) state.ringBuffer)
                        , metaBlockLength = state.metaBlockLength - 1
                        , pos = state.pos + 1
                        , j = state.j + 1
                        }
                in
                if s1.pos >= context.fence then
                    s1

                else
                    go distance s1

            smallerState =
                { ringBuffer = s.ringBuffer
                , metaBlockLength = s.metaBlockLength
                , pos = s.pos
                , j = s.j
                }

            newSmallerState =
                go s.distance smallerState
        in
        { s
            | nextRunningState = 8
            , runningState = 13
            , ringBuffer = newSmallerState.ringBuffer
            , metaBlockLength = newSmallerState.metaBlockLength
            , pos = newSmallerState.pos
            , j = newSmallerState.j
        }


setArraySlice slice startIndex whole =
    let
        before =
            Array.slice 0 startIndex whole

        after =
            Array.slice (startIndex + Array.length slice) (Array.length whole) whole

        result =
            Array.append before (Array.append slice after)
    in
    result


copyUncompressedData : State -> Result Error State
copyUncompressedData s =
    if s.metaBlockLength <= 0 then
        reload s
            |> Result.map (\state -> { state | runningState = 2 })

    else
        let
            chunkLength =
                min (s.ringBufferSize - s.pos) s.metaBlockLength
        in
        case copyBytesToRingBuffer s.pos chunkLength s.ringBuffer s of
            Err e ->
                Err e

            Ok ( s1, newRingBuffer ) ->
                let
                    s2 =
                        { s1 | ringBuffer = newRingBuffer, metaBlockLength = s1.metaBlockLength - chunkLength, pos = s1.pos + chunkLength }
                in
                if s2.pos == s2.ringBufferSize then
                    Ok { s2 | nextRunningState = 6, runningState = 13 }

                else
                    reload s2
                        |> Result.map (\state -> { state | runningState = 2 })


copyBytesToRingBuffer offset length data s =
    -- NOTE data is the ringbufffer
    if Bitwise.and s.bitOffset 7 /= 0 then
        Err "unaligned copyBytes"

    else
        let
            loop1 state currentOffset currentLength accum =
                if state.bitOffset /= 32 && currentLength /= 0 then
                    loop1 { state | bitOffset = state.bitOffset + 8 } (currentOffset + 1) (currentLength - 1) (Array.set currentOffset (Bitwise.shiftRightZfBy state.bitOffset state.accumulator32) accum)

                else
                    ( state, ( currentOffset, currentLength ), accum )

            ( s1, ( offset1, length1 ), data1 ) =
                loop1 s offset length data

            maybeCopyNibbles state currentOffset currentLength accum =
                let
                    copyNibbles =
                        min (halfAvailable state) (Bitwise.shiftRightBy 1 currentLength)
                in
                if copyNibbles > 0 then
                    let
                        readOffset =
                            Bitwise.shiftLeftBy 1 state.halfOffset

                        delta =
                            Bitwise.shiftLeftBy 1 copyNibbles
                    in
                    ( { state | halfOffset = state.halfOffset + copyNibbles }
                    , ( currentOffset + delta, currentLength - delta )
                    , setArraySlice (Array.slice readOffset (readOffset + delta) state.byteBuffer) currentOffset accum
                    )

                else
                    ( state
                    , ( currentOffset, currentLength )
                    , accum
                    )

            maybeWriteMore s0 offset0 length0 accum0 =
                let
                    go state currentOffset currentLength accum =
                        if currentLength /= 0 then
                            go
                                (updateBitOffset (state.bitOffset + 8) state)
                                (currentOffset + 1)
                                (currentLength - 1)
                                (Array.set currentOffset (Bitwise.shiftRightZfBy state.bitOffset state.accumulator32) accum)

                        else
                            ( state, accum )

                    ( s3, newData ) =
                        go (topUpAccumulator s0) offset0 length0 accum0
                in
                checkHealth False s3
                    |> Result.map (\st -> ( st, newData ))

            readFromInput state currentOffset currentLength accum =
                if currentLength > 0 then
                    case readInput currentOffset currentLength state of
                        Err e ->
                            Err e

                        Ok ( newState, len ) ->
                            if len == -1 then
                                Err "unexpected end of input"

                            else
                                readFromInput newState (currentOffset + len) (currentLength - len) accum

                else
                    Ok ( state, accum )
        in
        if length1 == 0 then
            Ok ( s1, data1 )

        else
            let
                ( s2, ( offset2, length2 ), data2 ) =
                    maybeCopyNibbles s1 offset1 length1 data1
            in
            if length2 == 0 then
                Ok ( s2, data2 )

            else if halfAvailable s2 > 0 then
                maybeWriteMore s2 offset2 length2 data2

            else
                readFromInput s2 offset2 length2 data2


writeRingBuffer : Int -> State -> ( State, Int )
writeRingBuffer ringBufferBytesReady s =
    let
        toWrite =
            min (outputLength - s.written.toOutput) (ringBufferBytesReady - s.written.fromRingBuffer)

        wasThereWritten state =
            if state.written.toOutput < outputLength then
                1

            else
                0

        newState =
            if toWrite /= 0 then
                let
                    slice =
                        Array.slice s.written.fromRingBuffer (s.written.fromRingBuffer + toWrite) s.ringBuffer

                    newOutput =
                        Array.append s.output slice

                    newWritten =
                        { toOutput = s.written.toOutput + toWrite, fromRingBuffer = s.written.fromRingBuffer + toWrite }

                    s2 =
                        { s | output = newOutput, written = newWritten }
                in
                s2

            else
                s
    in
    ( newState, wasThereWritten newState )


remainder7 : Context -> State -> Result Error State
remainder7 context s =
    let
        _ =
            log "state in remainder " ( s.runningState, ( s.pos, s.bitOffset, s.accumulator32 ) )
    in
    if s.runningState /= 7 then
        Ok s

    else
        let
            s0 =
                s

            newMetaBlockLength =
                s0.metaBlockLength - s0.insertLength

            s1 =
                s0
        in
        if newMetaBlockLength <= 0 then
            Ok { s1 | runningState = 4, metaBlockLength = newMetaBlockLength }

        else
            let
                step1 =
                    let
                        oldDistanceCode =
                            s1.distanceCode
                    in
                    if oldDistanceCode < 0 then
                        let
                            newDistance =
                                unsafeGet s1.distRbIdx s1.rings
                        in
                        Ok
                            { state = s1
                            , distance = newDistance
                            , distanceCode = oldDistanceCode
                            , distanceBlockLength = s1.distanceBlockLength
                            , metaBlockLength = newMetaBlockLength
                            }

                    else
                        let
                            maybeDistance state =
                                if state.distanceBlockLength == 0 then
                                    decodeDistanceBlockSwitch state

                                else
                                    state
                        in
                        case maybeReadMoreInput 2030 s1 |> Result.map (maybeDistance >> topUpAccumulator) of
                            Err e ->
                                Err e

                            Ok s2 ->
                                let
                                    distTreeIdx =
                                        Bitwise.and 0xFF (unsafeGet (s2.distContextMapSlice + oldDistanceCode) s2.distContextMap)
                                in
                                -- guess: the distanceTreeGroup is incorrect
                                case readSymbol s2.distanceTreeGroup distTreeIdx s2 of
                                    ( s3, distanceCode ) ->
                                        if distanceCode < 16 then
                                            let
                                                index =
                                                    Bitwise.and (s3.distRbIdx + unsafeGet distanceCode distance_short_code_index_offset) 0x03

                                                newDistance =
                                                    unsafeGet index s3.rings + unsafeGet distanceCode distance_short_code_value_offset
                                            in
                                            if newDistance < 0 then
                                                Err "negative distance"

                                            else
                                                Ok
                                                    { state = s3
                                                    , distance = newDistance
                                                    , distanceCode = distanceCode
                                                    , distanceBlockLength = s3.distanceBlockLength - 1
                                                    , metaBlockLength = newMetaBlockLength
                                                    }

                                        else
                                            let
                                                extraBits =
                                                    unsafeGet distanceCode s3.distExtraBits

                                                readSomeBits =
                                                    if s3.bitOffset + extraBits <= 32 then
                                                        readFewBits extraBits s3

                                                    else
                                                        readBits extraBits (topUpAccumulator s3)
                                            in
                                            case readSomeBits of
                                                ( s4, bits ) ->
                                                    let
                                                        newDistance =
                                                            unsafeGet distanceCode s4.distOffset
                                                                + Bitwise.shiftLeftBy s4.distancePostfixBits bits
                                                    in
                                                    Ok
                                                        { state = s4
                                                        , distance = newDistance
                                                        , distanceCode = distanceCode
                                                        , distanceBlockLength = s4.distanceBlockLength - 1
                                                        , metaBlockLength = newMetaBlockLength
                                                        }
            in
            case step1 of
                Err e ->
                    Err e

                Ok new ->
                    let
                        s5 =
                            new.state

                        -- new.state
                        newMaxDistance =
                            if new.distance /= s5.maxBackwardDistance && s5.pos < s5.maxBackwardDistance then
                                s5.pos

                            else
                                s5.maxBackwardDistance
                    in
                    if new.distance > newMaxDistance then
                        -- Ok { s5 | runningState = 9, maxDistance = newMaxDistance, distance = newDistance }
                        Ok
                            (updateRemainder7
                                new.distance
                                newMaxDistance
                                new.distanceBlockLength
                                new.metaBlockLength
                                s5.nextRunningState
                                9
                                s5.distRbIdx
                                s5.rings
                                s5
                            )

                    else if s5.copyLength > s.metaBlockLength then
                        Err ("Invalid backward reference in remainder7 at position " ++ String.fromInt s5.pos)

                    else if new.distanceCode > 0 then
                        let
                            distRbIdx =
                                (s5.distRbIdx + 1) |> Bitwise.and 0x03
                        in
                        Ok
                            (updateRemainder7
                                new.distance
                                newMaxDistance
                                new.distanceBlockLength
                                new.metaBlockLength
                                0
                                8
                                distRbIdx
                                (Array.set distRbIdx new.distance s5.rings)
                                s5
                            )

                    else
                        Ok
                            (updateRemainder7
                                new.distance
                                newMaxDistance
                                new.distanceBlockLength
                                new.metaBlockLength
                                0
                                8
                                s5.distRbIdx
                                s5.rings
                                s5
                            )


decodeLiteralBlockSwitch : State -> State
decodeLiteralBlockSwitch s0 =
    case decodeBlockTypeAndLength 0 s0.numLiteralBlockTypes s0 of
        ( s1, newRings, literalBlockLength ) ->
            let
                literalBlockType =
                    unsafeGet 5 newRings

                newContextMapSlice =
                    Bitwise.shiftLeftBy 6 literalBlockType

                contextMode =
                    unsafeGet literalBlockType s1.contextModes

                newLiteralTreeIdx =
                    Bitwise.and (unsafeGet newContextMapSlice s1.contextMap) 0xFF
            in
            { s1
                | contextMapSlice = newContextMapSlice
                , literalBlockLength = literalBlockLength
                , literalTreeIdx = newLiteralTreeIdx
                , contextLookupOffset1 = Bitwise.shiftLeftBy 9 contextMode
                , contextLookupOffset2 = Bitwise.shiftLeftBy 9 contextMode + 256
                , rings = newRings
            }


decodeDistanceBlockSwitch : State -> State
decodeDistanceBlockSwitch s0 =
    case decodeBlockTypeAndLength 2 s0.numDistanceBlockTypes s0 of
        ( s1, newRings, v ) ->
            { s1
              -- add  -1 to distanceBlockLength here to prevent extra record update
                | distanceBlockLength = v
                , distContextMapSlice = Bitwise.shiftLeftBy 2 (unsafeGet 9 newRings)
                , rings = newRings
            }


decodeCommandBlockSwitch s0 =
    case decodeBlockTypeAndLength 1 s0.numCommandBlockTypes s0 of
        ( s1, newRings, v ) ->
            { s1
                | commandBlockLength = v
                , commandTreeIdx = unsafeGet 7 newRings
                , rings = newRings
            }


decodeBlockTypeAndLength : Int -> Int -> State -> ( State, Array Int, Int )
decodeBlockTypeAndLength treeType numBlockTypes s0 =
    let
        offset =
            4 + treeType * 2

        s1 =
            topUpAccumulator s0
    in
    case readSymbol s1.blockTrees (2 * treeType) s1 of
        ( s2, initialBlockType ) ->
            case readBlockLength s2.blockTrees (2 * treeType + 1) s2 of
                ( s3, result ) ->
                    let
                        blockType =
                            (case initialBlockType of
                                1 ->
                                    unsafeGet (offset + 1) s3.rings + 1

                                0 ->
                                    unsafeGet offset s3.rings

                                other ->
                                    other - 2
                            )
                                |> (\v ->
                                        if v >= numBlockTypes then
                                            v - numBlockTypes

                                        else
                                            v
                                   )
                    in
                    ( s3
                    , s3.rings
                        |> Array.set offset (unsafeGet (offset + 1) s3.rings)
                        |> Array.set (offset + 1) blockType
                    , result
                    )


calculateFence : State -> Int
calculateFence s =
    let
        result =
            s.ringBufferSize

        value =
            if s.isEager then
                min result (s.written.fromRingBuffer + outputLength - s.written.toOutput)

            else
                result
    in
    value


halfAvailable : { state | endOfStreamReached : Bool, halfOffset : Int, tailBytes : Int } -> Int
halfAvailable s =
    let
        limit =
            if s.endOfStreamReached then
                Bitwise.shiftRightBy 1 (s.tailBytes + 1)

            else
                2048
    in
    limit - s.halfOffset


jumpToByteBoundary : State -> Result Error State
jumpToByteBoundary s =
    let
        padding =
            Bitwise.and (32 - s.bitOffset) 7
    in
    if padding /= 0 then
        case readFewBits padding s of
            ( s2, paddingBits ) ->
                if paddingBits /= 0 then
                    Err "Corrupted padding bits"

                else
                    Ok s2

    else
        Ok s


readBits : Int -> State -> ( State, Int )
readBits nbits state =
    if nbits <= 16 then
        let
            ( newBitOffset, newHalfOffset, newAccumulator32 ) =
                if state.bitOffset >= 16 then
                    let
                        next =
                            unsafeGet state.halfOffset state.shortBuffer
                                |> Bitwise.shiftLeftBy 16
                    in
                    ( state.bitOffset - 16, state.halfOffset + 1, Bitwise.or next (Bitwise.shiftRightZfBy 16 state.accumulator32) )

                else
                    ( state.bitOffset, state.halfOffset, state.accumulator32 )

            ( newerBitOffset, val ) =
                pureReadFewBits nbits newBitOffset newAccumulator32
        in
        ( updateAccumulator newerBitOffset newHalfOffset newAccumulator32 state, val )

    else
        readManyBits nbits
            (if state.bitOffset >= 16 then
                putOnAccumulator state

             else
                state
            )


pureReadFewBits : Int -> Int -> Int -> ( Int, Int )
pureReadFewBits n bitOffset accumulator32 =
    let
        val =
            Bitwise.and (Bitwise.shiftRightZfBy bitOffset accumulator32) (Bitwise.shiftLeftBy n 1 - 1)
    in
    ( bitOffset + n, val )


readFewBits : Int -> State -> ( State, Int )
readFewBits n s =
    let
        ( newBitOffset, val ) =
            pureReadFewBits n s.bitOffset s.accumulator32
    in
    ( updateBitOffset newBitOffset s, val )


readFewBitsSafe : Int -> State -> ( State, Int )
readFewBitsSafe nbits state =
    -- A fused version of `readFewBits n (topUpAccumulator state)`
    let
        ( newBitOffset, newHalfOffset, newAccumulator32 ) =
            if state.bitOffset >= 16 then
                let
                    next =
                        unsafeGet state.halfOffset state.shortBuffer
                            |> Bitwise.shiftLeftBy 16
                in
                ( state.bitOffset - 16, state.halfOffset + 1, Bitwise.or next (Bitwise.shiftRightZfBy 16 state.accumulator32) )

            else
                ( state.bitOffset, state.halfOffset, state.accumulator32 )

        ( newerBitOffset, val ) =
            pureReadFewBits nbits newBitOffset newAccumulator32
    in
    ( updateAccumulator newerBitOffset newHalfOffset newAccumulator32 state, val )


readManyBits : Int -> State -> ( State, Int )
readManyBits n inputS =
    case readFewBits 16 inputS of
        ( s, low ) ->
            let
                newAccumulator32 =
                    Bitwise.or
                        (Bitwise.shiftLeftBy 16 (unsafeGet s.halfOffset s.shortBuffer))
                        (Bitwise.shiftRightZfBy 16 s.accumulator32)
            in
            updateAccumulator (s.bitOffset - 16) (s.halfOffset + 1) newAccumulator32 s
                |> readFewBits (n - 16)
                |> Tuple.mapSecond (\high -> Bitwise.or low (Bitwise.shiftLeftBy 16 high))


type alias ReadInputState s =
    { s
        | endOfStreamReached : Bool
        , tailBytes : Int
        , input : InputStream
        , byteBuffer : Array Int
        , halfOffset : Int
        , shortBuffer : Array Int
        , pos : Int
    }


maybeReadMoreInput : Int -> ReadInputState s -> Result Error (ReadInputState s)
maybeReadMoreInput n s =
    if s.halfOffset > n then
        doReadMoreInput s

    else
        Ok s


doReadMoreInput : ReadInputState s -> Result Error (ReadInputState s)
doReadMoreInput s =
    if s.endOfStreamReached then
        if halfAvailable s >= -2 then
            Ok s

        else
            Err "No more inupt"

    else
        let
            readOffset =
                Bitwise.shiftLeftBy 1 s.halfOffset

            bytesInBuffer =
                4096 - readOffset
        in
        case doReadMoreInputHelp bytesInBuffer { s | byteBuffer = copyWithin 0 readOffset 4096 s.byteBuffer, halfOffset = 0 } of
            Err e ->
                Err e

            Ok ( s2, bytesInBuffer2 ) ->
                Ok (bytesToNibbles bytesInBuffer2 s2)


doReadMoreInputHelp :
    Int
    -> { state | endOfStreamReached : Bool, tailBytes : Int, input : InputStream, byteBuffer : Array Int }
    -> Result Error ( { state | endOfStreamReached : Bool, tailBytes : Int, input : InputStream, byteBuffer : Array Int }, Int )
doReadMoreInputHelp bytesInBuffer s =
    if bytesInBuffer < 4096 then
        let
            spaceLeft =
                4096 - bytesInBuffer
        in
        case readInput bytesInBuffer spaceLeft s of
            Err e ->
                Err e

            Ok ( s2, len ) ->
                if len <= 0 then
                    Ok ( { s2 | endOfStreamReached = True, tailBytes = bytesInBuffer }, bytesInBuffer + 1 )

                else
                    doReadMoreInputHelp (bytesInBuffer + len) s2

    else
        Ok ( s, bytesInBuffer )


{-| TODO write tests
<https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/copyWithin>
-}
copyWithin : Int -> Int -> Int -> Array a -> Array a
copyWithin destination sourceStart sourceEnd arr =
    {- If the source and target area don't overlap, we can slice it out and append it back in. That is faster.

    -}
    if destination >= sourceStart && destination < sourceEnd then
        setArraySlice (Array.slice sourceStart sourceEnd arr) destination arr

    else if sourceStart == sourceEnd then
        arr

    else
        let
            newArray =
                case Array.get sourceStart arr of
                    Nothing ->
                        arr

                    Just v ->
                        Array.set destination v arr
        in
        copyWithin (destination + 1) (sourceStart + 1) sourceEnd newArray


overflow8 v =
    if v > 2 ^ 7 then
        v - 2 ^ 8

    else
        v


overflow16 v =
    if v > 2 ^ 15 then
        v - 2 ^ 16

    else
        v


bytesToNibbles : Int -> { state | shortBuffer : Array Int, byteBuffer : Array Int } -> { state | shortBuffer : Array Int, byteBuffer : Array Int }
bytesToNibbles byteLen s =
    let
        halfLen =
            Bitwise.shiftRightBy 1 byteLen

        go i shortBuffer =
            if i < halfLen then
                let
                    byte1 =
                        unsafeGet (i * 2) s.byteBuffer

                    byte2 =
                        unsafeGet (i * 2 + 1) s.byteBuffer

                    value =
                        Bitwise.or
                            (Bitwise.and 0xFF byte1)
                            (Bitwise.and 0xFF byte2
                                |> Bitwise.shiftLeftBy 8
                            )
                            |> overflow16
                in
                go (i + 1) (Array.set i value shortBuffer)

            else
                shortBuffer
    in
    { s | shortBuffer = go 0 s.shortBuffer }


readInput :
    Int
    -> Int
    -> { state | input : InputStream, byteBuffer : Array Int }
    -> Result Error ( { state | input : InputStream, byteBuffer : Array Int }, Int )
readInput offset length s =
    let
        end =
            min (s.input.offset + length) (Bytes.width s.input.buffer)

        bytesRead =
            end - s.input.offset

        decoder =
            Decode.map2 (\_ v -> v) (Decode.bytes s.input.offset) (array bytesRead Decode.signedInt8)
    in
    case Decode.decode decoder s.input.buffer of
        Just newSegment ->
            let
                oldInput =
                    s.input

                newInput =
                    { oldInput | offset = oldInput.offset + bytesRead }

                newByteBuffer =
                    setArraySlice newSegment offset s.byteBuffer
            in
            Ok ( { s | input = newInput, byteBuffer = newByteBuffer }, bytesRead )

        Nothing ->
            Err "readInput: insufficient input"


readInputToArray :
    Int
    -> Int
    -> Array Int
    -> { state | input : InputStream }
    -> Result Error ( { state | input : InputStream }, Array Int, Int )
readInputToArray offset length data s =
    let
        end =
            min (s.input.offset + length) (Bytes.width s.input.buffer)

        bytesRead =
            end - s.input.offset

        decoder =
            Decode.map2 (\_ v -> v) (Decode.bytes offset) (array bytesRead Decode.unsignedInt8)
    in
    case Decode.decode decoder s.input.buffer of
        Just newSegment ->
            let
                oldInput =
                    s.input

                newInput =
                    { oldInput | offset = oldInput.offset + bytesRead }
            in
            Ok ( { s | input = newInput }, Array.append (Array.slice 0 offset data) newSegment, bytesRead )

        Nothing ->
            Err "readInput: insufficient input"


array : Int -> Decode.Decoder a -> Decode.Decoder (Array a)
array tableCount decoder =
    let
        helper ( n, xs ) =
            if n <= 0 then
                Decode.succeed (Done xs)

            else
                Decode.map (\x -> Loop ( n - 1, Array.push x xs )) decoder
    in
    Decode.loop ( tableCount, Array.empty ) helper


outputLength : Int
outputLength =
    16384


decode : Bytes -> Result Error Bytes
decode input =
    case initState (defaultState input) of
        Err e ->
            Err e

        Ok s ->
            let
                decodeLoop state chunks =
                    case decompress { state | written = { toOutput = 0, fromRingBuffer = state.written.fromRingBuffer }, output = Array.empty } of
                        Err e ->
                            Err e

                        Ok ( buffer, newState ) ->
                            if Bytes.width buffer < 16384 then
                                Ok (List.reverse (Encode.bytes buffer :: chunks))

                            else
                                decodeLoop newState (Encode.bytes buffer :: chunks)
            in
            case decodeLoop s [] of
                Err e ->
                    Err e

                Ok v ->
                    v
                        |> Encode.sequence
                        |> Encode.encode
                        |> Ok


updateAccumulator : Int -> Int -> Int -> State -> State
updateAccumulator newBitOffset newHalfOffset newAccumulator32 orig =
    { ringBuffer = orig.ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , output = orig.output
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , literalTreeGroup = orig.literalTreeGroup
    , commandTreeGroup = orig.commandTreeGroup
    , distanceTreeGroup = orig.distanceTreeGroup
    , distOffset = orig.distOffset
    , runningState = orig.runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = newAccumulator32
    , bitOffset = newBitOffset
    , halfOffset = newHalfOffset
    , tailBytes = orig.tailBytes
    , endOfStreamReached = orig.endOfStreamReached
    , metaBlockLength = orig.metaBlockLength
    , inputEnd = orig.inputEnd
    , isUncompressed = orig.isUncompressed
    , isMetadata = orig.isMetadata
    , literalBlockLength = orig.literalBlockLength
    , numLiteralBlockTypes = orig.numLiteralBlockTypes
    , commandBlockLength = orig.commandBlockLength
    , numCommandBlockTypes = orig.numCommandBlockTypes
    , distanceBlockLength = orig.distanceBlockLength
    , numDistanceBlockTypes = orig.numDistanceBlockTypes
    , pos = orig.pos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , trivialLiteralContext = orig.trivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = orig.j
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookupOffset1 = orig.contextLookupOffset1
    , contextLookupOffset2 = orig.contextLookupOffset2
    , distanceCode = orig.distanceCode
    , numDirectDistanceCodes = orig.numDirectDistanceCodes
    , distancePostfixMask = orig.distancePostfixMask
    , distancePostfixBits = orig.distancePostfixBits
    , distance = orig.distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , isEager = orig.isEager
    , isLargeWindow = orig.isLargeWindow
    , input = orig.input
    , written = orig.written
    }


updateBitOffset : Int -> State -> State
updateBitOffset newBitOffset orig =
    { ringBuffer = orig.ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , output = orig.output
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , literalTreeGroup = orig.literalTreeGroup
    , commandTreeGroup = orig.commandTreeGroup
    , distanceTreeGroup = orig.distanceTreeGroup
    , distOffset = orig.distOffset
    , runningState = orig.runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = newBitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , endOfStreamReached = orig.endOfStreamReached
    , metaBlockLength = orig.metaBlockLength
    , inputEnd = orig.inputEnd
    , isUncompressed = orig.isUncompressed
    , isMetadata = orig.isMetadata
    , literalBlockLength = orig.literalBlockLength
    , numLiteralBlockTypes = orig.numLiteralBlockTypes
    , commandBlockLength = orig.commandBlockLength
    , numCommandBlockTypes = orig.numCommandBlockTypes
    , distanceBlockLength = orig.distanceBlockLength
    , numDistanceBlockTypes = orig.numDistanceBlockTypes
    , pos = orig.pos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , trivialLiteralContext = orig.trivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = orig.j
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookupOffset1 = orig.contextLookupOffset1
    , contextLookupOffset2 = orig.contextLookupOffset2
    , distanceCode = orig.distanceCode
    , numDirectDistanceCodes = orig.numDirectDistanceCodes
    , distancePostfixMask = orig.distancePostfixMask
    , distancePostfixBits = orig.distancePostfixBits
    , distance = orig.distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , written = orig.written
    , isEager = orig.isEager
    , isLargeWindow = orig.isLargeWindow
    , input = orig.input
    }


copyState7 : Int -> Int -> Array Int -> Int -> State -> State
copyState7 newPos newJ newRingBuffer literalBlockLength orig =
    { ringBuffer = newRingBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , output = orig.output
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , literalTreeGroup = orig.literalTreeGroup
    , commandTreeGroup = orig.commandTreeGroup
    , distanceTreeGroup = orig.distanceTreeGroup
    , distOffset = orig.distOffset
    , runningState = orig.runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , endOfStreamReached = orig.endOfStreamReached
    , metaBlockLength = orig.metaBlockLength
    , inputEnd = orig.inputEnd
    , isUncompressed = orig.isUncompressed
    , isMetadata = orig.isMetadata
    , literalBlockLength = literalBlockLength
    , numLiteralBlockTypes = orig.numLiteralBlockTypes
    , commandBlockLength = orig.commandBlockLength
    , numCommandBlockTypes = orig.numCommandBlockTypes
    , distanceBlockLength = orig.distanceBlockLength
    , numDistanceBlockTypes = orig.numDistanceBlockTypes
    , pos = newPos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , trivialLiteralContext = orig.trivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = newJ
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookupOffset1 = orig.contextLookupOffset1
    , contextLookupOffset2 = orig.contextLookupOffset2
    , distanceCode = orig.distanceCode
    , numDirectDistanceCodes = orig.numDirectDistanceCodes
    , distancePostfixMask = orig.distancePostfixMask
    , distancePostfixBits = orig.distancePostfixBits
    , distance = orig.distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , written = orig.written
    , isEager = orig.isEager
    , isLargeWindow = orig.isLargeWindow
    , input = orig.input
    }


updateRemainder7 : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Array Int -> State -> State
updateRemainder7 distance maxDistance distanceBlockLength metaBlockLength j runningState distRbIdx rings orig =
    { ringBuffer = orig.ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , output = orig.output
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = rings
    , blockTrees = orig.blockTrees
    , literalTreeGroup = orig.literalTreeGroup
    , commandTreeGroup = orig.commandTreeGroup
    , distanceTreeGroup = orig.distanceTreeGroup
    , distOffset = orig.distOffset
    , runningState = runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , endOfStreamReached = orig.endOfStreamReached
    , metaBlockLength = metaBlockLength
    , inputEnd = orig.inputEnd
    , isUncompressed = orig.isUncompressed
    , isMetadata = orig.isMetadata
    , literalBlockLength = orig.literalBlockLength
    , numLiteralBlockTypes = orig.numLiteralBlockTypes
    , commandBlockLength = orig.commandBlockLength
    , numCommandBlockTypes = orig.numCommandBlockTypes
    , distanceBlockLength = distanceBlockLength
    , numDistanceBlockTypes = orig.numDistanceBlockTypes
    , pos = orig.pos
    , maxDistance = maxDistance
    , distRbIdx = distRbIdx
    , trivialLiteralContext = orig.trivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = j
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookupOffset1 = orig.contextLookupOffset1
    , contextLookupOffset2 = orig.contextLookupOffset2
    , distanceCode = orig.distanceCode
    , numDirectDistanceCodes = orig.numDirectDistanceCodes
    , distancePostfixMask = orig.distancePostfixMask
    , distancePostfixBits = orig.distancePostfixBits
    , distance = distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , written = orig.written
    , isEager = orig.isEager
    , isLargeWindow = orig.isLargeWindow
    , input = orig.input
    }


updateEvaluateState8 : Array Int -> Int -> Int -> Int -> Int -> State -> State
updateEvaluateState8 ringBuffer j metaBlockLength pos runningState orig =
    { ringBuffer = ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , output = orig.output
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , literalTreeGroup = orig.literalTreeGroup
    , commandTreeGroup = orig.commandTreeGroup
    , distanceTreeGroup = orig.distanceTreeGroup
    , distOffset = orig.distOffset
    , runningState = runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , endOfStreamReached = orig.endOfStreamReached
    , metaBlockLength = metaBlockLength
    , inputEnd = orig.inputEnd
    , isUncompressed = orig.isUncompressed
    , isMetadata = orig.isMetadata
    , literalBlockLength = orig.literalBlockLength
    , numLiteralBlockTypes = orig.numLiteralBlockTypes
    , commandBlockLength = orig.commandBlockLength
    , numCommandBlockTypes = orig.numCommandBlockTypes
    , distanceBlockLength = orig.distanceBlockLength
    , numDistanceBlockTypes = orig.numDistanceBlockTypes
    , pos = pos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , trivialLiteralContext = orig.trivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = j
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookupOffset1 = orig.contextLookupOffset1
    , contextLookupOffset2 = orig.contextLookupOffset2
    , distanceCode = orig.distanceCode
    , numDirectDistanceCodes = orig.numDirectDistanceCodes
    , distancePostfixMask = orig.distancePostfixMask
    , distancePostfixBits = orig.distancePostfixBits
    , distance = orig.distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , written = orig.written
    , isEager = orig.isEager
    , isLargeWindow = orig.isLargeWindow
    , input = orig.input
    }


updateEvaluateState4 : Int -> Int -> Int -> Int -> Int -> Int -> State -> State
updateEvaluateState4 j runningState copyLength insertLength distanceCode commandBlockLength orig =
    { ringBuffer = orig.ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , output = orig.output
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , literalTreeGroup = orig.literalTreeGroup
    , commandTreeGroup = orig.commandTreeGroup
    , distanceTreeGroup = orig.distanceTreeGroup
    , distOffset = orig.distOffset
    , runningState = runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , endOfStreamReached = orig.endOfStreamReached
    , metaBlockLength = orig.metaBlockLength
    , inputEnd = orig.inputEnd
    , isUncompressed = orig.isUncompressed
    , isMetadata = orig.isMetadata
    , literalBlockLength = orig.literalBlockLength
    , numLiteralBlockTypes = orig.numLiteralBlockTypes
    , commandBlockLength = commandBlockLength
    , numCommandBlockTypes = orig.numCommandBlockTypes
    , distanceBlockLength = orig.distanceBlockLength
    , numDistanceBlockTypes = orig.numDistanceBlockTypes
    , pos = orig.pos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , trivialLiteralContext = orig.trivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = j
    , insertLength = insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookupOffset1 = orig.contextLookupOffset1
    , contextLookupOffset2 = orig.contextLookupOffset2
    , distanceCode = distanceCode
    , numDirectDistanceCodes = orig.numDirectDistanceCodes
    , distancePostfixMask = orig.distancePostfixMask
    , distancePostfixBits = orig.distancePostfixBits
    , distance = orig.distance
    , copyLength = copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , written = orig.written
    , isEager = orig.isEager
    , isLargeWindow = orig.isLargeWindow
    , input = orig.input
    }


updateRunningState : Int -> State -> State
updateRunningState runningState orig =
    { ringBuffer = orig.ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , output = orig.output
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , literalTreeGroup = orig.literalTreeGroup
    , commandTreeGroup = orig.commandTreeGroup
    , distanceTreeGroup = orig.distanceTreeGroup
    , distOffset = orig.distOffset
    , runningState = runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , endOfStreamReached = orig.endOfStreamReached
    , metaBlockLength = orig.metaBlockLength
    , inputEnd = orig.inputEnd
    , isUncompressed = orig.isUncompressed
    , isMetadata = orig.isMetadata
    , literalBlockLength = orig.literalBlockLength
    , numLiteralBlockTypes = orig.numLiteralBlockTypes
    , commandBlockLength = orig.commandBlockLength
    , numCommandBlockTypes = orig.numCommandBlockTypes
    , distanceBlockLength = orig.distanceBlockLength
    , numDistanceBlockTypes = orig.numDistanceBlockTypes
    , pos = orig.pos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , trivialLiteralContext = orig.trivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = orig.j
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookupOffset1 = orig.contextLookupOffset1
    , contextLookupOffset2 = orig.contextLookupOffset2
    , distanceCode = orig.distanceCode
    , numDirectDistanceCodes = orig.numDirectDistanceCodes
    , distancePostfixMask = orig.distancePostfixMask
    , distancePostfixBits = orig.distancePostfixBits
    , distance = orig.distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , written = orig.written
    , isEager = orig.isEager
    , isLargeWindow = orig.isLargeWindow
    , input = orig.input
    }


updateEvaluateState9 : Int -> Int -> Array Int -> Int -> Int -> State -> State
updateEvaluateState9 nextRunningState runningState ringBuffer pos metaBlockLength orig =
    { ringBuffer = ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , output = orig.output
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , literalTreeGroup = orig.literalTreeGroup
    , commandTreeGroup = orig.commandTreeGroup
    , distanceTreeGroup = orig.distanceTreeGroup
    , distOffset = orig.distOffset
    , runningState = runningState
    , nextRunningState = nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , endOfStreamReached = orig.endOfStreamReached
    , metaBlockLength = metaBlockLength
    , inputEnd = orig.inputEnd
    , isUncompressed = orig.isUncompressed
    , isMetadata = orig.isMetadata
    , literalBlockLength = orig.literalBlockLength
    , numLiteralBlockTypes = orig.numLiteralBlockTypes
    , commandBlockLength = orig.commandBlockLength
    , numCommandBlockTypes = orig.numCommandBlockTypes
    , distanceBlockLength = orig.distanceBlockLength
    , numDistanceBlockTypes = orig.numDistanceBlockTypes
    , pos = pos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , trivialLiteralContext = orig.trivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = orig.j
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookupOffset1 = orig.contextLookupOffset1
    , contextLookupOffset2 = orig.contextLookupOffset2
    , distanceCode = orig.distanceCode
    , numDirectDistanceCodes = orig.numDirectDistanceCodes
    , distancePostfixMask = orig.distancePostfixMask
    , distancePostfixBits = orig.distancePostfixBits
    , distance = orig.distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , written = orig.written
    , isEager = orig.isEager
    , isLargeWindow = orig.isLargeWindow
    , input = orig.input
    }


{-| Just copies all the fields, intended as a template for custom update functions that circumvent the elm record update syntax
-}
copyState : State -> State
copyState orig =
    { ringBuffer = orig.ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , output = orig.output
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , literalTreeGroup = orig.literalTreeGroup
    , commandTreeGroup = orig.commandTreeGroup
    , distanceTreeGroup = orig.distanceTreeGroup
    , distOffset = orig.distOffset
    , runningState = orig.runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , endOfStreamReached = orig.endOfStreamReached
    , metaBlockLength = orig.metaBlockLength
    , inputEnd = orig.inputEnd
    , isUncompressed = orig.isUncompressed
    , isMetadata = orig.isMetadata
    , literalBlockLength = orig.literalBlockLength
    , numLiteralBlockTypes = orig.numLiteralBlockTypes
    , commandBlockLength = orig.commandBlockLength
    , numCommandBlockTypes = orig.numCommandBlockTypes
    , distanceBlockLength = orig.distanceBlockLength
    , numDistanceBlockTypes = orig.numDistanceBlockTypes
    , pos = orig.pos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , trivialLiteralContext = orig.trivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = orig.j
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookupOffset1 = orig.contextLookupOffset1
    , contextLookupOffset2 = orig.contextLookupOffset2
    , distanceCode = orig.distanceCode
    , numDirectDistanceCodes = orig.numDirectDistanceCodes
    , distancePostfixMask = orig.distancePostfixMask
    , distancePostfixBits = orig.distancePostfixBits
    , distance = orig.distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , written = orig.written
    , isEager = orig.isEager
    , isLargeWindow = orig.isLargeWindow
    , input = orig.input
    }
