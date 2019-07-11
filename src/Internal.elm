module Internal exposing (Error(..), buildHuffmanTable, calculateDistanceAlphabetSize, calculateDistanceLut, decode, decompress, encodeByteList, generateCount, generateOffsets, nextTableBitSize, phase1, readComplexHuffmanCodeHelp, readFewBits, sortSymbols, topUpAccumulator)

import Array exposing (Array)
import Array.Helpers
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Step(..))
import Bytes.Encode as Encode
import Constants
import Transforms


type Error
    = MaxDistanceTooSmall { actual : Int, minimal : Int }
    | StateNotInitialized
    | ReadAfterEnd
    | UnusedByteAfterEnd
    | ExuberantNibble
    | ExuberantByte
    | CorruptedReservedBit
    | CorruptedHuffmanHistogram
    | CustomError String
    | UnusedSpace
    | NoHuffmanCode Int
    | CorruptedContextMap
    | InvalidWindowBits
    | InvalidMetablockLength
    | InvalidBackwardReference String
    | UnalignedCopyBytes
    | CorruptedPaddingBits


type alias TreeGroup =
    { literalTreeGroup : Array Int
    , commandTreeGroup : Array Int
    , distanceTreeGroup : Array Int
    }


type alias Num =
    { numLiteralBlockTypes : Int
    , numCommandBlockTypes : Int
    , numDistanceBlockTypes : Int
    }


type alias BlockLength =
    { literalBlockLength : Int
    , commandBlockLength : Int
    , distanceBlockLength : Int
    }


type alias Distance =
    { distancePostfixBits : Int
    , numDirectDistanceCodes : Int
    }


type alias State =
    { ringBuffer : Array Int
    , contextModes : Array Int
    , contextMap : Array Int
    , distContextMap : Array Int
    , distExtraBits : Array Int
    , byteBuffer : Array Int
    , shortBuffer : Array Int
    , intBuffer : Array Int
    , rings : Array Int
    , blockTrees : Array Int
    , treeGroup : TreeGroup
    , distOffset : Array Int
    , runningState : Int
    , nextRunningState : Int
    , accumulator32 : Int
    , bitOffset : Int
    , halfOffset : Int
    , tailBytes : Int
    , metaBlockLength : Int
    , num : Num
    , blockLength : BlockLength
    , pos : Int
    , distRbIdx : Int
    , isTrivialLiteralContext : Bool
    , literalTreeIdx : Int
    , commandTreeIdx : Int
    , j : Int
    , insertLength : Int
    , contextMapSlice : Int
    , distContextMapSlice : Int
    , contextLookup : ContextLookup
    , distanceCode : Int
    , distanceConstants : Distance
    , distance : Int
    , copyLength : Int
    , maxDistance : Int
    , maxBackwardDistance : Int
    , maxRingBufferSize : Int
    , ringBufferSize : Int
    , expectedTotalSize : Int
    , input : InputStream
    , flags : Flags
    }


type ContextLookup
    = ContextLookup Int


contextLookupOffset1 (ContextLookup v) =
    v


contextLookupOffset2 (ContextLookup v) =
    v + 256


type alias Flags =
    { endOfStreamReached : Bool
    , isUncompressed : Bool
    , isMetadata : Bool
    , isLargeWindow : Bool
    , inputEnd : Bool
    }


defaultState : Bytes -> State
defaultState buffer =
    let
        defaultFlags =
            { endOfStreamReached = False
            , inputEnd = False
            , isUncompressed = False
            , isMetadata = False
            , isLargeWindow = False
            }
    in
    { ringBuffer = Array.empty
    , contextModes = Array.empty
    , contextMap = Array.empty
    , distContextMap = Array.empty
    , distExtraBits = Array.empty
    , byteBuffer = Array.empty
    , shortBuffer = Array.empty
    , intBuffer = Array.empty
    , rings = Array.fromList [ 16, 15, 11, 4, 0, 0, 0, 0, 0, 0 ]
    , blockTrees = Array.empty
    , treeGroup =
        { literalTreeGroup = Array.empty
        , commandTreeGroup = Array.empty
        , distanceTreeGroup = Array.empty
        }
    , distOffset = Array.empty
    , runningState = 0
    , nextRunningState = 0
    , accumulator32 = 0
    , bitOffset = 0
    , halfOffset = 0
    , tailBytes = 0
    , metaBlockLength = 0
    , num =
        { numLiteralBlockTypes = 0
        , numCommandBlockTypes = 0
        , numDistanceBlockTypes = 0
        }
    , blockLength =
        { literalBlockLength = 0
        , commandBlockLength = 0
        , distanceBlockLength = 0
        }
    , pos = 0
    , maxDistance = 0
    , distRbIdx = 0
    , isTrivialLiteralContext = False
    , literalTreeIdx = 0
    , commandTreeIdx = 0
    , j = 0
    , insertLength = 0
    , contextMapSlice = 0
    , distContextMapSlice = 0
    , contextLookup = ContextLookup 0
    , distanceCode = 0
    , distanceConstants =
        { numDirectDistanceCodes = 0
        , distancePostfixBits = 0
        }
    , distance = 0
    , copyLength = 0
    , maxBackwardDistance = 0
    , maxRingBufferSize = 0
    , ringBufferSize = 0
    , expectedTotalSize = 0
    , input = { offset = 0, buffer = buffer }
    , flags = defaultFlags
    }


initState : State -> Result Error State
initState s =
    if s.runningState /= 0 then
        Err StateNotInitialized

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


calculateDistanceAlphabetLimit : Int -> Int -> Int -> Result Error Int
calculateDistanceAlphabetLimit maxDistance npostfix ndirect =
    if maxDistance < ndirect + Bitwise.shiftLeftBy npostfix 2 then
        Err (MaxDistanceTooSmall { actual = maxDistance, minimal = ndirect + Bitwise.shiftLeftBy npostfix 2 })

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


log2floor : Int -> Int
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
    let
        oldFlags =
            s.flags

        newFlags =
            { oldFlags | endOfStreamReached = False }
    in
    { s
        | byteBuffer = Array.repeat 4160 0
        , accumulator32 = 0
        , shortBuffer = Array.repeat 2080 0
        , bitOffset = 32
        , halfOffset = 2048
        , flags = newFlags
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


reload : State -> Result Error State
reload s =
    if s.bitOffset == 32 then
        prepare s

    else
        Ok s


checkHealth : Bool -> State -> Result Error State
checkHealth endOfStream s =
    if not s.flags.endOfStreamReached then
        Ok s

    else
        let
            byteOffset =
                Bitwise.shiftLeftBy 1 s.halfOffset + Bitwise.shiftRightBy 3 (s.bitOffset + 7) - 4
        in
        if byteOffset > s.tailBytes then
            Err ReadAfterEnd

        else if endOfStream && (byteOffset /= s.tailBytes) then
            Err UnusedByteAfterEnd

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


readNextMetablockHeader : State -> Result Error State
readNextMetablockHeader s =
    if s.flags.inputEnd then
        Ok
            { s
                | nextRunningState = 10
                , runningState = 13
            }

    else
        let
            s2 =
                { s
                    | treeGroup =
                        { literalTreeGroup = Array.empty
                        , commandTreeGroup = Array.empty
                        , distanceTreeGroup = Array.empty
                        }
                }
        in
        maybeReadMoreInput 2030 s2
            |> Result.andThen decodeMetaBlockLength
            |> Result.andThen
                (\s4 ->
                    if s4.metaBlockLength == 0 && s4.flags.isMetadata == False then
                        Ok s4

                    else
                        let
                            findNextState =
                                if s4.flags.isUncompressed || s4.flags.isMetadata then
                                    case jumpToByteBoundary s4 of
                                        Err e ->
                                            Err e

                                        Ok s5 ->
                                            Ok
                                                ( s5
                                                , if s5.flags.isMetadata then
                                                    5

                                                  else
                                                    6
                                                )

                                else
                                    Ok ( s4, 3 )
                        in
                        case findNextState of
                            Err e ->
                                Err e

                            Ok ( s6, nextRunningState ) ->
                                if s6.flags.isMetadata then
                                    Ok (updateRunningState nextRunningState s6)

                                else
                                    let
                                        newExpectedTotalSize =
                                            min (s6.expectedTotalSize + s6.metaBlockLength) (Bitwise.shiftLeftBy 30 1)
                                    in
                                    if s6.ringBufferSize < s6.maxRingBufferSize then
                                        case maybeReallocateRingBuffer newExpectedTotalSize s6 of
                                            Nothing ->
                                                Ok { s6 | expectedTotalSize = newExpectedTotalSize, runningState = nextRunningState }

                                            Just { ringBuffer, ringBufferSize } ->
                                                Ok { s6 | ringBuffer = ringBuffer, ringBufferSize = ringBufferSize, expectedTotalSize = newExpectedTotalSize, runningState = nextRunningState }

                                    else
                                        Ok { s6 | expectedTotalSize = newExpectedTotalSize, runningState = nextRunningState }
                )


maybeReallocateRingBuffer : Int -> { state | flags : { flags | inputEnd : Bool }, ringBuffer : Array Int, ringBufferSize : Int, maxRingBufferSize : Int } -> Maybe { ringBuffer : Array Int, ringBufferSize : Int }
maybeReallocateRingBuffer expectedTotalSize s =
    let
        newSize =
            let
                initialSize =
                    s.maxRingBufferSize
            in
            if initialSize > expectedTotalSize then
                let
                    minimalNewSize =
                        expectedTotalSize

                    calculate1 size =
                        if Bitwise.shiftRightBy 1 size > minimalNewSize then
                            calculate1 (Bitwise.shiftRightBy 1 size)

                        else
                            size

                    calculate2 size =
                        if s.flags.inputEnd == False && size < 16384 && s.maxRingBufferSize >= 16384 then
                            16384

                        else
                            size
                in
                initialSize |> calculate1 |> calculate2

            else
                initialSize
    in
    if newSize < s.ringBufferSize then
        Nothing

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
        Just { ringBuffer = newBuffer, ringBufferSize = newSize }


decodeMetaBlockBytes : State -> Result Error State
decodeMetaBlockBytes s_ =
    let
        byteLoop i sizeBytes state_ =
            if i < sizeBytes then
                case readFewBitsSafe 8 state_ of
                    ( s2, bits ) ->
                        if bits == 0 && i + 1 == sizeBytes && sizeBytes > 1 then
                            Err ExuberantByte

                        else
                            -- @optimize remove this record update
                            byteLoop (i + 1) sizeBytes { s2 | metaBlockLength = Bitwise.or s2.metaBlockLength (Bitwise.shiftLeftBy (i * 8) bits) }

            else
                Ok state_

        oldFlags =
            s_.flags

        newFlags =
            { oldFlags | isMetadata = True }

        s =
            { s_ | flags = newFlags }
    in
    case readFewBits 1 s of
        ( s1, reserved ) ->
            if reserved /= 0 then
                Err CorruptedReservedBit

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
                            Err ExuberantNibble

                        else
                            -- @optimize remove record update
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
                    let
                        oldFlags =
                            s3.flags

                        newFlags =
                            { oldFlags
                                | inputEnd =
                                    if inputEnd == 0 then
                                        False

                                    else
                                        True
                                , isUncompressed = False
                                , isMetadata = False
                            }
                    in
                    { s3
                        | flags = newFlags
                        , metaBlockLength = 0
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
                                    if s6.flags.inputEnd == False then
                                        case readFewBits 1 s6 of
                                            ( s8, uncompressed ) ->
                                                let
                                                    oldFlags =
                                                        s8.flags

                                                    newFlags =
                                                        { oldFlags | isUncompressed = uncompressed == 1 }
                                                in
                                                Ok { s8 | flags = newFlags, metaBlockLength = s6.metaBlockLength + 1 }

                                    else
                                        Ok { s6 | metaBlockLength = s6.metaBlockLength + 1 }
            in
            if s.flags.inputEnd then
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
            initial.flags.isLargeWindow

        newState =
            let
                ( s1, v ) =
                    readFewBitsSafe 1 initial

                oldFlags =
                    s1.flags

                newFlags =
                    { oldFlags | isLargeWindow = False }

                s2 =
                    { s1 | flags = newFlags }
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
                                    oldFlags2 =
                                        s3.flags

                                    newFlags2 =
                                        { oldFlags2 | isLargeWindow = True }

                                    s4 =
                                        { s3 | flags = newFlags2 }

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


readMetablockPartition : Int -> Int -> State -> Result Error ( State, Int )
readMetablockPartition treeType numBlockTypes s =
    let
        offset =
            Array.Helpers.unsafeGet (2 * treeType) s.blockTrees
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
        ( newBitOffset, sym ) =
            readSymbolPure tableGroup tableIdx s
    in
    ( updateBitOffset newBitOffset s, sym )


readSymbolPure : Array Int -> Int -> State -> ( Int, Int )
readSymbolPure tableGroup tableIdx s =
    let
        readTableGroup index =
            Array.Helpers.unsafeGet index tableGroup

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
        ( s.bitOffset + bits, sym )

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
        ( newBitOffset, result )


readHuffmanCode : Int -> Int -> Array Int -> Int -> State -> Result Error ( State, { tableGroup : Array Int, total_size : Int } )
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
                Array.Helpers.unsafeGet i Constants.code_length_code_order

            s1 =
                topUpAccumulator s

            p =
                Bitwise.shiftRightZfBy s1.bitOffset s1.accumulator32
                    |> Bitwise.and 15

            s2 =
                updateBitOffset (s1.bitOffset + Bitwise.shiftRightBy 16 (Array.Helpers.unsafeGet p Constants.fixed_table)) s1

            v =
                Array.Helpers.unsafeGet p Constants.fixed_table
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
                    -- Err ("Corrupted Huffman code histogram: space =" ++ String.fromInt space ++ " (should be 0), and numCodes=" ++ String.fromInt numCodes ++ " (should be 1)!")
                    Err CorruptedHuffmanHistogram

                else
                    case readHuffmanCodeLengths newCodelengthCodelengths alphabetSizeLimit codeLengths s of
                        Err e ->
                            Err e

                        Ok ( s1, newCodeLengths ) ->
                            Ok ( s1, buildHuffmanTable tableGroup tableIdx 8 newCodeLengths )
           )


readHuffmanCodeLengths : Array Int -> Int -> Array Int -> State -> Result Error ( State, Array Int )
readHuffmanCodeLengths codeLengthCodeLengths numSymbols initialCodeLengths state =
    let
        -- @optimize could use Array.push instead of Array.set, as elements are created order
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
                                updateBitOffset (s1.bitOffset + Bitwise.shiftRightBy 16 (Array.Helpers.unsafeGet p table)) s1

                            codeLen =
                                Array.Helpers.unsafeGet p table
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
                                        Err (CustomError "symbol + repeatDelta > numSymbols")

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
                -- Err ("unused space! the space is " ++ String.fromInt newSpace ++ ", but the symbol is " ++ String.fromInt newSymbol)
                Err UnusedSpace

            else
                Ok ( s1, Array.Helpers.fill 0 newSymbol numSymbols codeLengths )


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
                            -- there is no huffman code for this symbol
                            Err (NoHuffmanCode symbol)

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

                Ok ( s2, symbols ) ->
                    if Array.Helpers.hasDuplicates symbols then
                        Err (CustomError "the symbols contain duplicate elements")

                    else
                        case readHistogramId numSymbols s2 of
                            ( s3, histogramId ) ->
                                let
                                    readSymbols index =
                                        Array.Helpers.unsafeGet index symbols

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
                        go (symbol + 1) (Array.Helpers.update idx (\x -> x + 1) acc)

            else
                acc
    in
    go 0 (Array.repeat 16 0)


generateOffsets : Array Int -> Array Int
generateOffsets count =
    let
        go len offset =
            if len < Constants.max_length then
                let
                    newValue =
                        Array.Helpers.unsafeGet len offset + Array.Helpers.unsafeGet len count
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
                case Array.Helpers.unsafeGet symbol codeLengths of
                    0 ->
                        go (symbol + 1) sorted offset

                    idx ->
                        let
                            idx2 =
                                Array.Helpers.unsafeGet idx offset
                        in
                        go (symbol + 1) (Array.set idx2 symbol sorted) (Array.Helpers.update idx (\x -> x + 1) offset)

            else
                { offset = offset, sorted = sorted }
    in
    go 0 (Array.repeat init.codeLengthsSize 0) init.offset


phase1 : Int -> Int -> Int -> Array Int -> Array Int -> Int -> Int -> Int -> Int -> Array Int -> ( ( Array Int, Int ), Int, Array Int )
phase1 rootBits tableOffset tableSize sorted =
    let
        loop1 : Array Int -> Int -> Int -> Int -> Int -> Array Int -> ( ( Array Int, Int ), Int, Array Int )
        loop1 currentCount len key symbol step currentTableGroup =
            if len <= rootBits then
                case loop2 currentCount len key symbol step (Array.Helpers.unsafeGet len currentCount) currentTableGroup of
                    ( ( newCount, newKey ), newSymbol, newTableGroup ) ->
                        loop1 newCount (len + 1) newKey newSymbol (Bitwise.shiftLeftBy 1 step) newTableGroup

            else
                ( ( currentCount, key ), symbol, currentTableGroup )

        loop2 currentCount len key symbol step iterationsRemaining currentTableGroup =
            if Array.Helpers.unsafeGet len currentCount > 0 then
                let
                    newTableGroup =
                        Array.Helpers.replicateValue currentTableGroup (tableOffset + key) step tableSize (Bitwise.or (Bitwise.shiftLeftBy 16 len) (Array.Helpers.unsafeGet symbol sorted))

                    newSymbol =
                        symbol + 1

                    newKey =
                        getNextKey key len
                in
                loop2 (Array.Helpers.update len (\v -> v - 1) currentCount) len newKey newSymbol step (iterationsRemaining - 1) newTableGroup

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
            Array.Helpers.unsafeGet tableIdx tableGroup
    in
    if Array.Helpers.unsafeGet 15 offset == 1 then
        let
            go : Int -> Array Int -> Array Int
            go key arr =
                if key < totalSize then
                    go (key + 1) (Array.set (tableOffset + key) (Array.Helpers.unsafeGet 0 sorted) arr)

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


nextTableBitSize : Array Int -> Int -> Int -> Int
nextTableBitSize count initialLen rootBits =
    let
        initialLeft =
            Bitwise.shiftLeftBy (initialLen - rootBits) 1

        go len left =
            if len < 15 then
                let
                    newLeft =
                        left - Array.Helpers.unsafeGet len count
                in
                if newLeft <= 0 then
                    len - rootBits

                else
                    go (len + 1) (Bitwise.shiftLeftBy 1 newLeft)

            else
                len - rootBits
    in
    go initialLen initialLeft


type alias TableLoopState =
    { count : Array Int
    , currentOffset : Int
    , currentTableGroup : Array Int
    , key : Int
    , len : Int
    , low : Int
    , step : Int
    , symbol : Int
    , tableBits : Int
    , tableSize : Int
    , totalSize : Int
    }


huffmanTableLoop3 : Array Int -> Int -> Int -> Int -> TableLoopState -> { tableGroup : Array Int, total_size : Int }
huffmanTableLoop3 sorted mask tableOffset rootBits state =
    -- @optimize len and step fields?
    if state.len <= 15 then
        let
            newState =
                huffmanTableLoop4 sorted mask tableOffset rootBits state
        in
        huffmanTableLoop3 sorted mask tableOffset rootBits { newState | len = newState.len + 1, step = Bitwise.shiftLeftBy 1 newState.step }

    else
        { tableGroup = state.currentTableGroup, total_size = state.totalSize }


huffmanTableLoop4 : Array Int -> Int -> Int -> Int -> TableLoopState -> TableLoopState
huffmanTableLoop4 sorted mask tableOffset rootBits ({ count, len, key, currentTableGroup, currentOffset, tableSize, totalSize, low } as state) =
    if Array.Helpers.unsafeGet len count > 0 then
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


finalize : Array Int -> Int -> TableLoopState -> TableLoopState
finalize sorted rootBits ({ count, len, key, symbol, step, currentTableGroup, currentOffset, tableSize } as state) =
    { state
        | currentTableGroup =
            Array.Helpers.replicateValue
                currentTableGroup
                (currentOffset + Bitwise.shiftRightBy rootBits key)
                step
                tableSize
                (Bitwise.shiftLeftBy 16 (len - rootBits) |> Bitwise.or (Array.Helpers.unsafeGet symbol sorted))
        , symbol = symbol + 1
        , key = getNextKey key len
        , count = Array.Helpers.update len (\x -> x - 1) count
    }


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
                    Array.Helpers.unsafeGet code Constants.block_length_n_bits

                ( s4, result ) =
                    if n <= 16 then
                        readFewBitsSafe n s2

                    else
                        readManyBits n (topUpAccumulator s2)
            in
            ( s4, Array.Helpers.unsafeGet code Constants.block_length_offset + result )


map2 : (a -> b -> c) -> (State -> Result Error ( State, a )) -> (State -> Result Error ( State, b )) -> (State -> Result Error ( State, c ))
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


{-| @performance there are some record updates here, but only gets called once at the start
-}
readBlocks : State -> Result Error State
readBlocks s0 =
    case decodeVarLenUnsignedByte s0 of
        ( s1, numLiteralBlockTypes ) ->
            -- case readMetablockPartition 0 (numLiteralBlockTypes + 1) { s1 | numLiteralBlockTypes = numLiteralBlockTypes + 1 } of
            case readMetablockPartition 0 (numLiteralBlockTypes + 1) s1 of
                Err e ->
                    Err e

                Ok ( s2, literalBlockLength ) ->
                    case decodeVarLenUnsignedByte s2 of
                        ( s3, numCommandBlockTypes ) ->
                            case readMetablockPartition 1 (numCommandBlockTypes + 1) s3 of
                                Err e ->
                                    Err e

                                Ok ( s4, commandBlockLength ) ->
                                    case decodeVarLenUnsignedByte s4 of
                                        ( s5, numDistanceBlockTypes ) ->
                                            case readMetablockPartition 2 (numDistanceBlockTypes + 1) s5 of
                                                Err e ->
                                                    Err e

                                                Ok ( s6, distanceBlockLength ) ->
                                                    let
                                                        num =
                                                            { numDistanceBlockTypes = numDistanceBlockTypes + 1
                                                            , numCommandBlockTypes = numCommandBlockTypes + 1
                                                            , numLiteralBlockTypes = numLiteralBlockTypes + 1
                                                            }

                                                        blockLength =
                                                            { literalBlockLength = literalBlockLength
                                                            , commandBlockLength = commandBlockLength
                                                            , distanceBlockLength = distanceBlockLength
                                                            }
                                                    in
                                                    Ok { s6 | num = num, blockLength = blockLength }


readMetablockHuffmanCodesAndContextMaps : State -> Result Error State
readMetablockHuffmanCodesAndContextMaps =
    let
        topUp s =
            maybeReadMoreInput 2030 s
                |> Result.map topUpAccumulator

        readContextModes : State -> Result Error ( State, Array Int )
        readContextModes state =
            let
                go1 i s acc =
                    if i < s.num.numLiteralBlockTypes then
                        let
                            limit =
                                min (i + 96) s.num.numLiteralBlockTypes
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
                                | distanceConstants =
                                    { distancePostfixBits = a
                                    , numDirectDistanceCodes = Bitwise.shiftLeftBy a b
                                    }
                            }
                    in
                    go1 0 newerState Array.empty

        readContextMap ( s, contextModes ) =
            let
                s0 =
                    s

                size =
                    Bitwise.shiftLeftBy 6 s.num.numLiteralBlockTypes
            in
            case decodeContextMap size (Array.repeat size 0) s0 of
                Err e ->
                    Err e

                Ok ( s1, contextMap, numLiteralTrees ) ->
                    let
                        isTrivial =
                            let
                                go j =
                                    if j < Bitwise.shiftLeftBy 6 s1.num.numLiteralBlockTypes then
                                        if Array.Helpers.unsafeGet j contextMap /= Bitwise.shiftRightBy 6 j then
                                            False

                                        else
                                            go (j + 1)

                                    else
                                        True
                            in
                            go 0

                        distanceSize =
                            Bitwise.shiftLeftBy 2 s.num.numDistanceBlockTypes
                    in
                    case decodeContextMap distanceSize (Array.repeat distanceSize 0) s1 of
                        Err e ->
                            Err e

                        Ok ( s3, distContextMap, numDistTrees ) ->
                            map2 Tuple.pair
                                (decodeHuffmanTreeGroup 256 256 numLiteralTrees)
                                (decodeHuffmanTreeGroup 704 704 s3.num.numCommandBlockTypes)
                                s3
                                |> Result.andThen
                                    (\( s5, ( literalTreeGroup, commandTreeGroup ) ) ->
                                        let
                                            distanceAlphabet =
                                                if s.flags.isLargeWindow then
                                                    let
                                                        max =
                                                            calculateDistanceAlphabetSize s5.distanceConstants.distancePostfixBits s5.distanceConstants.numDirectDistanceCodes 62
                                                    in
                                                    case calculateDistanceAlphabetLimit 0x7FFFFFFC s5.distanceConstants.distancePostfixBits s5.distanceConstants.numDirectDistanceCodes of
                                                        Ok v ->
                                                            Ok ( max, v )

                                                        Err e ->
                                                            Err e

                                                else
                                                    let
                                                        max =
                                                            calculateDistanceAlphabetSize s5.distanceConstants.distancePostfixBits s5.distanceConstants.numDirectDistanceCodes 24
                                                    in
                                                    Ok ( max, max )
                                        in
                                        case distanceAlphabet of
                                            Err e ->
                                                Err e

                                            Ok ( distanceAlphabetSizeMax, distanceAlphabetSizeLimit ) ->
                                                case decodeHuffmanTreeGroup distanceAlphabetSizeMax distanceAlphabetSizeLimit numDistTrees s5 of
                                                    Err e ->
                                                        Err e

                                                    Ok ( s7, distanceTreeGroup ) ->
                                                        let
                                                            { distExtraBits, distOffset } =
                                                                calculateDistanceLut distanceAlphabetSizeLimit s7

                                                            treeGroup =
                                                                { literalTreeGroup = literalTreeGroup, commandTreeGroup = commandTreeGroup, distanceTreeGroup = distanceTreeGroup }

                                                            s9 =
                                                                { s7
                                                                    | contextMapSlice = 0
                                                                    , treeGroup = treeGroup
                                                                    , distContextMapSlice = 0
                                                                    , contextLookup = ContextLookup (Array.Helpers.unsafeGet 0 contextModes * 512)
                                                                    , literalTreeIdx = 0
                                                                    , commandTreeIdx = 0
                                                                    , contextModes = contextModes
                                                                    , distExtraBits = distExtraBits
                                                                    , distOffset = distOffset
                                                                    , distContextMap = distContextMap
                                                                    , contextMap = contextMap
                                                                    , isTrivialLiteralContext = isTrivial
                                                                    , rings =
                                                                        s7.rings
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
    in
    readBlocks
        >> Result.andThen (\s -> topUp s)
        >> Result.andThen (\s -> readContextModes s)
        >> Result.andThen readContextMap


decodeContextMap : Int -> Array Int -> State -> Result Error ( State, Array Int, Int )
decodeContextMap contextMapSize contextMap s0 =
    let
        readMaxRunLengthPrefix =
            readFewBitsSafe 1
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
            let
                ( s2, v ) =
                    decodeVarLenUnsignedByte s1

                numTrees =
                    v + 1
            in
            if numTrees == 1 then
                Ok ( s2, Array.repeat contextMapSize 0, numTrees )

            else
                case readMaxRunLengthPrefix s2 of
                    ( s3, maxRunLengthPrefix ) ->
                        let
                            alphabetSize =
                                numTrees + maxRunLengthPrefix

                            tableSize =
                                Array.Helpers.unsafeGet (Bitwise.shiftRightBy 5 (alphabetSize + 31)) Constants.max_huffman_table_size

                            tableIdx =
                                (tableSize + 1) - 1
                        in
                        case readHuffmanCode alphabetSize alphabetSize (Array.repeat (tableSize + 1) 0) tableIdx s3 of
                            Err e ->
                                Err e

                            Ok ( s4, table ) ->
                                let
                                    go2 i reps currentContextMap =
                                        if reps /= 0 then
                                            if i >= contextMapSize then
                                                Err CorruptedContextMap

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
                                            case readFewBitsSafe 1 s of
                                                ( newState, 1 ) ->
                                                    Ok ( newState, Array.Helpers.inverseMoveToFrontTransform currentContextMap contextMapSize, numTrees )

                                                ( newState, _ ) ->
                                                    Ok ( newState, currentContextMap, numTrees )
                                in
                                go 0 contextMap s4


decodeHuffmanTreeGroup : Int -> Int -> Int -> State -> Result Error ( State, Array Int )
decodeHuffmanTreeGroup alphabetSizeMax alphabetSizeLimit n =
    let
        maxTableSize =
            Array.get (Bitwise.shiftRightBy 5 (alphabetSizeLimit + 31)) Constants.max_huffman_table_size
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


calculateDistanceLut : Int -> State -> { distExtraBits : Array Int, distOffset : Array Int }
calculateDistanceLut alphabetSizeLimit state =
    let
        npostfix =
            state.distanceConstants.distancePostfixBits

        ndirect =
            state.distanceConstants.numDirectDistanceCodes

        postfix =
            Bitwise.shiftLeftBy npostfix 1

        go1 i j bits base upperLimit distExtraBits distOffset =
            if j < upperLimit then
                go1 (i + 1)
                    (j + 1)
                    bits
                    base
                    upperLimit
                    (Array.set i bits distExtraBits)
                    (Array.set i (j + base) distOffset)

            else
                ( distExtraBits, distOffset, i )

        go2 bits half ( distExtraBits, distOffset, i ) =
            if i < alphabetSizeLimit then
                let
                    base =
                        ndirect + Bitwise.shiftLeftBy npostfix (Bitwise.shiftLeftBy bits (2 + half) - 4) + 1

                    ( newDistExtraBits, newDistOffset, newI ) =
                        go1 i 0 bits base postfix distExtraBits distOffset
                in
                go2 (bits + half) (Bitwise.xor half 1) ( newDistExtraBits, newDistOffset, newI )

            else
                { distExtraBits = distExtraBits, distOffset = distOffset }
    in
    go1 16 0 0 1 ndirect state.distExtraBits state.distOffset
        |> go2 1 0


decompress : Written -> State -> Result Error ( Bytes, State, Written )
decompress written unvalidated =
    if unvalidated.runningState == 0 then
        Err (CustomError "Can't decompreunvalidateds until initialized")

    else if unvalidated.runningState == 11 then
        Err (CustomError "Can't decompress after close")

    else
        let
            s =
                if unvalidated.runningState == 1 then
                    case decodeWindowBits unvalidated of
                        ( newS, windowBits ) ->
                            if windowBits == -1 then
                                Err InvalidWindowBits

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
                    written
                    s2
        in
        case s |> Result.andThen work of
            Err e ->
                Err e

            Ok ( r, _, newWritten ) ->
                Ok
                    ( newWritten.output
                        |> List.reverse
                        |> Encode.sequence
                        |> Encode.encode
                    , r
                    , newWritten
                    )


encodeByteList : List Int -> List Encode.Encoder -> List Encode.Encoder
encodeByteList remaining encoders =
    {- NOTE the values can in practice use more than the first 8 bits, so we have to mask them -}
    case remaining of
        [] ->
            encoders

        [ x ] ->
            Encode.unsignedInt8 x :: encoders

        [ x, y ] ->
            let
                value =
                    Bitwise.or (Bitwise.shiftLeftBy 8 x) (Bitwise.and 0xFF y)
            in
            Encode.unsignedInt16 BE value :: encoders

        [ x, y, z ] ->
            let
                value =
                    Bitwise.or (Bitwise.shiftLeftBy 8 x) (Bitwise.and 0xFF y)
            in
            Encode.unsignedInt8 z :: Encode.unsignedInt16 BE value :: encoders

        x :: y :: z :: w :: rest ->
            let
                value =
                    Bitwise.or
                        (Bitwise.or (Bitwise.shiftLeftBy 24 x) (Bitwise.shiftLeftBy 16 (Bitwise.and 0xFF y)))
                        (Bitwise.or (Bitwise.shiftLeftBy 8 (Bitwise.and 0xFF z)) (Bitwise.and 0xFF w))
            in
            encodeByteList rest (Encode.unsignedInt32 BE value :: encoders)


type alias Context =
    { fence : Int
    , ringBufferMask : Int
    }


type alias Written =
    { toOutput : Int
    , fromRingBuffer : Int
    , output : List Encode.Encoder
    }


decompressHelp : Context -> Written -> State -> Result Error ( State, Context, Written )
decompressHelp context written s =
    let
        _ =
            -- Debug.log "state" ( s.runningState, ( s.pos, s.bitOffset, s.accumulator32 ) )
            ()
    in
    case s.runningState of
        10 ->
            Ok ( s, context, written )

        2 ->
            if s.metaBlockLength < 0 then
                Err InvalidMetablockLength

            else
                case readNextMetablockHeader s of
                    Err e ->
                        Err e

                    Ok s2 ->
                        let
                            fence =
                                calculateFence s2
                        in
                        decompressHelp { fence = fence, ringBufferMask = s2.ringBufferSize - 1 } written s2

        3 ->
            case readMetablockHuffmanCodesAndContextMaps s of
                Err e ->
                    Err e

                Ok s2 ->
                    decompressHelp context written (updateRunningState 4 s2)

        4 ->
            case evaluateState4 s of
                Err e ->
                    Err e

                Ok newState ->
                    decompressHelp context written newState

        7 ->
            if s.isTrivialLiteralContext then
                let
                    go s0 =
                        if s0.j < s0.insertLength then
                            case maybeReadMoreInput 2030 s0 |> Result.map (maybeLiteral >> topUpAccumulator) of
                                Err e ->
                                    Err e

                                Ok s1 ->
                                    case readSymbolPure s1.treeGroup.literalTreeGroup s1.literalTreeIdx s1 of
                                        ( newBitOffset, value ) ->
                                            let
                                                newRingBuffer =
                                                    Array.set s1.pos value s1.ringBuffer

                                                newPos =
                                                    s1.pos + 1
                                            in
                                            if newPos >= context.fence then
                                                let
                                                    oldBlockLengths =
                                                        s1.blockLength

                                                    newBlockLengths =
                                                        { literalBlockLength = s1.blockLength.literalBlockLength - 1
                                                        , commandBlockLength = oldBlockLengths.commandBlockLength
                                                        , distanceBlockLength = oldBlockLengths.distanceBlockLength
                                                        }
                                                in
                                                Ok { s1 | nextRunningState = 7, runningState = 13, bitOffset = newBitOffset, pos = s1.pos + 1, j = s1.j + 1, ringBuffer = newRingBuffer, blockLength = newBlockLengths }

                                            else
                                                go (copyState7 newBitOffset newPos (s1.j + 1) newRingBuffer (s1.blockLength.literalBlockLength - 1) s1)

                        else
                            let
                                oldBlockLengths =
                                    s0.blockLength

                                newBlockLengths =
                                    { literalBlockLength = s0.blockLength.literalBlockLength - 1
                                    , commandBlockLength = oldBlockLengths.commandBlockLength
                                    , distanceBlockLength = oldBlockLengths.distanceBlockLength
                                    }
                            in
                            remainder7 { s0 | blockLength = newBlockLengths }
                in
                case go s of
                    Err e ->
                        Err e

                    Ok newState ->
                        decompressHelp context written newState

            else
                let
                    init_prevByte1 =
                        Array.Helpers.unsafeGet (Bitwise.and (s.pos - 1) context.ringBufferMask) s.ringBuffer
                            |> Bitwise.and 0xFF

                    init_prevByte2 =
                        Array.Helpers.unsafeGet (Bitwise.and (s.pos - 2) context.ringBufferMask) s.ringBuffer
                            |> Bitwise.and 0xFF
                in
                case evaluateState7 context init_prevByte1 init_prevByte2 s of
                    Err e ->
                        Err e

                    Ok newState ->
                        decompressHelp context written newState

        8 ->
            decompressHelp context written (evaluateState8 context s)

        9 ->
            if s.distance > 0x7FFFFFFC then
                Err (InvalidBackwardReference ("distance is " ++ String.fromInt s.distance ++ ", but may not exceed " ++ String.fromInt 0x7FFFFFFC))

            else if s.copyLength >= 4 && s.copyLength <= 24 then
                let
                    offset =
                        Array.Helpers.unsafeGet s.copyLength Constants.dictionary_offsets_by_length + wordIdx * s.copyLength

                    wordId =
                        s.distance - s.maxDistance - 1

                    shift =
                        Array.Helpers.unsafeGet s.copyLength Constants.dictionary_size_bits_by_length

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
                            Transforms.transformDictionaryWord s.ringBuffer s.pos Constants.dictionary_data offset s.copyLength Transforms.rfc_transforms transformIdx

                        newMetaBlockLength =
                            s.metaBlockLength - len

                        newPos =
                            s.pos + len
                    in
                    if newPos >= context.fence then
                        decompressHelp context written (updateEvaluateState9 4 13 newRingBuffer (s.pos + len) newMetaBlockLength s)

                    else
                        decompressHelp context written (updateEvaluateState9 s.nextRunningState 4 newRingBuffer (s.pos + len) newMetaBlockLength s)

                else
                    Err (InvalidBackwardReference ("the transform index is " ++ String.fromInt transformIdx ++ ", but it must be smaller than 121"))

            else
                Err (InvalidBackwardReference ("CopyLength must be 4 >= copyLength <= 24, but it is " ++ String.fromInt s.copyLength))

        6 ->
            copyUncompressedData s
                |> Result.andThen (decompressHelp context written)

        12 ->
            -- should not happen, go directly to state 13
            decompressHelp context written s

        13 ->
            let
                ( wasWritten, newWritten ) =
                    writeRingBuffer (min s.pos s.ringBufferSize) written s
            in
            if not wasWritten then
                Ok ( s, context, newWritten )

            else
                let
                    newMaxDistance =
                        if s.pos >= s.maxBackwardDistance then
                            s.maxBackwardDistance

                        else
                            s.maxDistance
                in
                if s.pos >= s.ringBufferSize then
                    let
                        newRingBuffer =
                            Array.Helpers.copyWithin 0 s.ringBufferSize s.pos s.ringBuffer
                    in
                    decompressHelp context
                        { fromRingBuffer = 0, toOutput = newWritten.toOutput, output = newWritten.output }
                        { s
                            | pos = Bitwise.and s.pos context.ringBufferMask
                            , runningState = s.nextRunningState
                            , ringBuffer = newRingBuffer
                            , maxDistance = newMaxDistance
                        }

                else
                    decompressHelp context
                        newWritten
                        { s
                            | runningState = s.nextRunningState
                            , maxDistance = newMaxDistance
                        }

        _ ->
            Ok ( s, context, written )


evaluateState4 : State -> Result Error State
evaluateState4 s =
    if s.metaBlockLength <= 0 then
        Ok { s | runningState = 2 }

    else
        case maybeReadMoreInput 2030 s of
            Err e ->
                Err e

            Ok s1 ->
                let
                    maybeCommandBlock =
                        if s1.blockLength.commandBlockLength == 0 then
                            decodeCommandBlockSwitch s1

                        else
                            s1
                in
                case maybeCommandBlock |> topUpAccumulator of
                    s2 ->
                        case readSymbol s2.treeGroup.commandTreeGroup s2.commandTreeIdx s2 of
                            ( s3, v ) ->
                                let
                                    cmdCode =
                                        Bitwise.shiftLeftBy 2 v

                                    insertAndCopyExtraBits =
                                        Array.Helpers.unsafeGet (cmdCode + 0) Constants.cmd_lookup

                                    insertLengthOffset =
                                        Array.Helpers.unsafeGet (cmdCode + 1) Constants.cmd_lookup

                                    copyLengthOffset =
                                        Array.Helpers.unsafeGet (cmdCode + 2) Constants.cmd_lookup

                                    readLengths state =
                                        let
                                            extraBits1 =
                                                Bitwise.and insertAndCopyExtraBits 0xFF

                                            extraBits2 =
                                                Bitwise.shiftRightBy 8 insertAndCopyExtraBits
                                        in
                                        readBits2 extraBits1 extraBits2 state

                                    ( s5, insertLengthBits, copyLengthBits ) =
                                        readLengths s3
                                in
                                Ok
                                    (updateEvaluateState4
                                        0
                                        7
                                        (copyLengthBits + copyLengthOffset)
                                        (insertLengthBits + insertLengthOffset)
                                        (Array.Helpers.unsafeGet (cmdCode + 3) Constants.cmd_lookup)
                                        (s3.blockLength.commandBlockLength - 1)
                                        s5
                                    )


maybeLiteral : State -> State
maybeLiteral state =
    if state.blockLength.literalBlockLength == 0 then
        decodeLiteralBlockSwitch state

    else
        state


evaluateState7 : Context -> Int -> Int -> State -> Result Error State
evaluateState7 context prevByte1 prevByte2 s0 =
    if s0.j < s0.insertLength then
        case maybeReadMoreInput 2030 s0 of
            Err e ->
                Err e

            Ok s_ ->
                let
                    s1 =
                        topUpAccumulator (maybeLiteral s_)

                    i1 =
                        contextLookupOffset1 s1.contextLookup + prevByte1

                    i2 =
                        contextLookupOffset2 s1.contextLookup + prevByte2

                    v1 =
                        Array.Helpers.unsafeGet i1 Constants.lookup

                    v2 =
                        Array.Helpers.unsafeGet i2 Constants.lookup

                    literalContext =
                        Bitwise.or v1 v2

                    literalTreeIdx =
                        Array.Helpers.unsafeGet (s1.contextMapSlice + literalContext) s1.contextMap
                            |> Bitwise.and 0xFF

                    s2 =
                        s1

                    byte2 =
                        prevByte1
                in
                case readSymbolPure s2.treeGroup.literalTreeGroup literalTreeIdx s2 of
                    ( newBitOffset, byte1 ) ->
                        let
                            newRingBuffer =
                                Array.set s2.pos byte1 s2.ringBuffer

                            newPos =
                                s2.pos + 1
                        in
                        if newPos >= context.fence then
                            let
                                oldBlockLengths =
                                    s2.blockLength

                                newBlockLengths =
                                    { literalBlockLength = s2.blockLength.literalBlockLength - 1
                                    , commandBlockLength = oldBlockLengths.commandBlockLength
                                    , distanceBlockLength = oldBlockLengths.distanceBlockLength
                                    }
                            in
                            Ok { s2 | nextRunningState = 7, runningState = 13, bitOffset = newBitOffset, pos = newPos, j = s2.j + 1, ringBuffer = newRingBuffer, blockLength = newBlockLengths }

                        else
                            evaluateState7 context byte1 byte2 (copyState7 newBitOffset newPos (s2.j + 1) newRingBuffer (s2.blockLength.literalBlockLength - 1) s2)

    else
        remainder7 s0


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
                if srcEnd > dst && dstEnd > src then
                    -- the source and target areas overlap, so we have to be very careful to put values in place before they are read again
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
                                            Array.set (currentDst + 0) (Array.Helpers.unsafeGet (currentSrc + 0) a0) a0

                                        a2 =
                                            Array.set (currentDst + 1) (Array.Helpers.unsafeGet (currentSrc + 1) a1) a1

                                        a3 =
                                            Array.set (currentDst + 2) (Array.Helpers.unsafeGet (currentSrc + 2) a2) a2

                                        a4 =
                                            Array.set (currentDst + 3) (Array.Helpers.unsafeGet (currentSrc + 3) a3) a3
                                     in
                                     a4
                                    )

                            else
                                accum
                    in
                    go 0 dst src s.ringBuffer

                else if copyLength < 12 then
                    let
                        go k currentDst currentSrc accum =
                            if k < copyLength then
                                go (k + 4)
                                    (currentDst + 4)
                                    (currentSrc + 4)
                                    (accum
                                        |> Array.set (currentDst + 0) (Array.Helpers.unsafeGet (currentSrc + 0) accum)
                                        |> Array.set (currentDst + 1) (Array.Helpers.unsafeGet (currentSrc + 1) accum)
                                        |> Array.set (currentDst + 2) (Array.Helpers.unsafeGet (currentSrc + 2) accum)
                                        |> Array.set (currentDst + 3) (Array.Helpers.unsafeGet (currentSrc + 3) accum)
                                    )

                            else
                                accum
                    in
                    go 0 dst src s.ringBuffer

                else
                    Array.Helpers.copyWithin dst src srcEnd s.ringBuffer
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
                        { ringBuffer = state.ringBuffer |> Array.set state.pos (Array.Helpers.unsafeGet (Bitwise.and (state.pos - distance) context.ringBufferMask) state.ringBuffer)
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


copyBytesToRingBuffer : Int -> Int -> Array Int -> State -> Result Error ( State, Array Int )
copyBytesToRingBuffer offset length data s =
    -- NOTE data is the ringbufffer
    if Bitwise.and s.bitOffset 7 /= 0 then
        Err UnalignedCopyBytes

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
                    , Array.Helpers.setSlice (Array.slice readOffset (readOffset + delta) state.byteBuffer) currentOffset accum
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
                                Err (CustomError "Unexpected end of input")

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


writeRingBuffer : Int -> Written -> State -> ( Bool, Written )
writeRingBuffer ringBufferBytesReady written s =
    let
        toWrite =
            min (Constants.outputLength - written.toOutput) (ringBufferBytesReady - written.fromRingBuffer)

        newerWritten =
            if toWrite /= 0 then
                let
                    slice =
                        Array.slice written.fromRingBuffer (written.fromRingBuffer + toWrite) s.ringBuffer

                    newOutput =
                        Array.Helpers.fasterEncode slice :: written.output

                    -- Array.append written.output slice
                    newWritten =
                        { toOutput = written.toOutput + toWrite, fromRingBuffer = written.fromRingBuffer + toWrite, output = newOutput }
                in
                newWritten

            else
                written
    in
    ( newerWritten.toOutput < Constants.outputLength, newerWritten )


remainder7 : State -> Result Error State
remainder7 s =
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
                                Array.Helpers.unsafeGet s1.distRbIdx s1.rings
                        in
                        Ok
                            { state = s1
                            , distance = newDistance
                            , distanceCode = oldDistanceCode
                            , distanceBlockLength = s1.blockLength.distanceBlockLength
                            , metaBlockLength = newMetaBlockLength
                            }

                    else
                        let
                            maybeDistance state =
                                if state.blockLength.distanceBlockLength == 0 then
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
                                        Bitwise.and 0xFF (Array.Helpers.unsafeGet (s2.distContextMapSlice + oldDistanceCode) s2.distContextMap)
                                in
                                case readSymbol s2.treeGroup.distanceTreeGroup distTreeIdx s2 of
                                    ( s3, distanceCode ) ->
                                        if distanceCode < 16 then
                                            let
                                                index =
                                                    Bitwise.and (s3.distRbIdx + Array.Helpers.unsafeGet distanceCode Constants.distance_short_code_index_offset) 0x03

                                                newDistance =
                                                    Array.Helpers.unsafeGet index s3.rings + Array.Helpers.unsafeGet distanceCode Constants.distance_short_code_value_offset
                                            in
                                            if newDistance < 0 then
                                                Err (CustomError "negative distance")

                                            else
                                                Ok
                                                    { state = s3
                                                    , distance = newDistance
                                                    , distanceCode = distanceCode
                                                    , distanceBlockLength = s3.blockLength.distanceBlockLength - 1
                                                    , metaBlockLength = newMetaBlockLength
                                                    }

                                        else
                                            let
                                                extraBits =
                                                    Array.Helpers.unsafeGet distanceCode s3.distExtraBits

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
                                                            Array.Helpers.unsafeGet distanceCode s4.distOffset
                                                                + Bitwise.shiftLeftBy s4.distanceConstants.distancePostfixBits bits
                                                    in
                                                    Ok
                                                        { state = s4
                                                        , distance = newDistance
                                                        , distanceCode = distanceCode
                                                        , distanceBlockLength = s4.blockLength.distanceBlockLength - 1
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
                        Err (CustomError ("Invalid backward reference in remainder7 at position " ++ String.fromInt s5.pos))

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
    let
        ( s1, newRings, literalBlockLength ) =
            decodeBlockTypeAndLength 0 s0.num.numLiteralBlockTypes s0

        literalBlockType =
            Array.Helpers.unsafeGet 5 newRings

        newContextMapSlice =
            Bitwise.shiftLeftBy 6 literalBlockType

        contextMode =
            Array.Helpers.unsafeGet literalBlockType s1.contextModes

        newLiteralTreeIdx =
            Bitwise.and (Array.Helpers.unsafeGet newContextMapSlice s1.contextMap) 0xFF

        oldBlockLengths =
            s1.blockLength

        newBlockLengths =
            { literalBlockLength = literalBlockLength
            , commandBlockLength = oldBlockLengths.commandBlockLength
            , distanceBlockLength = oldBlockLengths.distanceBlockLength
            }
    in
    { s1
        | contextMapSlice = newContextMapSlice
        , blockLength = newBlockLengths
        , literalTreeIdx = newLiteralTreeIdx
        , contextLookup = ContextLookup (Bitwise.shiftLeftBy 9 contextMode)
        , rings = newRings
    }


decodeDistanceBlockSwitch : State -> State
decodeDistanceBlockSwitch s0 =
    let
        ( s1, newRings, v ) =
            decodeBlockTypeAndLength 2 s0.num.numDistanceBlockTypes s0

        oldBlockLengths =
            s1.blockLength

        newBlockLengths =
            { literalBlockLength = oldBlockLengths.literalBlockLength
            , commandBlockLength = oldBlockLengths.commandBlockLength
            , distanceBlockLength = v
            }
    in
    { s1
        | blockLength = newBlockLengths
        , distContextMapSlice = Bitwise.shiftLeftBy 2 (Array.Helpers.unsafeGet 9 newRings)
        , rings = newRings
    }


decodeCommandBlockSwitch : State -> State
decodeCommandBlockSwitch s0 =
    let
        ( s1, newRings, v ) =
            decodeBlockTypeAndLength 1 s0.num.numCommandBlockTypes s0

        oldBlockLength =
            s1.blockLength

        newBlockLength =
            { literalBlockLength = oldBlockLength.literalBlockLength
            , commandBlockLength = v
            , distanceBlockLength = oldBlockLength.distanceBlockLength
            }
    in
    { s1
        | blockLength = newBlockLength
        , commandTreeIdx = Array.Helpers.unsafeGet 7 newRings
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
                                    Array.Helpers.unsafeGet (offset + 1) s3.rings + 1

                                0 ->
                                    Array.Helpers.unsafeGet offset s3.rings

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
                        |> Array.set offset (Array.Helpers.unsafeGet (offset + 1) s3.rings)
                        |> Array.set (offset + 1) blockType
                    , result
                    )


calculateFence : State -> Int
calculateFence s =
    {-
       if s.flags.isEager then
           min result (written.fromRingBuffer + Constants.outputLength - written.toOutput)

       else
    -}
    s.ringBufferSize


halfAvailable : State -> Int
halfAvailable s =
    let
        limit =
            if s.flags.endOfStreamReached then
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
                    Err CorruptedPaddingBits

                else
                    Ok s2

    else
        Ok s


readBits2 : Int -> Int -> State -> ( State, Int, Int )
readBits2 nbits mbits s0 =
    if nbits <= 16 && mbits <= 16 then
        let
            ( bitOffset1, halfOffset1, accumulator32_1 ) =
                topUpPure s0.bitOffset s0.halfOffset s0.accumulator32 s0.shortBuffer

            ( bitOffset2, val1 ) =
                pureReadFewBits nbits bitOffset1 accumulator32_1

            ( bitOffset3, halfOffset3, accumulator32_3 ) =
                topUpPure bitOffset2 halfOffset1 accumulator32_1 s0.shortBuffer

            ( bitOffset4, val2 ) =
                pureReadFewBits mbits bitOffset3 accumulator32_3
        in
        ( updateAccumulator bitOffset4 halfOffset3 accumulator32_3 s0
        , val1
        , val2
        )

    else
        let
            ( s1, v1 ) =
                readBits nbits s0

            ( s2, v2 ) =
                readBits mbits s1
        in
        ( s2, v1, v2 )


topUpPure bitOffset halfOffset accumulator32 shortBuffer =
    if bitOffset >= 16 then
        let
            next =
                Array.Helpers.unsafeGet halfOffset shortBuffer
                    |> Bitwise.shiftLeftBy 16
        in
        ( bitOffset - 16, halfOffset + 1, Bitwise.or next (Bitwise.shiftRightZfBy 16 accumulator32) )

    else
        ( bitOffset, halfOffset, accumulator32 )


readBits : Int -> State -> ( State, Int )
readBits nbits state =
    if nbits <= 16 then
        let
            ( newBitOffset, newHalfOffset, newAccumulator32 ) =
                if state.bitOffset >= 16 then
                    let
                        next =
                            Array.Helpers.unsafeGet state.halfOffset state.shortBuffer
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
                        Array.Helpers.unsafeGet state.halfOffset state.shortBuffer
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
                        (Bitwise.shiftLeftBy 16 (Array.Helpers.unsafeGet s.halfOffset s.shortBuffer))
                        (Bitwise.shiftRightZfBy 16 s.accumulator32)
            in
            updateAccumulator (s.bitOffset - 16) (s.halfOffset + 1) newAccumulator32 s
                |> readFewBits (n - 16)
                |> Tuple.mapSecond (\high -> Bitwise.or low (Bitwise.shiftLeftBy 16 high))


maybeReadMoreInput : Int -> State -> Result Error State
maybeReadMoreInput n s =
    if s.halfOffset > n then
        doReadMoreInput s

    else
        Ok s


doReadMoreInput : State -> Result Error State
doReadMoreInput s =
    if s.flags.endOfStreamReached then
        if halfAvailable s >= -2 then
            Ok s

        else
            Err (CustomError "No more inupt")

    else
        let
            readOffset =
                Bitwise.shiftLeftBy 1 s.halfOffset

            bytesInBuffer =
                4096 - readOffset

            byteBuffer =
                Array.Helpers.copyWithin 0 readOffset 4096 s.byteBuffer
        in
        -- @optimize replace record update?
        case doReadMoreInputHelp bytesInBuffer { s | byteBuffer = byteBuffer, halfOffset = 0 } of
            Err e ->
                Err e

            Ok ( s2, bytesInBuffer2 ) ->
                Ok (bytesToNibbles bytesInBuffer2 s2)


doReadMoreInputHelp : Int -> State -> Result Error ( State, Int )
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
                    let
                        oldFlags =
                            s2.flags

                        newFlags =
                            { oldFlags | endOfStreamReached = True }
                    in
                    Ok ( { s2 | flags = newFlags, tailBytes = bytesInBuffer }, bytesInBuffer + 1 )

                else
                    doReadMoreInputHelp (bytesInBuffer + len) s2

    else
        Ok ( s, bytesInBuffer )


overflow16 : Int -> Int
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
                        Array.Helpers.unsafeGet (i * 2) s.byteBuffer

                    byte2 =
                        Array.Helpers.unsafeGet (i * 2 + 1) s.byteBuffer

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
            Decode.map2 (\_ v -> v) (Decode.bytes s.input.offset) (Array.Helpers.decodeArray bytesRead Decode.signedInt8)
    in
    case Decode.decode decoder s.input.buffer of
        Just newSegment ->
            let
                oldInput =
                    s.input

                newInput =
                    { oldInput | offset = oldInput.offset + bytesRead }

                newByteBuffer =
                    Array.Helpers.setSlice newSegment offset s.byteBuffer
            in
            Ok ( { s | input = newInput, byteBuffer = newByteBuffer }, bytesRead )

        Nothing ->
            Err (CustomError "readInput: insufficient input")


decode : Bytes -> Result Error Bytes
decode input =
    case initState (defaultState input) of
        Err e ->
            Err e

        Ok s ->
            let
                decodeLoop written state chunks =
                    let
                        newWritten =
                            { toOutput = 0, fromRingBuffer = written.fromRingBuffer, output = [] }
                    in
                    case decompress newWritten state of
                        Err e ->
                            Err e

                        Ok ( buffer, newState, newerWritten ) ->
                            if Bytes.width buffer < 16384 then
                                Ok (List.reverse (Encode.bytes buffer :: chunks))

                            else
                                decodeLoop newerWritten newState (Encode.bytes buffer :: chunks)
            in
            case decodeLoop { toOutput = 0, fromRingBuffer = 0, output = [] } s [] of
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
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , runningState = orig.runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = newAccumulator32
    , bitOffset = newBitOffset
    , halfOffset = newHalfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = orig.metaBlockLength
    , num = orig.num
    , blockLength = orig.blockLength
    , pos = orig.pos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , isTrivialLiteralContext = orig.isTrivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = orig.j
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookup = orig.contextLookup
    , distanceCode = orig.distanceCode
    , distanceConstants = orig.distanceConstants
    , distance = orig.distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , flags = orig.flags
    , input = orig.input
    }


updateBitOffset : Int -> State -> State
updateBitOffset newBitOffset orig =
    { ringBuffer = orig.ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , runningState = orig.runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = newBitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = orig.metaBlockLength
    , num = orig.num
    , blockLength = orig.blockLength
    , pos = orig.pos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , isTrivialLiteralContext = orig.isTrivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = orig.j
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookup = orig.contextLookup
    , distanceCode = orig.distanceCode
    , distanceConstants = orig.distanceConstants
    , distance = orig.distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , flags = orig.flags
    , input = orig.input
    }


copyState7 : Int -> Int -> Int -> Array Int -> Int -> State -> State
copyState7 bitOffset newPos newJ newRingBuffer literalBlockLength orig =
    { ringBuffer = newRingBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , runningState = orig.runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = orig.metaBlockLength
    , num = orig.num
    , blockLength =
        let
            oldBlockLength =
                orig.blockLength
        in
        { literalBlockLength = literalBlockLength
        , commandBlockLength = oldBlockLength.commandBlockLength
        , distanceBlockLength = oldBlockLength.distanceBlockLength
        }
    , pos = newPos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , isTrivialLiteralContext = orig.isTrivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = newJ
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookup = orig.contextLookup
    , distanceCode = orig.distanceCode
    , distanceConstants = orig.distanceConstants
    , distance = orig.distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , flags = orig.flags
    , input = orig.input
    }


updateRemainder7 : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Array Int -> State -> State
updateRemainder7 distance maxDistance distanceBlockLength metaBlockLength j runningState distRbIdx rings orig =
    { ringBuffer = orig.ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , runningState = runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = metaBlockLength
    , num = orig.num
    , blockLength =
        let
            oldBlockLength =
                orig.blockLength
        in
        { literalBlockLength = oldBlockLength.literalBlockLength
        , commandBlockLength = oldBlockLength.commandBlockLength
        , distanceBlockLength = distanceBlockLength
        }
    , pos = orig.pos
    , maxDistance = maxDistance
    , distRbIdx = distRbIdx
    , isTrivialLiteralContext = orig.isTrivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = j
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookup = orig.contextLookup
    , distanceCode = orig.distanceCode
    , distanceConstants = orig.distanceConstants
    , distance = distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , flags = orig.flags
    , input = orig.input
    }


updateEvaluateState8 : Array Int -> Int -> Int -> Int -> Int -> State -> State
updateEvaluateState8 ringBuffer j metaBlockLength pos runningState orig =
    { ringBuffer = ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , runningState = runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = metaBlockLength
    , num = orig.num
    , blockLength = orig.blockLength
    , pos = pos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , isTrivialLiteralContext = orig.isTrivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = j
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookup = orig.contextLookup
    , distanceCode = orig.distanceCode
    , distanceConstants = orig.distanceConstants
    , distance = orig.distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , flags = orig.flags
    , input = orig.input
    }


updateEvaluateState4 : Int -> Int -> Int -> Int -> Int -> Int -> State -> State
updateEvaluateState4 j runningState copyLength insertLength distanceCode commandBlockLength orig =
    { ringBuffer = orig.ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , runningState = runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = orig.metaBlockLength
    , num = orig.num
    , blockLength =
        let
            oldBlockLength =
                orig.blockLength
        in
        { literalBlockLength = oldBlockLength.literalBlockLength
        , commandBlockLength = commandBlockLength
        , distanceBlockLength = oldBlockLength.distanceBlockLength
        }
    , pos = orig.pos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , isTrivialLiteralContext = orig.isTrivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = j
    , insertLength = insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookup = orig.contextLookup
    , distanceCode = distanceCode
    , distanceConstants = orig.distanceConstants
    , distance = orig.distance
    , copyLength = copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , flags = orig.flags
    , input = orig.input
    }


updateRunningState : Int -> State -> State
updateRunningState runningState orig =
    { ringBuffer = orig.ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , runningState = runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = orig.metaBlockLength
    , num = orig.num
    , blockLength = orig.blockLength
    , pos = orig.pos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , isTrivialLiteralContext = orig.isTrivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = orig.j
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookup = orig.contextLookup
    , distanceCode = orig.distanceCode
    , distanceConstants = orig.distanceConstants
    , distance = orig.distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , flags = orig.flags
    , input = orig.input
    }


updateEvaluateState9 : Int -> Int -> Array Int -> Int -> Int -> State -> State
updateEvaluateState9 nextRunningState runningState ringBuffer pos metaBlockLength orig =
    { ringBuffer = ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , runningState = runningState
    , nextRunningState = nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = metaBlockLength
    , num = orig.num
    , blockLength = orig.blockLength
    , pos = pos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , isTrivialLiteralContext = orig.isTrivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = orig.j
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookup = orig.contextLookup
    , distanceCode = orig.distanceCode
    , distanceConstants = orig.distanceConstants
    , distance = orig.distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , flags = orig.flags
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
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , intBuffer = orig.intBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , runningState = orig.runningState
    , nextRunningState = orig.nextRunningState
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = orig.metaBlockLength
    , num = orig.num
    , blockLength = orig.blockLength
    , pos = orig.pos
    , maxDistance = orig.maxDistance
    , distRbIdx = orig.distRbIdx
    , isTrivialLiteralContext = orig.isTrivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = orig.commandTreeIdx
    , j = orig.j
    , insertLength = orig.insertLength
    , contextMapSlice = orig.contextMapSlice
    , distContextMapSlice = orig.distContextMapSlice
    , contextLookup = orig.contextLookup
    , distanceCode = orig.distanceCode
    , distanceConstants = orig.distanceConstants
    , distance = orig.distance
    , copyLength = orig.copyLength
    , maxBackwardDistance = orig.maxBackwardDistance
    , maxRingBufferSize = orig.maxRingBufferSize
    , ringBufferSize = orig.ringBufferSize
    , expectedTotalSize = orig.expectedTotalSize
    , flags = orig.flags
    , input = orig.input
    }
