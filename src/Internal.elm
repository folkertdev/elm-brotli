module Internal exposing (buildHuffmanTable, calculateDistanceLut, decode, decompress, generateCount, generateOffsets, nextTableBitSize, phase1, readComplexHuffmanCodeHelp, sortSymbols)

import Array exposing (Array)
import Array.Helpers
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Step(..))
import Bytes.Encode as Encode
import Constants
import DictionaryData
import DistanceRingBuffer exposing (DistanceRingBuffer)
import RingBuffer exposing (RingBuffer)
import State exposing (Error(..), State)
import Transforms


decode : Bytes -> Result Error Bytes
decode input =
    case State.initState (State.defaultState input) |> Result.andThen evaluateState1 of
        Err e ->
            Err e

        Ok ( initialRunningState, initialNextRunningState, s ) ->
            let
                decodeLoop runningState nextRunningState written state chunks =
                    let
                        newWritten =
                            { toOutput = 0, fromRingBuffer = written.fromRingBuffer, output = [] }
                    in
                    case decompress newWritten ( runningState, nextRunningState, state ) of
                        Err e ->
                            Err e

                        Ok ( buffer, ( newRunningState, newNextRunningState, newState ), newerWritten ) ->
                            if Bytes.width buffer < 16384 then
                                Ok (List.reverse (Encode.bytes buffer :: chunks))

                            else
                                decodeLoop newRunningState newNextRunningState newerWritten newState (Encode.bytes buffer :: chunks)
            in
            case decodeLoop initialRunningState initialNextRunningState { toOutput = 0, fromRingBuffer = 0, output = [] } s [] of
                Err e ->
                    Err e

                Ok v ->
                    v
                        |> Encode.sequence
                        |> Encode.encode
                        |> Ok


decompress : Written -> ( Int, Int, State ) -> Result Error ( Bytes, ( Int, Int, State ), Written )
decompress written ( runningState, nextRunningState, s ) =
    if runningState == 0 then
        Err (CustomError "Can't decompreunvalidateds until initialized")

    else if runningState == 11 then
        Err (CustomError "Can't decompress after close")

    else if runningState == 1 then
        Err (CustomError "use evaluateState1 first")

    else
        case decompressHelp { fence = calculateFence written s, ringBufferMask = RingBuffer.size s.ringBuffer - 1 } written runningState nextRunningState s of
            Err e ->
                Err e

            Ok ( r, _, newWritten ) ->
                let
                    asBytes =
                        newWritten.output
                            |> List.reverse
                            |> Encode.sequence
                            |> Encode.encode
                in
                Ok ( asBytes, r, newWritten )


type alias Context =
    { fence : Int
    , ringBufferMask : Int
    }


type alias Written =
    { toOutput : Int
    , fromRingBuffer : Int
    , output : List Encode.Encoder
    }


decompressHelp : Context -> Written -> Int -> Int -> State -> Result Error ( ( Int, Int, State ), Context, Written )
decompressHelp context written runningState nextRunningState s =
    let
        _ =
            -- Debug.log "state" ( runningState, ( s.pos, s.bitOffset, s.accumulator32 ) )
            -- Debug.log "state" ( runningState, s.ringBuffer )
            ()
    in
    case runningState of
        10 ->
            Ok ( ( runningState, nextRunningState, s ), context, written )

        2 ->
            if s.metaBlockLength < 0 then
                Err InvalidMetablockLength

            else
                case readNextMetablockHeader runningState nextRunningState s of
                    Err e ->
                        Err e

                    Ok ( newRunningState, newNextRunningState, s2 ) ->
                        let
                            fence =
                                calculateFence written s2
                        in
                        decompressHelp { fence = fence, ringBufferMask = RingBuffer.size s2.ringBuffer - 1 } written newRunningState newNextRunningState s2

        3 ->
            case readMetablockHuffmanCodesAndContextMaps s of
                Err e ->
                    Err e

                Ok s2 ->
                    decompressHelp context written 4 nextRunningState s2

        4 ->
            if s.metaBlockLength <= 0 then
                decompressHelp context written 2 nextRunningState s

            else
                case State.maybeReadMoreInput 2030 s of
                    Err e ->
                        Err e

                    Ok s1 ->
                        case evaluateState4 runningState nextRunningState s1 of
                            Err e ->
                                Err e

                            Ok ( newRunningState, newNextRunningState, newState ) ->
                                decompressHelp context written newRunningState newNextRunningState newState

        7 ->
            if s.isTrivialLiteralContext then
                case evaluateState7Trivial context runningState nextRunningState s of
                    Err e ->
                        Err e

                    Ok ( newRunningState, newNextRunningState, newState ) ->
                        decompressHelp context written newRunningState newNextRunningState newState

            else
                let
                    pos =
                        RingBuffer.position s.ringBuffer

                    init_prevByte1 =
                        RingBuffer.get (Bitwise.and (pos - 1) context.ringBufferMask) s.ringBuffer
                            |> Bitwise.and 0xFF

                    init_prevByte2 =
                        RingBuffer.get (Bitwise.and (pos - 2) context.ringBufferMask) s.ringBuffer
                            |> Bitwise.and 0xFF
                in
                case evaluateState7NonTrivial context init_prevByte1 init_prevByte2 runningState nextRunningState s of
                    Err e ->
                        Err e

                    Ok ( newRunningState, newNextRunningState, newState ) ->
                        decompressHelp context written newRunningState newNextRunningState newState

        8 ->
            let
                ( newRunningState, newNextRunningState, newState ) =
                    evaluateState8 context ( runningState, nextRunningState, s )
            in
            decompressHelp context written newRunningState newNextRunningState newState

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
                            Transforms.transformDictionaryWord s.ringBuffer (RingBuffer.position s.ringBuffer) DictionaryData.dictionary offset s.copyLength Transforms.rfc_transforms transformIdx

                        newMetaBlockLength =
                            s.metaBlockLength - len
                    in
                    if RingBuffer.position newRingBuffer >= context.fence then
                        let
                            newState =
                                State.updateEvaluateState9 newRingBuffer newMetaBlockLength s
                        in
                        decompressHelp context written 13 4 newState

                    else
                        let
                            newState =
                                State.updateEvaluateState9 newRingBuffer newMetaBlockLength s
                        in
                        decompressHelp context written 4 nextRunningState newState

                else
                    Err (InvalidBackwardReference ("the transform index is " ++ String.fromInt transformIdx ++ ", but it must be smaller than 121"))

            else
                Err (InvalidBackwardReference ("CopyLength must be 4 >= copyLength <= 24, but it is " ++ String.fromInt s.copyLength))

        6 ->
            case copyUncompressedData s of
                Err e ->
                    Err e

                Ok ( newRunningState, newNextRunningState, newState ) ->
                    decompressHelp context written newRunningState newNextRunningState newState

        12 ->
            -- should not happen, go directly to state 13
            let
                newState =
                    s
            in
            decompressHelp context written runningState nextRunningState newState

        13 ->
            let
                ( wasWritten, newWritten ) =
                    writeRingBuffer written s.ringBuffer
            in
            if not wasWritten then
                Ok ( ( runningState, nextRunningState, s ), context, newWritten )

            else
                let
                    position =
                        RingBuffer.position s.ringBuffer

                    newMaxDistance =
                        if position >= s.maxBackwardDistance then
                            s.maxBackwardDistance

                        else
                            s.maxDistance
                in
                if position >= RingBuffer.size s.ringBuffer then
                    let
                        newRingBuffer =
                            RingBuffer.loopAround (Bitwise.and position context.ringBufferMask) s.ringBuffer

                        newState =
                            { s
                                | ringBuffer = newRingBuffer
                                , maxDistance = newMaxDistance
                            }

                        newerWritten =
                            { fromRingBuffer = 0, toOutput = newWritten.toOutput, output = newWritten.output }
                    in
                    -- not a typo, switches to the next running state
                    decompressHelp context newerWritten nextRunningState nextRunningState newState

                else
                    let
                        newState =
                            { s | maxDistance = newMaxDistance }
                    in
                    decompressHelp context newWritten nextRunningState nextRunningState newState

        _ ->
            Ok ( ( runningState, nextRunningState, s ), context, written )


readNextMetablockHeader : Int -> Int -> State -> Result Error ( Int, Int, State )
readNextMetablockHeader runningState nextRunningState s =
    if s.flags.inputEnd then
        Ok ( 13, 10, s )

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
        State.maybeReadMoreInput 2030 s2
            |> Result.andThen decodeMetaBlockLength
            |> Result.andThen
                (\s4 ->
                    if s4.metaBlockLength == 0 && s4.flags.isMetadata == False then
                        Ok ( runningState, nextRunningState, s4 )

                    else
                        let
                            findNextState =
                                if s4.flags.isUncompressed || s4.flags.isMetadata then
                                    case State.jumpToByteBoundary s4 of
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

                            Ok ( s6, newRunningState ) ->
                                if s6.flags.isMetadata then
                                    Ok ( newRunningState, nextRunningState, s6 )

                                else
                                    let
                                        newRingBuffer =
                                            RingBuffer.updateExpectation s6.flags s6.metaBlockLength s6.ringBuffer
                                    in
                                    Ok ( newRunningState, nextRunningState, { s6 | ringBuffer = newRingBuffer } )
                )


decodeMetaBlockBytes : State -> Result Error State
decodeMetaBlockBytes s_ =
    let
        byteLoop i sizeBytes state_ =
            if i < sizeBytes then
                case State.readFewBitsSafe 8 state_ of
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
    case State.readFewBits 1 s of
        ( s1, reserved ) ->
            if reserved /= 0 then
                Err CorruptedReservedBit

            else
                case State.readFewBits 2 s1 of
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
                case State.readFewBitsSafe 4 state_ of
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
    case State.readFewBitsSafe 1 s_ of
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
                    case State.readFewBits 2 s0 of
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
                                        case State.readFewBits 1 s6 of
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
                case State.readFewBits 1 s of
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
                    State.readFewBitsSafe 1 initial

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
                        State.readFewBits 3 s2
                in
                if firstN /= 0 then
                    ( s3_, 17 + firstN )

                else
                    let
                        ( s3, n ) =
                            State.readFewBits 3 s3_
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
                                        State.readFewBits 1 s4
                                in
                                if w == 1 then
                                    ( s5, -1 )

                                else
                                    let
                                        ( s6, m ) =
                                            State.readFewBits 6 s5
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
    case State.readFewBitsSafe 1 s_ of
        ( s, 0 ) ->
            ( s, 0 )

        ( s, _ ) ->
            case State.readFewBits 3 s of
                ( s2, n ) ->
                    if n == 0 then
                        ( s2, 1 )

                    else
                        case State.readFewBits n s2 of
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
        ( bitOffset1, halfOffset1, accumulator32_1 ) =
            if s.bitOffset >= 16 then
                let
                    next =
                        Array.get s.halfOffset s.shortBuffer
                            |> Maybe.withDefault 0
                            |> Bitwise.shiftLeftBy 16
                in
                ( s.bitOffset - 16, s.halfOffset + 1, Bitwise.or next (Bitwise.shiftRightZfBy 16 s.accumulator32) )

            else
                ( s.bitOffset, s.halfOffset, s.accumulator32 )

        ( bitOffset2, sym ) =
            readSymbolPureLowlevel bitOffset1 accumulator32_1 tableGroup tableIdx
    in
    ( State.updateAccumulator bitOffset2 halfOffset1 accumulator32_1 s, sym )


readSymbolPure : Array Int -> Int -> State -> ( Int, Int )
readSymbolPure tableGroup tableIdx s =
    readSymbolPureLowlevel s.bitOffset s.accumulator32 tableGroup tableIdx


readSymbolPureLowlevel : Int -> Int -> Array Int -> Int -> ( Int, Int )
readSymbolPureLowlevel bitOffset accumulator32 tableGroup tableIdx =
    let
        offset =
            Array.Helpers.unsafeGet tableIdx tableGroup + Bitwise.and val 0xFF

        val =
            Bitwise.shiftRightZfBy bitOffset accumulator32

        tableValue1 =
            Array.Helpers.unsafeGet offset tableGroup

        bits =
            Bitwise.shiftRightBy 16 tableValue1

        sym =
            Bitwise.and tableValue1 0xFFFF
    in
    if bits <= 8 then
        ( bitOffset + bits, sym )

    else
        let
            offset2 =
                offset + sym

            mask =
                Bitwise.shiftLeftBy bits 1 - 1

            offset3 =
                offset2 + Bitwise.shiftRightZfBy 8 (Bitwise.and val mask)

            tableValue2 =
                Array.Helpers.unsafeGet offset3 tableGroup

            newBitOffset =
                bitOffset + 8 + Bitwise.shiftRightBy 16 tableValue2

            result =
                Bitwise.and tableValue2 0xFFFF
        in
        ( newBitOffset, result )


readHuffmanCode : Int -> Int -> Array Int -> Int -> State -> Result Error ( State, { tableGroup : Array Int, total_size : Int } )
readHuffmanCode alphabetSizeMax alphabetSizeLimit tableGroup tableIdx s_ =
    let
        checkEnoughRead =
            State.maybeReadMoreInput 2030 s_
    in
    case checkEnoughRead of
        Err e ->
            Err e

        Ok s ->
            case State.readFewBitsSafe 2 s of
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
                State.topUpAccumulator s

            p =
                Bitwise.shiftRightZfBy s1.bitOffset s1.accumulator32
                    |> Bitwise.and 15

            s2 =
                State.updateBitOffset (s1.bitOffset + Bitwise.shiftRightBy 16 (Array.Helpers.unsafeGet p Constants.fixed_table)) s1

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
                case State.maybeReadMoreInput 2030 s |> Result.map State.topUpAccumulator of
                    Err e ->
                        Err e

                    Ok s1 ->
                        let
                            p =
                                Bitwise.and (Bitwise.shiftRightZfBy s1.bitOffset s1.accumulator32) 31

                            s2 =
                                State.updateBitOffset (s1.bitOffset + Bitwise.shiftRightBy 16 (Array.Helpers.unsafeGet p table)) s1

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
                                    State.topUpAccumulator s2
                            in
                            case State.readFewBits extraBits s3 of
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
            1 + State.log2floor (alphabetSizeMax - 1)

        go i numSymbols s_ acc =
            if i < numSymbols then
                case State.readFewBitsSafe maxBits s_ of
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
                case State.readFewBits 1 s of
                    ( newS, extra ) ->
                        ( newS, extra + numSymbols )

            else
                ( s, numSymbols )
    in
    case State.readFewBits 2 s0 of
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
        ( s2, code ) =
            readSymbol tableGroup tableIdx s0

        n =
            Array.Helpers.unsafeGet code Constants.block_length_n_bits

        ( s4, result ) =
            if n <= 16 then
                State.readFewBitsSafe n s2

            else
                -- @note removed a topUpAccumulator here
                State.readManyBits n s2
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
            State.maybeReadMoreInput 2030 s
                |> Result.map State.topUpAccumulator

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
                                case State.maybeReadMoreInput 2030 newS of
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
                                State.topUpAccumulator s0
                        in
                        case State.readFewBits 2 s1 of
                            ( s2, cm ) ->
                                go2 limit (i + 1) s2 (Array.push cm acc)

                    else
                        Ok ( i, s0, acc )

                readFewBits2 n m w0 =
                    let
                        ( w1, v1 ) =
                            State.readFewBits n w0

                        ( w2, v2 ) =
                            State.readFewBits m w1
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
                                                            State.calculateDistanceAlphabetSize s5.distanceConstants.distancePostfixBits s5.distanceConstants.numDirectDistanceCodes 62
                                                    in
                                                    case State.calculateDistanceAlphabetLimit 0x7FFFFFFC s5.distanceConstants.distancePostfixBits s5.distanceConstants.numDirectDistanceCodes of
                                                        Ok v ->
                                                            Ok ( max, v )

                                                        Err e ->
                                                            Err e

                                                else
                                                    let
                                                        max =
                                                            State.calculateDistanceAlphabetSize s5.distanceConstants.distancePostfixBits s5.distanceConstants.numDirectDistanceCodes 24
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
                                                                    , contextLookup = State.ContextLookup (Array.Helpers.unsafeGet 0 contextModes * 512)
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
                                                                            |> DistanceRingBuffer.set 4 1
                                                                            |> DistanceRingBuffer.set 5 0
                                                                            |> DistanceRingBuffer.set 6 1
                                                                            |> DistanceRingBuffer.set 7 0
                                                                            |> DistanceRingBuffer.set 8 1
                                                                            |> DistanceRingBuffer.set 9 0
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
            State.readFewBitsSafe 1
                >> (\( s, useRleForZeros ) ->
                        if useRleForZeros /= 0 then
                            case State.readFewBits 4 s of
                                ( newS, v ) ->
                                    ( newS, v + 1 )

                        else
                            ( s, 0 )
                   )
    in
    case State.maybeReadMoreInput 2030 s0 of
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
                                            case State.maybeReadMoreInput 2030 s |> Result.map (readSymbol table.tableGroup tableIdx) of
                                                Err e ->
                                                    Err e

                                                Ok ( s5, code ) ->
                                                    if code == 0 then
                                                        go (i + 1) (Array.set i 0 currentContextMap) s5

                                                    else if code <= maxRunLengthPrefix then
                                                        case State.readFewBitsSafe code s5 of
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
                                            case State.readFewBitsSafe 1 s of
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


evaluateState1 : ( Int, Int, State ) -> Result Error ( Int, Int, State )
evaluateState1 ( runningState, nextRunningState, s ) =
    case decodeWindowBits s of
        ( newS, windowBits ) ->
            if windowBits == -1 then
                Err InvalidWindowBits

            else
                let
                    maxRingBufferSize =
                        Bitwise.shiftLeftBy windowBits 1
                in
                Ok
                    ( 2
                    , nextRunningState
                    , { newS
                        | ringBuffer = RingBuffer.setMaxSize maxRingBufferSize newS.ringBuffer
                        , maxBackwardDistance = Bitwise.shiftLeftBy windowBits 1 - 16
                      }
                    )


evaluateState4 : Int -> Int -> State -> Result Error ( Int, Int, State )
evaluateState4 runningState nextRunningState s1 =
    let
        ( s2, commandBlockData ) =
            if s1.blockLength.commandBlockLength == 0 then
                let
                    ( sx, newRings, value ) =
                        decodeBlockTypeAndLength 1 s1.num.numCommandBlockTypes s1

                    oldBlockLength =
                        sx.blockLength

                    newBlockLength =
                        { literalBlockLength = oldBlockLength.literalBlockLength
                        , commandBlockLength = value
                        , distanceBlockLength = oldBlockLength.distanceBlockLength
                        }
                in
                ( sx
                , { blockLength = newBlockLength
                  , commandTreeIdx = DistanceRingBuffer.get 7 newRings
                  , rings = newRings
                  }
                )

            else
                -- @optimize can we remove this allocation?
                ( s1
                , { blockLength = s1.blockLength
                  , commandTreeIdx = s1.commandTreeIdx
                  , rings = s1.rings
                  }
                )
    in
    let
        ( s3, v ) =
            readSymbol s2.treeGroup.commandTreeGroup commandBlockData.commandTreeIdx s2

        cmdCode =
            Bitwise.shiftLeftBy 2 v

        insertAndCopyExtraBits =
            Array.Helpers.unsafeGet (cmdCode + 0) Constants.cmd_lookup

        insertLengthOffset =
            Array.Helpers.unsafeGet (cmdCode + 1) Constants.cmd_lookup

        copyLengthOffset =
            Array.Helpers.unsafeGet (cmdCode + 2) Constants.cmd_lookup

        ( s5, insertLengthBits, copyLengthBits ) =
            let
                extraBits1 =
                    Bitwise.and insertAndCopyExtraBits 0xFF

                extraBits2 =
                    Bitwise.shiftRightBy 8 insertAndCopyExtraBits
            in
            State.readBits2 extraBits1 extraBits2 s3

        oldBlockLength =
            commandBlockData.blockLength

        newBlockLength =
            { commandBlockLength = oldBlockLength.commandBlockLength - 1
            , literalBlockLength = oldBlockLength.literalBlockLength
            , distanceBlockLength = oldBlockLength.distanceBlockLength
            }
    in
    Ok
        ( 7
        , nextRunningState
        , State.updateEvaluateState4
            0
            (copyLengthBits + copyLengthOffset)
            (insertLengthBits + insertLengthOffset)
            (Array.Helpers.unsafeGet (cmdCode + 3) Constants.cmd_lookup)
            newBlockLength
            commandBlockData.commandTreeIdx
            commandBlockData.rings
            s5
        )


maybeLiteral : State -> State
maybeLiteral state =
    if state.blockLength.literalBlockLength == 0 then
        decodeLiteralBlockSwitch state

    else
        state


evaluateState7Trivial context runningState nextRunningState s0 =
    if s0.j < s0.insertLength then
        case State.maybeReadMoreInput 2030 s0 |> Result.map maybeLiteral of
            Err e ->
                Err e

            Ok s1 ->
                case readSymbolPure s1.treeGroup.literalTreeGroup s1.literalTreeIdx s1 of
                    ( newBitOffset, value ) ->
                        let
                            newRingBuffer =
                                RingBuffer.push value s1.ringBuffer
                        in
                        if RingBuffer.position newRingBuffer >= context.fence then
                            let
                                oldBlockLengths =
                                    s1.blockLength

                                newBlockLengths =
                                    { literalBlockLength = s1.blockLength.literalBlockLength - 1
                                    , commandBlockLength = oldBlockLengths.commandBlockLength
                                    , distanceBlockLength = oldBlockLengths.distanceBlockLength
                                    }
                            in
                            Ok ( 13, 7, { s1 | bitOffset = newBitOffset, j = s1.j + 1, ringBuffer = newRingBuffer, blockLength = newBlockLengths } )

                        else
                            evaluateState7Trivial context runningState nextRunningState (State.copyState7 newBitOffset (s1.j + 1) newRingBuffer (s1.blockLength.literalBlockLength - 1) s1)

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
        remainder7 ( runningState, nextRunningState, { s0 | blockLength = newBlockLengths } )


evaluateState7NonTrivial : Context -> Int -> Int -> Int -> Int -> State -> Result Error ( Int, Int, State )
evaluateState7NonTrivial context prevByte1 prevByte2 runningState nextRunningState s0 =
    if s0.j < s0.insertLength then
        case State.maybeReadMoreInput 2030 s0 of
            Err e ->
                Err e

            Ok s_ ->
                let
                    s1 =
                        State.topUpAccumulator (maybeLiteral s_)

                    i1 =
                        State.contextLookupOffset1 s1.contextLookup + prevByte1

                    i2 =
                        State.contextLookupOffset2 s1.contextLookup + prevByte2

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
                                -- RingBuffer.set s2.pos byte1 s2.ringBuffer
                                RingBuffer.push byte1 s2.ringBuffer
                        in
                        if RingBuffer.position newRingBuffer >= context.fence then
                            let
                                oldBlockLengths =
                                    s2.blockLength

                                newBlockLengths =
                                    { literalBlockLength = s2.blockLength.literalBlockLength - 1
                                    , commandBlockLength = oldBlockLengths.commandBlockLength
                                    , distanceBlockLength = oldBlockLengths.distanceBlockLength
                                    }
                            in
                            Ok ( 13, 7, { s2 | bitOffset = newBitOffset, j = s2.j + 1, ringBuffer = newRingBuffer, blockLength = newBlockLengths } )

                        else
                            evaluateState7NonTrivial context byte1 byte2 runningState nextRunningState (State.copyState7 newBitOffset (s2.j + 1) newRingBuffer (s2.blockLength.literalBlockLength - 1) s2)

    else
        remainder7 ( runningState, nextRunningState, s0 )


evaluateState8 : Context -> ( Int, Int, State ) -> ( Int, Int, State )
evaluateState8 context ( runningState, nextRunningState, s ) =
    let
        src =
            Bitwise.and (RingBuffer.position s.ringBuffer - s.distance) context.ringBufferMask

        dst =
            RingBuffer.position s.ringBuffer

        copyLength =
            s.copyLength - s.j

        srcEnd =
            src + copyLength

        dstEnd =
            dst + copyLength
    in
    if (srcEnd < context.ringBufferMask) && (dstEnd < context.ringBufferMask) then
        let
            -- sliceFoldl is slower because of allocation, probably
            newRingBuffer =
                if False && (dst < src || dst > srcEnd) then
                    RingBuffer.sliceFoldl src srcEnd RingBuffer.push s.ringBuffer s.ringBuffer

                else
                    RingBuffer.copyWithin dst src srcEnd s.ringBuffer
        in
        ( if runningState == 8 then
            4

          else
            runningState
        , nextRunningState
        , State.updateEvaluateState8
            newRingBuffer
            (s.j + copyLength)
            (s.metaBlockLength - copyLength)
            s
        )

    else
        -- NOTE this branch is untested; seems to almost never get hit
        let
            go : Int -> { ringBuffer : RingBuffer, metaBlockLength : Int, j : Int } -> { ringBuffer : RingBuffer, metaBlockLength : Int, j : Int }
            go distance state =
                let
                    s1 =
                        { ringBuffer =
                            state.ringBuffer
                                -- |> RingBuffer.set state.pos (RingBuffer.get (Bitwise.and (state.pos - distance) context.ringBufferMask) state.ringBuffer)
                                |> RingBuffer.push (RingBuffer.get (Bitwise.and (RingBuffer.position state.ringBuffer - distance) context.ringBufferMask) state.ringBuffer)
                        , metaBlockLength = state.metaBlockLength - 1
                        , j = state.j + 1
                        }
                in
                if RingBuffer.position s1.ringBuffer >= context.fence then
                    s1

                else
                    go distance s1

            smallerState =
                { ringBuffer = s.ringBuffer
                , metaBlockLength = s.metaBlockLength
                , j = s.j
                }

            newSmallerState =
                go s.distance smallerState
        in
        ( 13
        , 8
        , { s
            | ringBuffer = newSmallerState.ringBuffer
            , metaBlockLength = newSmallerState.metaBlockLength
            , j = newSmallerState.j
          }
        )


copyUncompressedData : State -> Result Error ( Int, Int, State )
copyUncompressedData s =
    if s.metaBlockLength <= 0 then
        case State.reload s of
            Err e ->
                Err e

            Ok state ->
                Ok ( 2, -42, state )

    else
        let
            chunkLength =
                min (RingBuffer.size s.ringBuffer - RingBuffer.position s.ringBuffer) s.metaBlockLength
        in
        case copyBytesToRingBuffer (RingBuffer.position s.ringBuffer) chunkLength s.ringBuffer s of
            Err e ->
                Err e

            Ok ( s1, newRingBuffer ) ->
                let
                    s2 =
                        { s1 | ringBuffer = newRingBuffer, metaBlockLength = s1.metaBlockLength - chunkLength }
                in
                if RingBuffer.position s2.ringBuffer == RingBuffer.size s2.ringBuffer then
                    Ok ( 13, 6, s2 )

                else
                    case State.reload s2 of
                        Err e ->
                            Err e

                        Ok state ->
                            Ok ( 2, -42, state )


copyBytesToRingBuffer : Int -> Int -> RingBuffer -> State -> Result Error ( State, RingBuffer )
copyBytesToRingBuffer offset length data s =
    -- NOTE data is the ringbufffer
    if Bitwise.and s.bitOffset 7 /= 0 then
        Err UnalignedCopyBytes

    else
        let
            emptyAccumulator state bitOffset currentLength accum =
                if bitOffset /= 32 && currentLength /= 0 then
                    emptyAccumulator state (bitOffset + 8) (currentLength - 1) (RingBuffer.push (Bitwise.shiftRightZfBy bitOffset state.accumulator32) accum)

                else
                    ( State.updateBitOffset bitOffset state, currentLength, accum )

            ( s1, length1, data1 ) =
                emptyAccumulator s s.bitOffset length data

            maybeCopyNibbles state currentLength accum =
                let
                    copyNibbles =
                        min (State.halfAvailable state) (Bitwise.shiftRightBy 1 currentLength)
                in
                if copyNibbles > 0 then
                    let
                        readOffset =
                            Bitwise.shiftLeftBy 1 state.halfOffset

                        delta =
                            Bitwise.shiftLeftBy 1 copyNibbles
                    in
                    ( { state | halfOffset = state.halfOffset + copyNibbles }
                    , currentLength - delta
                    , RingBuffer.appendRight accum (Array.slice readOffset (readOffset + delta) state.byteBuffer)
                    )

                else
                    ( state
                    , currentLength
                    , accum
                    )

            maybeWriteMore s0 length0 accum0 =
                let
                    go state currentBitOffset currentLength accum =
                        if currentLength /= 0 then
                            go
                                state
                                (currentBitOffset + 8)
                                (currentLength - 1)
                                (RingBuffer.push (Bitwise.shiftRightZfBy currentBitOffset state.accumulator32) accum)

                        else
                            ( State.updateBitOffset currentBitOffset state, accum )

                    ( s3, newData ) =
                        let
                            sx =
                                State.topUpAccumulator s0
                        in
                        go sx sx.bitOffset length0 accum0
                in
                State.checkHealth False s3
                    |> Result.map (\st -> ( st, newData ))

            readFromInput state currentOffset currentLength accum =
                if currentLength > 0 then
                    case State.readInput currentOffset currentLength state of
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
                ( s2, length2, data2 ) =
                    maybeCopyNibbles s1 length1 data1
            in
            if length2 == 0 then
                Ok ( s2, data2 )

            else if State.halfAvailable s2 > 0 then
                maybeWriteMore s2 length2 data2

            else
                readFromInput s2 (RingBuffer.position s2.ringBuffer) length2 data2


{-| Take a slice from the ringbuffer and write it to the output
-}
writeRingBuffer : Written -> RingBuffer -> ( Bool, Written )
writeRingBuffer written ringBuffer =
    let
        ringBufferBytesReady =
            min (RingBuffer.position ringBuffer) (RingBuffer.size ringBuffer)

        toWrite =
            min (Constants.outputLength - written.toOutput) (ringBufferBytesReady - written.fromRingBuffer)

        newerWritten =
            if toWrite /= 0 then
                let
                    newOutput =
                        let
                            encoderList =
                                RingBuffer.sliceFoldr written.fromRingBuffer (written.fromRingBuffer + toWrite) Array.Helpers.fasterEncodeFolderR ( 0, 0, [] ) ringBuffer
                                    |> Array.Helpers.fasterEncodeR
                                    |> Encode.sequence
                        in
                        encoderList :: written.output

                    newWritten =
                        { toOutput = written.toOutput + toWrite, fromRingBuffer = written.fromRingBuffer + toWrite, output = newOutput }
                in
                newWritten

            else
                written
    in
    ( newerWritten.toOutput < Constants.outputLength, newerWritten )


remainder7 : ( Int, Int, State ) -> Result Error ( Int, Int, State )
remainder7 ( runningState, nextRunningState, s ) =
    if runningState /= 7 then
        Ok ( runningState, nextRunningState, s )

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
            Ok ( 4, nextRunningState, { s1 | metaBlockLength = newMetaBlockLength } )

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
                                DistanceRingBuffer.getAtCurrentPosition s1.rings
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
                        case State.maybeReadMoreInput 2030 s1 |> Result.map maybeDistance of
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
                                                    Bitwise.and (DistanceRingBuffer.getPosition s3.rings + Array.Helpers.unsafeGet distanceCode Constants.distance_short_code_index_offset) 0x03

                                                newDistance =
                                                    DistanceRingBuffer.get index s3.rings + Array.Helpers.unsafeGet distanceCode Constants.distance_short_code_value_offset
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
                                                        State.readFewBits extraBits s3

                                                    else
                                                        State.readBits extraBits (State.topUpAccumulator s3)
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

                        newMaxDistance =
                            if new.distance /= s5.maxBackwardDistance && RingBuffer.position s5.ringBuffer < s5.maxBackwardDistance then
                                RingBuffer.position s5.ringBuffer

                            else
                                s5.maxBackwardDistance
                    in
                    if new.distance > newMaxDistance then
                        Ok
                            ( 9
                            , nextRunningState
                            , State.updateRemainder7
                                new.distance
                                newMaxDistance
                                new.distanceBlockLength
                                new.metaBlockLength
                                nextRunningState
                                s5.rings
                                s5
                            )

                    else if s5.copyLength > s.metaBlockLength then
                        Err (CustomError ("Invalid backward reference in remainder7 at position " ++ String.fromInt (RingBuffer.position s5.ringBuffer)))

                    else if new.distanceCode > 0 then
                        let
                            distRbIdx =
                                (DistanceRingBuffer.getPosition s5.rings + 1)
                                    |> Bitwise.and 0x03
                        in
                        Ok
                            ( 8
                            , nextRunningState
                            , State.updateRemainder7
                                new.distance
                                newMaxDistance
                                new.distanceBlockLength
                                new.metaBlockLength
                                0
                                (DistanceRingBuffer.set distRbIdx new.distance s5.rings |> DistanceRingBuffer.setPosition distRbIdx)
                                s5
                            )

                    else
                        Ok
                            ( 8
                            , nextRunningState
                            , State.updateRemainder7
                                new.distance
                                newMaxDistance
                                new.distanceBlockLength
                                new.metaBlockLength
                                0
                                --   8
                                s5.rings
                                s5
                            )


decodeLiteralBlockSwitch : State -> State
decodeLiteralBlockSwitch s0 =
    let
        ( s1, newRings, literalBlockLength ) =
            decodeBlockTypeAndLength 0 s0.num.numLiteralBlockTypes s0

        literalBlockType =
            DistanceRingBuffer.get 5 newRings

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
        , contextLookup = State.ContextLookup (Bitwise.shiftLeftBy 9 contextMode)
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
        , distContextMapSlice = Bitwise.shiftLeftBy 2 (DistanceRingBuffer.get 9 newRings)
        , rings = newRings
    }


decodeCommandBlockSwitch : State -> ( State, { blockLength : State.BlockLength, commandTreeIdx : Int, rings : DistanceRingBuffer } )
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
    ( s1
    , { blockLength = newBlockLength
      , commandTreeIdx = DistanceRingBuffer.get 7 newRings
      , rings = newRings
      }
    )


decodeBlockTypeAndLength : Int -> Int -> State -> ( State, DistanceRingBuffer, Int )
decodeBlockTypeAndLength treeType numBlockTypes s0 =
    let
        offset =
            4 + treeType * 2

        ( s2, initialBlockType ) =
            readSymbol s0.blockTrees (2 * treeType) s0

        ( s3, result ) =
            readBlockLength s2.blockTrees (2 * treeType + 1) s2

        blockType =
            let
                v =
                    case initialBlockType of
                        1 ->
                            DistanceRingBuffer.get (offset + 1) s3.rings + 1

                        0 ->
                            DistanceRingBuffer.get offset s3.rings

                        other ->
                            other - 2
            in
            if v >= numBlockTypes then
                v - numBlockTypes

            else
                v
    in
    ( s3
    , s3.rings
        |> DistanceRingBuffer.set offset (DistanceRingBuffer.get (offset + 1) s3.rings)
        |> DistanceRingBuffer.set (offset + 1) blockType
    , result
    )


calculateFence : Written -> State -> Int
calculateFence written s =
    -- RingBuffer.size s.ringBuffer
    let
        result =
            RingBuffer.size s.ringBuffer
    in
    {-
       -- RingBuffer.size s.ringBuffer
          if s.flags.isEager then
              min result (written.fromRingBuffer + Constants.outputLength - written.toOutput)

          else
    -}
    min result (written.fromRingBuffer + Constants.outputLength - written.toOutput)
