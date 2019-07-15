module State exposing (BlockLength, ContextLookup(..), Distance, Error(..), Flags, InputStream, Num, State, TreeGroup, calculateDistanceAlphabetLimit, calculateDistanceAlphabetSize, checkHealth, contextLookupOffset1, contextLookupOffset2, copyState, copyState7, defaultState, doReadMoreInput, doReadMoreInputHelp, halfAvailable, initBitReader, initState, jumpToByteBoundary, log2floor, maybeReadMoreInput, prepare, pureReadFewBits, putOnAccumulator, readBits, readBits2, readFewBits, readFewBitsSafe, readInput, readManyBits, reload, topUpAccumulator, topUpPure, updateAccumulator, updateBitOffset, updateEvaluateState4, updateEvaluateState8, updateEvaluateState9, updateRemainder7)

import Array exposing (Array)
import Array.Helpers
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import DistanceRingBuffer exposing (DistanceRingBuffer)
import RingBuffer exposing (RingBuffer)


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


type alias InputStream =
    { offset : Int
    , buffer : Bytes
    }


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
    { ringBuffer : RingBuffer
    , contextModes : Array Int
    , contextMap : Array Int
    , distContextMap : Array Int
    , distExtraBits : Array Int
    , byteBuffer : Array Int
    , shortBuffer : Array Int
    , rings : DistanceRingBuffer
    , blockTrees : Array Int
    , treeGroup : TreeGroup
    , distOffset : Array Int
    , accumulator32 : Int
    , bitOffset : Int
    , halfOffset : Int
    , tailBytes : Int
    , metaBlockLength : Int
    , num : Num
    , blockLength : BlockLength
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
    , input : InputStream
    , flags : Flags
    }


type ContextLookup
    = ContextLookup Int


contextLookupOffset1 : ContextLookup -> Int
contextLookupOffset1 (ContextLookup v) =
    v


contextLookupOffset2 : ContextLookup -> Int
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
    { ringBuffer = RingBuffer.empty 0
    , contextModes = Array.empty
    , contextMap = Array.empty
    , distContextMap = Array.empty
    , distExtraBits = Array.empty
    , byteBuffer = Array.empty
    , shortBuffer = Array.empty
    , rings =
        DistanceRingBuffer.empty 10
            |> DistanceRingBuffer.push 16
            |> DistanceRingBuffer.push 15
            |> DistanceRingBuffer.push 11
            |> DistanceRingBuffer.push 4
    , blockTrees = Array.empty
    , treeGroup =
        { literalTreeGroup = Array.empty
        , commandTreeGroup = Array.empty
        , distanceTreeGroup = Array.empty
        }
    , distOffset = Array.empty
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
    , maxDistance = 0
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
    , input = { offset = 0, buffer = buffer }
    , flags = defaultFlags
    }


initState : State -> Result Error ( Int, Int, State )
initState s =
    let
        maxDistanceAlphabetLimit =
            544
    in
    { s
        | blockTrees = Array.append (Array.fromList [ 7 ]) (Array.repeat 3090 0)
        , rings = DistanceRingBuffer.setPosition 3 s.rings
        , distExtraBits = Array.repeat maxDistanceAlphabetLimit 0
        , distOffset = Array.repeat maxDistanceAlphabetLimit 0
    }
        |> initBitReader
        |> Result.map (\st -> ( 1, 0, st ))


calculateDistanceAlphabetSize : Int -> Int -> Int -> Int
calculateDistanceAlphabetSize npostfix ndirect maxndistbits =
    16 + ndirect + 2 * Bitwise.shiftLeftBy npostfix maxndistbits


{-| The result of this function is hardcoded in `initState`. It was previously called with

    calculateDistanceAlphabetLimit 0x7FFFFFFC 3 (Bitwise.shiftLeftBy 3 15)

-}
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
        | accumulator32 = 0
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
            case Array.get s.halfOffset s.shortBuffer of
                Nothing ->
                    0

                Just value ->
                    Bitwise.shiftLeftBy 16 value
    in
    updateAccumulator (s.bitOffset - 16) (s.halfOffset + 1) (Bitwise.or next (Bitwise.shiftRightZfBy 16 s.accumulator32)) s


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
                -- Array.Helpers.copyWithin 0 readOffset 4096 s.byteBuffer
                Array.slice readOffset 4096 s.byteBuffer
        in
        -- @optimize replace record update?
        case doReadMoreInputHelp bytesInBuffer { s | byteBuffer = byteBuffer, halfOffset = 0 } of
            Err e ->
                Err e

            Ok ( s2, bytesInBuffer2 ) ->
                -- can't we just read them as shorts?
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
                -- go (i + 1) (Array.set i value shortBuffer)
                go (i + 1) (Array.push value shortBuffer)

            else
                shortBuffer
    in
    -- { s | shortBuffer = go 0 s.shortBuffer }
    { s | shortBuffer = go 0 Array.empty }


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
            Decode.map2 (\_ v -> v) (Decode.bytes s.input.offset) (Array.Helpers.decodeByteArrayLowLevel bytesRead s.byteBuffer)
    in
    case Decode.decode decoder s.input.buffer of
        Just newByteBuffer ->
            let
                oldInput =
                    s.input

                newInput =
                    { oldInput | offset = oldInput.offset + bytesRead }
            in
            Ok ( { s | input = newInput, byteBuffer = newByteBuffer }, bytesRead )

        Nothing ->
            Err (CustomError "readInput: insufficient input")


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
        readManyBits nbits state


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
    case readFewBitsSafe 16 inputS of
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


updateAccumulator : Int -> Int -> Int -> State -> State
updateAccumulator newBitOffset newHalfOffset newAccumulator32 orig =
    { ringBuffer = orig.ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , accumulator32 = newAccumulator32
    , bitOffset = newBitOffset
    , halfOffset = newHalfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = orig.metaBlockLength
    , num = orig.num
    , blockLength = orig.blockLength
    , maxDistance = orig.maxDistance
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
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , accumulator32 = orig.accumulator32
    , bitOffset = newBitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = orig.metaBlockLength
    , num = orig.num
    , blockLength = orig.blockLength
    , maxDistance = orig.maxDistance
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
    , flags = orig.flags
    , input = orig.input
    }


updateEvaluateState4 : Int -> Int -> Int -> Int -> BlockLength -> Int -> DistanceRingBuffer -> State -> State
updateEvaluateState4 j copyLength insertLength distanceCode blockLength commandTreeIdx rings orig =
    { ringBuffer = orig.ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , rings = rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = orig.metaBlockLength
    , num = orig.num
    , blockLength = blockLength
    , maxDistance = orig.maxDistance
    , isTrivialLiteralContext = orig.isTrivialLiteralContext
    , literalTreeIdx = orig.literalTreeIdx
    , commandTreeIdx = commandTreeIdx
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
    , flags = orig.flags
    , input = orig.input
    }


copyState7 : Int -> Int -> RingBuffer -> Int -> State -> State
copyState7 bitOffset newJ newRingBuffer literalBlockLength orig =
    { ringBuffer = newRingBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
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
    , maxDistance = orig.maxDistance
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
    , flags = orig.flags
    , input = orig.input
    }


updateRemainder7 : Int -> Int -> Int -> Int -> Int -> DistanceRingBuffer -> State -> State
updateRemainder7 distance maxDistance distanceBlockLength metaBlockLength j rings orig =
    { ringBuffer = orig.ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , rings = rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
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
    , maxDistance = maxDistance
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
    , flags = orig.flags
    , input = orig.input
    }


updateEvaluateState8 : RingBuffer -> Int -> Int -> State -> State
updateEvaluateState8 ringBuffer j metaBlockLength orig =
    { ringBuffer = ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = metaBlockLength
    , num = orig.num
    , blockLength = orig.blockLength
    , maxDistance = orig.maxDistance
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
    , flags = orig.flags
    , input = orig.input
    }


updateEvaluateState9 : RingBuffer -> Int -> State -> State
updateEvaluateState9 ringBuffer metaBlockLength orig =
    { ringBuffer = ringBuffer
    , contextModes = orig.contextModes
    , contextMap = orig.contextMap
    , distContextMap = orig.distContextMap
    , distExtraBits = orig.distExtraBits
    , byteBuffer = orig.byteBuffer
    , shortBuffer = orig.shortBuffer
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = metaBlockLength
    , num = orig.num
    , blockLength = orig.blockLength
    , maxDistance = orig.maxDistance
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
    , rings = orig.rings
    , blockTrees = orig.blockTrees
    , treeGroup = orig.treeGroup
    , distOffset = orig.distOffset
    , accumulator32 = orig.accumulator32
    , bitOffset = orig.bitOffset
    , halfOffset = orig.halfOffset
    , tailBytes = orig.tailBytes
    , metaBlockLength = orig.metaBlockLength
    , num = orig.num
    , blockLength = orig.blockLength
    , maxDistance = orig.maxDistance
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
    , flags = orig.flags
    , input = orig.input
    }
