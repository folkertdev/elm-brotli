module RingBuffer exposing (RingBuffer, appendRight, copyWithin, empty, get, getAtCurrentPosition, loopAround, maxSize, position, push, set, setMaxSize, size, slice, sliceFoldl, sliceFoldr, unsafeFromArray, update, updateExpectation)

import Array exposing (Array)
import Array.Helpers
import Bitwise


type RingBuffer
    = RingBuffer
        (Array Int)
        -- size ("allocated", not the actual length of the array
        Int
        -- max size
        Int
        -- expected total size
        Int
        (Maybe (Array Int))


getAtCurrentPosition : RingBuffer -> Int
getAtCurrentPosition input =
    get (position input) input


{-| Get the current position in the ring buffer
-}
position : RingBuffer -> Int
position (RingBuffer a b c d e) =
    Array.length a


size : RingBuffer -> Int
size (RingBuffer _ s _ _ _) =
    s


maxSize : RingBuffer -> Int
maxSize (RingBuffer _ _ s _ _) =
    s


{-| The ringbuffer has exceeded its capacity; we reset the position to a lower vlaue
-}
loopAround : Int -> RingBuffer -> RingBuffer
loopAround i (RingBuffer a currentSize c d e) =
    let
        newArray =
            -- copy anything past the "size" to the front
            Array.Helpers.copyWithin 0 currentSize (Array.length a) a
                |> Array.slice 0 i
    in
    RingBuffer newArray currentSize c d (Just a)


{-| turn an array into a ringbuffer. For testing purposes only
-}
unsafeFromArray : Array Int -> RingBuffer
unsafeFromArray a =
    RingBuffer a 0 0 0 Nothing


setMaxSize : Int -> RingBuffer -> RingBuffer
setMaxSize newMaxRingBufferSize (RingBuffer a b _ d e) =
    RingBuffer a b newMaxRingBufferSize d e


appendRight : RingBuffer -> Array Int -> RingBuffer
appendRight (RingBuffer a b c d e) newRight =
    RingBuffer (Array.append a newRight) b c d e


update i f (RingBuffer array b c d e) =
    RingBuffer (Array.Helpers.update i f array) b c d e


set : Int -> Int -> RingBuffer -> RingBuffer
set i value (RingBuffer array b c d e) =
    if i == Array.length array then
        RingBuffer (Array.push value array) b c d e

    else if i > Array.length array then
        -- Debug.todo "unsafe set"
        RingBuffer array b c d e

    else
        RingBuffer (Array.set i value array) b c d e


get : Int -> RingBuffer -> Int
get i (RingBuffer array _ _ _ previous) =
    case Array.get i array of
        Nothing ->
            case previous of
                Just prev ->
                    case Array.get i prev of
                        Just v ->
                            v

                        Nothing ->
                            -- this should never happen, as this is an access outside of the bounds of the ringbuffer
                            0

                Nothing ->
                    -- no previous array, assume the ringbuffer is initially filled with 0's
                    0

        Just v ->
            v


push : Int -> RingBuffer -> RingBuffer
push value (RingBuffer array b c d e) =
    RingBuffer (Array.push value array) b c d e


slice : Int -> Int -> RingBuffer -> Array Int
slice from to (RingBuffer array _ _ _ _) =
    Array.slice from to array


sliceFoldl : Int -> Int -> (Int -> b -> b) -> b -> RingBuffer -> b
sliceFoldl from to folder default (RingBuffer array _ _ _ _) =
    Array.Helpers.sliceFoldl from to folder default array


sliceFoldr : Int -> Int -> (Int -> b -> b) -> b -> RingBuffer -> b
sliceFoldr from to folder default (RingBuffer array _ _ _ _) =
    Array.Helpers.sliceFoldr from to folder default array


empty max =
    RingBuffer Array.empty 0 max 0 Nothing


maybeReallocate : { flags | inputEnd : Bool } -> Int -> RingBuffer -> RingBuffer
maybeReallocate { inputEnd } expectedTotalSize ((RingBuffer array ringBufferSize maxRingBufferSize _ previous) as input) =
    let
        newSize =
            let
                initialSize =
                    maxRingBufferSize
            in
            if initialSize > expectedTotalSize then
                let
                    minimalNewSize =
                        expectedTotalSize

                    calculate1 size_ =
                        if Bitwise.shiftRightBy 1 size_ > minimalNewSize then
                            calculate1 (Bitwise.shiftRightBy 1 size_)

                        else
                            size_

                    calculate2 size_ =
                        if inputEnd == False && size_ < 16384 && maxRingBufferSize >= 16384 then
                            16384

                        else
                            size_
                in
                initialSize |> calculate1 |> calculate2

            else
                initialSize
    in
    if newSize < ringBufferSize then
        input

    else
        let
            ringBufferSizeWithSlack =
                newSize + 37

            {-
               newBuffer =
                   if Array.length array == 0 then
                       Array.repeat ringBufferSizeWithSlack 0

                   else
                       Array.append (Array.slice 0 ringBufferSize array) (Array.repeat (ringBufferSizeWithSlack - ringBufferSize) 0)
            -}
        in
        RingBuffer array newSize maxRingBufferSize expectedTotalSize previous


updateExpectation flags metaBlockLength ((RingBuffer array ringBufferSize maxRingBufferSize expectedTotalSize previous) as input) =
    let
        newExpectedTotalSize =
            min (expectedTotalSize + metaBlockLength) (Bitwise.shiftLeftBy 30 1)
    in
    if ringBufferSize < maxRingBufferSize then
        maybeReallocate flags newExpectedTotalSize input

    else
        RingBuffer array ringBufferSize maxRingBufferSize newExpectedTotalSize previous


copyWithin dest start end (RingBuffer array a b c e) =
    let
        src =
            start

        srcEnd =
            end

        dst =
            dest

        dstEnd =
            dst + copyLength

        copyLength =
            end - start

        go k currentSrc accum =
            if copyLength - k >= 4 then
                go (k + 4)
                    (currentSrc + 4)
                    (let
                        a0 =
                            accum

                        a1 =
                            Array.push (Array.Helpers.unsafeGet (currentSrc + 0) a0) a0

                        a2 =
                            Array.push (Array.Helpers.unsafeGet (currentSrc + 1) a1) a1

                        a3 =
                            Array.push (Array.Helpers.unsafeGet (currentSrc + 2) a2) a2

                        a4 =
                            Array.push (Array.Helpers.unsafeGet (currentSrc + 3) a3) a3
                     in
                     a4
                    )

            else if k < copyLength then
                go (k + 1) (currentSrc + 1) (Array.push (Array.Helpers.unsafeGet (currentSrc + 0) accum) accum)

            else
                accum
    in
    {- slower, probably because of iteration
       if srcEnd < dst || src > dstEnd then
           let
               segment =
                   Array.slice start end array
           in
           RingBuffer (Array.foldl Array.push array segment) a b c e

       else
    -}
    RingBuffer (go 0 start array) a b c e


setSlice segment dest (RingBuffer array a b c e) =
    RingBuffer (Array.Helpers.setSlice segment dest array) a b c e
