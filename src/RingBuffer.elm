module RingBuffer exposing (RingBuffer, copyWithin, empty, fromArray, get, maxSize, push, resize, set, setSlice, size, slice, update, updateExpectation)

import Array exposing (Array)
import Array.Helpers
import Bitwise


fromArray a =
    RingBuffer a 0 0 0


resize newMaxRingBufferSize (RingBuffer a b _ d) =
    RingBuffer a b newMaxRingBufferSize d


update i f (RingBuffer array b c d) =
    RingBuffer (Array.Helpers.update i f array) b c d


type RingBuffer
    = RingBuffer
        (Array Int)
        -- size ("allocated", not the actual length of the array
        Int
        -- max size
        Int
        -- expected total size
        Int


set : Int -> Int -> RingBuffer -> RingBuffer
set i value (RingBuffer array b c d) =
    RingBuffer (Array.set i value array) b c d


get : Int -> RingBuffer -> Int
get i (RingBuffer array _ _ _) =
    case Array.get i array of
        Nothing ->
            Debug.todo "unsafe get"

        Just v ->
            v


push : Int -> RingBuffer -> RingBuffer
push value (RingBuffer array b c d) =
    RingBuffer (Array.push value array) b c d


slice : Int -> Int -> RingBuffer -> Array Int
slice from to (RingBuffer array _ _ _) =
    Array.slice from to array


empty max =
    RingBuffer Array.empty 0 max 0


maybeReallocate : { flags | inputEnd : Bool } -> Int -> RingBuffer -> RingBuffer
maybeReallocate { inputEnd } expectedTotalSize ((RingBuffer array ringBufferSize maxRingBufferSize _) as input) =
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

            newBuffer =
                if Array.length array == 0 then
                    Array.repeat ringBufferSizeWithSlack 0

                else
                    Array.append (Array.slice 0 ringBufferSize array) (Array.repeat (ringBufferSizeWithSlack - ringBufferSize) 0)
        in
        RingBuffer newBuffer newSize maxRingBufferSize expectedTotalSize


size (RingBuffer _ s _ _) =
    s


maxSize (RingBuffer _ _ s _) =
    s


updateExpectation flags metaBlockLength ((RingBuffer array ringBufferSize maxRingBufferSize expectedTotalSize) as input) =
    let
        newExpectedTotalSize =
            min (expectedTotalSize + metaBlockLength) (Bitwise.shiftLeftBy 30 1)
    in
    if ringBufferSize < maxRingBufferSize then
        maybeReallocate flags newExpectedTotalSize input

    else
        RingBuffer array ringBufferSize maxRingBufferSize newExpectedTotalSize


copyWithin dest start end (RingBuffer array a b c) =
    let
        copyLength =
            end - start

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
    RingBuffer (go 0 dest start array) a b c


setSlice segment dest (RingBuffer array a b c) =
    RingBuffer (Array.Helpers.setSlice segment dest array) a b c
