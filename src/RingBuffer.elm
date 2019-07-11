module RingBuffer exposing (RingBuffer, copyWithin, empty, fromArray, get, maxSize, push, resize, reslice, set, setSlice, size, slice, update, updateExpectation)

import Array exposing (Array)
import Array.Helpers
import Bitwise


reslice i (RingBuffer a b c d e) =
    RingBuffer (Array.slice 0 i a) b c d (Just a)


fromArray a =
    RingBuffer a 0 0 0 Nothing


resize newMaxRingBufferSize (RingBuffer a b _ d e) =
    RingBuffer a b newMaxRingBufferSize d e


update i f (RingBuffer array b c d e) =
    RingBuffer (Array.Helpers.update i f array) b c d e


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
                            -- Debug.todo ("prev insufficient & unsafe get, asked for " ++ String.fromInt i ++ " but the  length is only " ++ String.fromInt (Array.length array))
                            0

                Nothing ->
                    -- Debug.todo ("no prev & unsafe get, asked for " ++ String.fromInt i ++ " but the  length is only " ++ String.fromInt (Array.length array))
                    0

        Just v ->
            v


push : Int -> RingBuffer -> RingBuffer
push value (RingBuffer array b c d e) =
    RingBuffer (Array.push value array) b c d e


slice : Int -> Int -> RingBuffer -> Array Int
slice from to (RingBuffer array _ _ _ _) =
    Array.slice from to array


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


size (RingBuffer _ s _ _ _) =
    s


maxSize (RingBuffer _ _ s _ _) =
    s


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

        go k currentDst currentSrc accum =
            if k < copyLength then
                go (k + 1)
                    (currentDst + 1)
                    (currentSrc + 1)
                    (let
                        a0 =
                            accum

                        -- a1 = Array.set (currentDst + 0) (Array.Helpers.unsafeGet (currentSrc + 0) a0) a0
                        a1 =
                            Array.push (Array.Helpers.unsafeGet (currentSrc + 0) a0) a0

                        {-
                           a2 =
                               Array.set (currentDst + 1) (Array.Helpers.unsafeGet (currentSrc + 1) a1) a1

                           a3 =
                               Array.set (currentDst + 2) (Array.Helpers.unsafeGet (currentSrc + 2) a2) a2

                           a4 =
                               Array.set (currentDst + 3) (Array.Helpers.unsafeGet (currentSrc + 3) a3) a3
                        -}
                     in
                     -- a4
                     a1
                    )

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
    RingBuffer (go 0 dest start array) a b c e


setSlice segment dest (RingBuffer array a b c e) =
    RingBuffer (Array.Helpers.setSlice segment dest array) a b c e
