module Array.Helpers exposing (unsafeGet, setSlice, inverseMoveToFrontTransform, fill, update, moveToFront)

import Array exposing (Array)
import Bitwise



unsafeGet : Int -> Array Int -> Int
unsafeGet i arr =
    case Array.get i arr of
        Nothing ->
            0

        Just v ->
            v


setSlice  : Array a -> Int -> Array a -> Array a
setSlice slice startIndex whole =
    let
        before =
            Array.slice 0 startIndex whole

        after =
            Array.slice (startIndex + Array.length slice) (Array.length whole) whole

        result =
            Array.append before (Array.append slice after)
    in
    result

moveToFront : Int -> Array Int -> Array Int
moveToFront initialIndex v =
    -- @optimize optimize with slice?
    -- probably not worth it
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


inverseMoveToFrontTransform : Array Int -> Int -> Array Int
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
    go 0 (Array.initialize 256 identity) v


{-| TODO test
<https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fill>
-}
fill : a -> Int -> Int -> Array a -> Array a
fill value start end arr =
    if start < end then
        fill value (start + 1) end (Array.set start value arr)

    else
        arr


update : Int -> (a -> a) -> Array a -> Array a
update index f arr =
    case Array.get index arr of
        Nothing ->
            arr

        Just v ->
            Array.set index (f v) arr
