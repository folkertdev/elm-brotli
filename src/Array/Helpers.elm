module Array.Helpers exposing (copyWithin, decodeArray, decodeByteArray, decodeByteArrayLowLevel, decodeShortArray, fasterEncodeFolderL, fasterEncodeFolderR, fasterEncodeL, fasterEncodeR, fill, hasDuplicates, inverseMoveToFrontTransform, moveToFront, replicateValue, setSlice, sliceFoldl, sliceFoldr, unsafeGet, update)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode
import Set


sliceFoldl : Int -> Int -> (a -> b -> b) -> b -> Array a -> b
sliceFoldl start end folder default array =
    -- 12 is the cutoff point in chrome, it is actually lower in FF
    -- but in practice this seems slower (maybe because of GC)
    let
        n =
            12
    in
    if end - start >= n then
        Array.slice start end array |> Array.foldl folder default

    else
        sliceFoldLManualLoop start end folder default array


sliceFoldLManualLoop : Int -> Int -> (a -> b -> b) -> b -> Array a -> b
sliceFoldLManualLoop i n folder accum data =
    if i < n then
        case Array.get i data of
            Nothing ->
                accum

            Just value ->
                sliceFoldLManualLoop (i + 1) n folder (folder value accum) data

    else
        accum


sliceFoldr : Int -> Int -> (a -> b -> b) -> b -> Array a -> b
sliceFoldr start end folder default array =
    -- 12 is the cutoff point in chrome, it is actually lower in FF
    let
        n =
            12 * 10000
    in
    if end - start >= n then
        Array.slice start end array |> Array.foldr folder default

    else
        sliceFoldRManualLoop start (end - 1) folder default array


sliceFoldRManualLoop : Int -> Int -> (a -> b -> b) -> b -> Array a -> b
sliceFoldRManualLoop lower current folder accum data =
    if current >= lower then
        case Array.get current data of
            Nothing ->
                accum

            Just value ->
                sliceFoldRManualLoop lower (current - 1) folder (folder value accum) data

    else
        accum


fasterEncodeR ( bytesOnAccum, accum, otherEncoders ) =
    let
        encoders =
            case bytesOnAccum of
                0 ->
                    otherEncoders

                1 ->
                    Encode.unsignedInt8 accum :: otherEncoders

                2 ->
                    Encode.unsignedInt16 BE accum :: otherEncoders

                _ ->
                    let
                        firstByte =
                            Bitwise.and 0xFF accum

                        otherBytes =
                            Bitwise.shiftRightBy 8 accum
                    in
                    Encode.unsignedInt16 BE otherBytes :: Encode.unsignedInt8 firstByte :: otherEncoders
    in
    encoders


fasterEncodeFolderR byte ( bytesOnAccum, accum, encoders ) =
    case bytesOnAccum of
        0 ->
            ( 1, Bitwise.and 0xFF byte, encoders )

        1 ->
            let
                value =
                    Bitwise.or accum
                        (Bitwise.shiftLeftBy 8 (Bitwise.and 0xFF byte))
            in
            ( 2, value, encoders )

        2 ->
            let
                value =
                    Bitwise.or accum
                        (Bitwise.shiftLeftBy 16 (Bitwise.and 0xFF byte))
            in
            ( 3, value, encoders )

        _ ->
            let
                value =
                    Bitwise.or accum
                        (Bitwise.shiftLeftBy 24 (Bitwise.and 0xFF byte))
            in
            ( 0, 0, Encode.unsignedInt32 BE value :: encoders )


fasterEncodeL ( bytesOnAccum, accum, otherEncoders ) =
    let
        encoders =
            case bytesOnAccum of
                0 ->
                    otherEncoders

                1 ->
                    Encode.unsignedInt8 accum :: otherEncoders

                2 ->
                    Encode.unsignedInt16 BE accum :: otherEncoders

                _ ->
                    let
                        firstByte =
                            Bitwise.and 0xFF accum

                        otherBytes =
                            Bitwise.shiftRightBy 8 accum
                    in
                    Encode.unsignedInt8 firstByte :: Encode.unsignedInt16 BE otherBytes :: otherEncoders
    in
    encoders


fasterEncodeFolderL byte ( bytesOnAccum, accum, encoders ) =
    case bytesOnAccum of
        0 ->
            ( 1, Bitwise.and 0xFF byte, encoders )

        1 ->
            let
                value =
                    Bitwise.or (Bitwise.shiftLeftBy 8 accum) (Bitwise.and 0xFF byte)
            in
            ( 2, value, encoders )

        2 ->
            let
                value =
                    Bitwise.or (Bitwise.shiftLeftBy 8 accum) (Bitwise.and 0xFF byte)
            in
            ( 3, value, encoders )

        _ ->
            let
                value =
                    Bitwise.or (Bitwise.shiftLeftBy 8 accum) (Bitwise.and 0xFF byte)
            in
            ( 0, 0, Encode.unsignedInt32 BE value :: encoders )


{-| TODO test
-}
replicateValue : Array a -> Int -> Int -> Int -> a -> Array a
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


hasDuplicates : Array comparable -> Bool
hasDuplicates =
    Array.toList >> hasDuplicatesList


hasDuplicatesList : List comparable -> Bool
hasDuplicatesList list =
    let
        go seen remaining =
            case remaining of
                [] ->
                    False

                x :: xs ->
                    if Set.member x seen then
                        False

                    else
                        go (Set.insert x seen) xs
    in
    go Set.empty list


decodeShortArray : Int -> Decoder (Array Int)
decodeShortArray n =
    Decode.loop ( n, Array.empty ) decodeShortArrayHelp


decodeShortArrayHelp : ( Int, Array Int ) -> Decoder (Decode.Step ( Int, Array Int ) (Array Int))
decodeShortArrayHelp ( remaining, accum ) =
    if remaining >= 4 then
        Decode.unsignedInt32 BE
            |> Decode.map
                (\new ->
                    let
                        short1 =
                            Bitwise.shiftRightBy 16 new

                        short2 =
                            new

                        newAccum =
                            accum
                                |> Array.push short1
                                |> Array.push short2
                    in
                    Decode.Loop ( remaining - 4, newAccum )
                )

    else if remaining >= 2 then
        Decode.unsignedInt16 BE |> Decode.map (\new -> Decode.Loop ( remaining - 2, Array.push new accum ))

    else if remaining > 0 then
        Decode.unsignedInt8 |> Decode.map (\new -> Decode.Loop ( remaining - 1, Array.push (Bitwise.shiftLeftBy 8 new) accum ))

    else
        Decode.succeed (Decode.Done accum)


decodeByteArray : Int -> Decoder (Array Int)
decodeByteArray n =
    Decode.loop ( n, Array.empty ) decodeByteArrayHelp


decodeByteArrayLowLevel : Int -> Array Int -> Decoder (Array Int)
decodeByteArrayLowLevel n initial =
    Decode.loop ( n, initial ) decodeByteArrayHelp


decodeByteArrayHelp : ( Int, Array Int ) -> Decoder (Decode.Step ( Int, Array Int ) (Array Int))
decodeByteArrayHelp ( remaining, accum ) =
    if remaining >= 4 then
        Decode.unsignedInt32 BE
            |> Decode.map
                (\new ->
                    let
                        byte1 =
                            Bitwise.shiftRightBy 24 new

                        byte2 =
                            Bitwise.shiftRightBy 16 new
                                |> Bitwise.and 0xFF

                        byte3 =
                            Bitwise.shiftRightBy 8 new
                                |> Bitwise.and 0xFF

                        byte4 =
                            Bitwise.and 0xFF new

                        newAccum =
                            accum
                                |> Array.push byte1
                                |> Array.push byte2
                                |> Array.push byte3
                                |> Array.push byte4
                    in
                    Decode.Loop ( remaining - 4, newAccum )
                )

    else if remaining > 0 then
        Decode.unsignedInt8 |> Decode.map (\new -> Decode.Loop ( remaining - 1, Array.push new accum ))

    else
        Decode.succeed (Decode.Done accum)


decodeArray : Int -> Decoder a -> Decoder (Array a)
decodeArray n decoder =
    Decode.loop ( n, Array.empty ) (decodeArrayHelp decoder)


decodeArrayHelp : Decoder a -> ( Int, Array a ) -> Decoder (Decode.Step ( Int, Array a ) (Array a))
decodeArrayHelp decoder ( remaining, accum ) =
    if remaining > 0 then
        decoder |> Decode.map (\new -> Decode.Loop ( remaining - 1, Array.push new accum ))

    else
        Decode.succeed (Decode.Done accum)


{-| TODO write tests
<https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/copyWithin>
-}
copyWithin : Int -> Int -> Int -> Array a -> Array a
copyWithin target start end array =
    let
        len =
            Array.length array
                |> Bitwise.shiftRightZfBy 0

        relativeTarget =
            target
                |> Bitwise.shiftRightBy 0

        to =
            if relativeTarget < 0 then
                max (len + relativeTarget) 0

            else
                min relativeTarget len

        relativeStart =
            start
                |> Bitwise.shiftRightBy 0

        from =
            if relativeStart < 0 then
                max (len + relativeStart) 0

            else
                min relativeStart len

        relativeEnd =
            end
                |> Bitwise.shiftRightBy 0

        final =
            if relativeEnd < 0 then
                max (len + relativeEnd) 0

            else
                min relativeEnd len

        count =
            min (final - from) (len - to)
    in
    if count == 0 then
        array

    else
        copyWithinHelp from to count array


copyWithinHelp from_ to_ count array =
    let
        ( direction, from, to ) =
            if from_ < to_ && to_ < (from_ + count) then
                ( -1, from_ + count - 1, to_ + count - 1 )

            else
                ( 1, from_, to_ )
    in
    copyWithinLoop count from to direction array


copyWithinLoop : Int -> Int -> Int -> Int -> Array a -> Array a
copyWithinLoop count from to direction array =
    -- benchmarks show that unfolding the loop is a little faster
    -- e.g. updating 4 or 8 elements at once, that saves a jump
    if count > 0 then
        case Array.get from array of
            Nothing ->
                copyWithinLoop (count - 1) (from + direction) (to + direction) direction array

            Just value ->
                copyWithinLoop (count - 1) (from + direction) (to + direction) direction (Array.set to value array)

    else
        array


unsafeGet : Int -> Array Int -> Int
unsafeGet i arr =
    case Array.get i arr of
        Nothing ->
            0

        Just v ->
            v


setSlice : Array a -> Int -> Array a -> Array a
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
