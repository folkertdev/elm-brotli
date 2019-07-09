module Transforms exposing (Transforms, new, rfc_transforms, transformDictionaryWord)

import Array exposing (Array)
import Array.Helpers
import Bitwise


rfc_transforms : Transforms
rfc_transforms =
    new 121 167 50
        |> unpackTransforms rfcPrefixSuffixSrc rfcTransformsSrc


type alias Transforms =
    { numTransforms : Int
    , triplets : Array Triplet
    , prefixSuffixStorage : Array Int
    , prefixSuffixHeads : Array Int
    , params : Array Int
    }


type alias Triplet =
    { prefixIdx : Int
    , transformType : Int
    , suffixIdx : Int
    }


new : Int -> Int -> Int -> Transforms
new numTransforms prefixSuffixLen prefixSuffixCount =
    { numTransforms = numTransforms
    , params = Array.repeat numTransforms 0
    , triplets = Array.repeat numTransforms (Triplet 0 0 0)
    , prefixSuffixStorage = Array.repeat prefixSuffixLen 0
    , prefixSuffixHeads = Array.repeat (prefixSuffixCount + 1) 0
    }


{-| Transform words

This adds a suffix and/or prefix, and copies a subsection of `src` into `dst`.
The transformations are described [here](https://tools.ietf.org/html/rfc7932#page-124), some examples are:

           5           ""     Identity            " the "
           6          " "     Identity                 ""
           7         "s "     Identity                " "
           8           ""     Identity             " of "
           9           ""     FermentFirst             ""
          10           ""     Identity            " and "

You can see that the brotli spec is really geared towards English text.

-}
transformDictionaryWord : Array Int -> Int -> Array Int -> Int -> Int -> Transforms -> Int -> ( Array Int, Int )
transformDictionaryWord dst dstOffset src srcOffset_ len transforms transformIndex =
    let
        offset =
            dstOffset

        prefixSuffixStorage =
            transforms.prefixSuffixStorage

        prefixSuffixHeads =
            transforms.prefixSuffixHeads

        { prefixIdx, transformType, suffixIdx } =
            case Array.get transformIndex transforms.triplets of
                Nothing ->
                    Triplet 0 0 0

                Just v ->
                    v

        prefix =
            Array.Helpers.unsafeGet prefixIdx prefixSuffixHeads

        prefixEnd =
            Array.Helpers.unsafeGet (prefixIdx + 1) prefixSuffixHeads

        suffix =
            Array.Helpers.unsafeGet suffixIdx prefixSuffixHeads

        suffixEnd =
            Array.Helpers.unsafeGet (suffixIdx + 1) prefixSuffixHeads

        omitFirst =
            let
                temp =
                    transformType - 11
            in
            (if temp < 1 || temp > 9 then
                0

             else
                temp
            )
                |> (\temp2 ->
                        if temp2 > len then
                            len

                        else
                            temp2
                   )

        omitLast =
            let
                temp =
                    transformType - 0
            in
            if temp < 1 || temp > 9 then
                0

            else
                temp

        srcOffset =
            srcOffset_ + omitFirst

        len1 =
            len - (omitFirst + omitLast)

        go1 i currentOffset currentSourceOffset accum =
            if i > 0 then
                go1 (i - 1) (currentOffset + 1) (currentSourceOffset + 1) (Array.set currentOffset (Array.Helpers.unsafeGet currentSourceOffset src) accum)

            else
                ( currentOffset, currentSourceOffset, accum )

        prefixSuffixLoop currentSuffix fixEnd currentOffset accum =
            if currentSuffix < fixEnd then
                case Array.get currentSuffix prefixSuffixStorage of
                    Nothing ->
                        ( currentOffset, accum )

                    Just value ->
                        prefixSuffixLoop (currentSuffix + 1) fixEnd (currentOffset + 1) (Array.set currentOffset value accum)

            else
                ( currentOffset, accum )

        applyTransform1 inputLen_ currentOffset =
            let
                initialUppercaseOffset =
                    currentOffset - inputLen_

                go currentLen uppercaseOffset accum =
                    if currentLen > 0 then
                        let
                            c0 =
                                Array.Helpers.unsafeGet uppercaseOffset accum
                        in
                        if c0 < 0xC0 then
                            go (currentLen - 1)
                                (uppercaseOffset + 1)
                                (if c0 >= 97 && c0 <= 122 then
                                    Array.Helpers.update uppercaseOffset (\v -> Bitwise.xor v 32) accum

                                 else
                                    accum
                                )

                        else if c0 < 0x0E then
                            go (currentLen - 2) (uppercaseOffset + 2) (Array.Helpers.update (uppercaseOffset + 1) (\v -> Bitwise.xor v 32) accum)

                        else
                            go (currentLen - 3) (uppercaseOffset + 3) (Array.Helpers.update (uppercaseOffset + 2) (\v -> Bitwise.xor v 5) accum)

                    else
                        accum
            in
            go
                (if transformType == 10 then
                    1

                 else
                    inputLen_
                )
                initialUppercaseOffset

        applyTransform2 inputLen_ currentOffset =
            let
                param =
                    Array.Helpers.unsafeGet transformIndex transforms.params

                go currentLength shiftOffset scalar accum =
                    if currentLength > 0 then
                        let
                            c0 =
                                Array.Helpers.unsafeGet shiftOffset accum |> Bitwise.and 0xFF

                            result =
                                transform20Helper c0 scalar currentLength shiftOffset accum

                            newLen =
                                if transformType == 21 then
                                    0

                                else
                                    currentLength - result.step
                        in
                        go newLen (shiftOffset + result.step) result.scalar result.dst

                    else
                        accum
            in
            go inputLen_ (currentOffset - inputLen_) (Bitwise.and param 0x7FFF + (0x01000000 - Bitwise.and param 0x8000))
    in
    prefixSuffixLoop prefix prefixEnd offset dst
        |> (\( newOffset, newAccum ) -> go1 len1 newOffset srcOffset newAccum)
        |> (\( newOffset, _, newAccum ) ->
                if transformType == 10 || transformType == 11 then
                    ( newOffset, applyTransform1 len1 newOffset newAccum )

                else if transformType == 21 || transformType == 22 then
                    ( newOffset, applyTransform2 len1 newOffset newAccum )

                else
                    ( newOffset, newAccum )
           )
        |> (\( newOffset, newAccum ) -> prefixSuffixLoop suffix suffixEnd newOffset newAccum)
        |> (\( newOffset, newAccum ) -> ( newAccum, newOffset - dstOffset ))


transform20Helper : Int -> Int -> Int -> Int -> Array Int -> { scalar : Int, step : Int, dst : Array Int }
transform20Helper c0 scalar len shiftOffset dst =
    if c0 < 0x80 then
        { scalar = scalar + c0
        , step = 1
        , dst = Array.set shiftOffset (Bitwise.and scalar 0x7F) dst
        }

    else if c0 < 0xC0 then
        -- does nothing
        { scalar = scalar, step = 1, dst = dst }

    else if c0 < 0xE0 then
        if len >= 2 then
            let
                c1 =
                    Array.Helpers.unsafeGet (shiftOffset + 1) dst

                newScalar =
                    scalar + Bitwise.or (Bitwise.and c1 0x3F) (Bitwise.shiftLeftBy 6 (Bitwise.and c0 0x1F))
            in
            { scalar = newScalar
            , dst =
                dst
                    |> Array.set shiftOffset (Bitwise.or 0xC0 (Bitwise.and (Bitwise.shiftRightBy 6 scalar) 0x1F))
                    |> Array.set (shiftOffset + 1) (Bitwise.or (Bitwise.and c1 0xC0) (Bitwise.and scalar 0x3F))
            , step = 2
            }

        else
            { scalar = scalar, step = len, dst = dst }

    else if c0 < 0xF0 then
        if len >= 3 then
            let
                c1 =
                    Array.Helpers.unsafeGet (shiftOffset + 1) dst

                c2 =
                    Array.Helpers.unsafeGet (shiftOffset + 2) dst

                newScalar =
                    scalar + Bitwise.and c2 0x3F |> Bitwise.or (Bitwise.shiftLeftBy 6 (Bitwise.and c1 0x3F)) |> Bitwise.or (Bitwise.shiftLeftBy 12 (Bitwise.and c0 0x0F))
            in
            { scalar = newScalar
            , dst =
                dst
                    |> Array.set shiftOffset (Bitwise.or 0xE0 (Bitwise.and (Bitwise.shiftRightBy 12 scalar) 0x0F))
                    |> Array.set (shiftOffset + 1) (Bitwise.or (Bitwise.and c1 0xC0) (Bitwise.and (Bitwise.shiftRightBy 6 scalar) 0x3F))
                    |> Array.set (shiftOffset + 1) (Bitwise.or (Bitwise.and c2 0xC0) (Bitwise.and scalar 0x3F))
            , step = 3
            }

        else
            { scalar = scalar, step = len, dst = dst }

    else if c0 < 0xF8 then
        if len >= 4 then
            let
                c1 =
                    Array.Helpers.unsafeGet (shiftOffset + 1) dst

                c2 =
                    Array.Helpers.unsafeGet (shiftOffset + 2) dst

                c3 =
                    Array.Helpers.unsafeGet (shiftOffset + 3) dst

                newScalar =
                    scalar
                        + (Bitwise.and c3 0x3F
                            |> Bitwise.or (Bitwise.shiftLeftBy 6 (c2 |> Bitwise.and 0x3F))
                            |> Bitwise.or (Bitwise.shiftLeftBy 12 (c1 |> Bitwise.and 0x3F))
                            |> Bitwise.or (Bitwise.shiftLeftBy 18 (c0 |> Bitwise.and 0x07))
                          )
            in
            { scalar = newScalar
            , dst =
                dst
                    |> Array.set shiftOffset (Bitwise.or 0xF0 ((scalar |> Bitwise.shiftRightBy 18) |> Bitwise.and 0x07))
                    |> Array.set (shiftOffset + 1) (Bitwise.or (Bitwise.and c1 0xC0) ((scalar |> Bitwise.shiftRightBy 12) |> Bitwise.and 0x3F))
                    |> Array.set (shiftOffset + 2) (Bitwise.or (Bitwise.and c2 0xC0) ((scalar |> Bitwise.shiftRightBy 6) |> Bitwise.and 0x3F))
                    |> Array.set (shiftOffset + 3) (Bitwise.or (Bitwise.and c3 0xC0) (scalar |> Bitwise.and 0x3F))
            , step = 4
            }

        else
            { scalar = scalar, step = len, dst = dst }

    else
        { scalar = scalar, step = 1, dst = dst }


unpackTransforms : Array Int -> Array Int -> Transforms -> Transforms
unpackTransforms prefixSuffixSrc transformsSrc transforms =
    let
        n =
            Array.length prefixSuffixSrc

        go i j index accum headsAccum =
            if i < n then
                let
                    c =
                        Array.Helpers.unsafeGet i prefixSuffixSrc
                in
                if c == 35 then
                    go (i + 1) j (index + 1) accum (Array.set index j headsAccum)

                else
                    go (i + 1) (j + 1) index (Array.set j c accum) headsAccum

            else
                ( accum, headsAccum )

        go2 i currentTransforms =
            if i < (3 * 121) then
                let
                    triplet =
                        Triplet
                            (Array.Helpers.unsafeGet i transformsSrc - 32)
                            (Array.Helpers.unsafeGet (i + 1) transformsSrc - 32)
                            (Array.Helpers.unsafeGet (i + 2) transformsSrc - 32)
                in
                go2 (i + 3) (Array.set (i // 3) triplet currentTransforms)

            else
                currentTransforms

        newTriplets =
            go2 0 transforms.triplets

        ( newPrefixSuffixStorage, newPrefixSuffixHeads ) =
            go 0 0 1 transforms.prefixSuffixStorage transforms.prefixSuffixHeads
    in
    { transforms
        | triplets = newTriplets
        , prefixSuffixStorage = newPrefixSuffixStorage
        , prefixSuffixHeads = newPrefixSuffixHeads
    }




rfcPrefixSuffixSrc : Array Int 
rfcPrefixSuffixSrc = 
    Array.fromList [35,32,35,115,32,35,44,32,35,101,32,35,46,35,32,116,104,101,32,35,46,99,111,109,47,35,194,160,35,32,111,102,32,35,32,97,110,100,32,35,32,105,110,32,35,32,116,111,32,35,34,35,34,62,35,10,35,93,35,32,102,111,114,32,35,32,97,32,35,32,116,104,97,116,32,35,46,32,35,32,119,105,116,104,32,35,39,35,32,102,114,111,109,32,35,32,98,121,32,35,46,32,84,104,101,32,35,32,111,110,32,35,32,97,115,32,35,32,105,115,32,35,105,110,103,32,35,10,9,35,58,35,101,100,32,35,40,35,32,97,116,32,35,108,121,32,35,61,34,35,32,111,102,32,116,104,101,32,35,46,32,84,104,105,115,32,35,44,35,32,110,111,116,32,35,101,114,32,35,97,108,32,35,61,39,35,102,117,108,32,35,105,118,101,32,35,108,101,115,115,32,35,101,115,116,32,35,105,122,101,32,35,111,117,115,32,35]



rfcTransformsSrc : Array Int 
rfcTransformsSrc =
    Array.fromList

    [32,32,32,32,32,33,33,32,33,32,44,32,32,42,33,32,32,38,33,32,32,34,32,33,32,32,41,32,42,32,32,32,42,32,45,32,32,33,32,35,32,33,32,32,35,33,42,33,32,32,43,32,32,44,36,32,33,32,32,45,32,32,37,32,32,46,32,32,47,32,35,32,32,32,48,32,32,49,32,46,32,32,34,32,32,32,50,32,32,51,33,42,32,32,32,52,37,32,32,33,32,35,32,47,32,32,32,53,32,32,54,32,32,55,32,32,56,32,48,32,32,49,32,38,32,32,32,36,32,32,32,57,32,43,32,32,32,58,32,32,59,32,32,60,32,39,32,32,33,61,32,32,62,32,32,63,33,32,52,32,32,64,32,52,32,32,50,32,32,38,32,32,32,65,32,42,35,32,40,32,32,32,66,32,32,67,38,32,41,32,37,32,32,41,32,33,42,35,32,42,45,37,32,65,32,43,33,32,42,46,32,32,68,33,32,37,39,32,32,38,32,69,32,42,54,32,32,70,32,32,71,37,32,33,32,42,65,32,42,37,32,32,72,33,32,68,32,32,73,33,43,33,32,32,74,33,43,32,32,32,75,32,43,45,32,42,52,33,32,65,32,32,76,33,42,52,32,32,77,32,32,78,32,43,54,32,32,79,33,42,37,32,43,46,33,32,75,32,42,71,32,32,80,32,43,37,40,32,32,33,32,71,32,42,68,32,43,68,32,32,81,32,43,35,32,42,75,33,42,71,33,43,68,33,43,35,32,43,71,32,43,65,32,43,52,33,43,37,32,43,75,33,43,52,33,42,68,33,43,75,33,42,75]


