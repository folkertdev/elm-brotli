module Transforms exposing (Transforms, new, rfc_transforms, transformDictionaryWord)

import Array exposing (Array)
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
            unsafeGet prefixIdx prefixSuffixHeads

        prefixEnd =
            unsafeGet (prefixIdx + 1) prefixSuffixHeads

        suffix =
            unsafeGet suffixIdx prefixSuffixHeads

        suffixEnd =
            unsafeGet (suffixIdx + 1) prefixSuffixHeads

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
                go1 (i - 1) (currentOffset + 1) (currentSourceOffset + 1) (Array.set currentOffset (unsafeGet currentSourceOffset src) accum)

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
                                unsafeGet uppercaseOffset accum
                        in
                        if c0 < 0xC0 then
                            go (currentLen - 1)
                                (uppercaseOffset + 1)
                                (if c0 >= 97 && c0 <= 122 then
                                    update uppercaseOffset (\v -> Bitwise.xor v 32) accum

                                 else
                                    accum
                                )

                        else if c0 < 0x0E then
                            go (currentLen - 2) (uppercaseOffset + 2) (update (uppercaseOffset + 1) (\v -> Bitwise.xor v 32) accum)

                        else
                            go (currentLen - 3) (uppercaseOffset + 3) (update (uppercaseOffset + 2) (\v -> Bitwise.xor v 5) accum)

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
                    unsafeGet transformIndex transforms.params

                go currentLength shiftOffset scalar accum =
                    if currentLength > 0 then
                        let
                            c0 =
                                unsafeGet shiftOffset accum |> Bitwise.and 0xFF

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
                    unsafeGet (shiftOffset + 1) dst

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
                    unsafeGet (shiftOffset + 1) dst

                c2 =
                    unsafeGet (shiftOffset + 2) dst

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
                    unsafeGet (shiftOffset + 1) dst

                c2 =
                    unsafeGet (shiftOffset + 2) dst

                c3 =
                    unsafeGet (shiftOffset + 3) dst

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


unpackTransforms : String -> String -> Transforms -> Transforms
unpackTransforms prefixSuffixSrc transformsSrc transforms =
    let
        n =
            String.length prefixSuffixSrc

        go i j index accum headsAccum =
            if i < n then
                let
                    c =
                        charCodeAt i prefixSuffixSrc
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
                            (charCodeAt i transformsSrc - 32)
                            (charCodeAt (i + 1) transformsSrc - 32)
                            (charCodeAt (i + 2) transformsSrc - 32)
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


rfcPrefixSuffixSrc : String
rfcPrefixSuffixSrc =
    """# #s #, #e #.# the #.com/#Â\u{00A0}# of # and # in # to #"#">#
#]# for # a # that #. # with #'# from # by #. The # on # as # is #ing #
\t#:#ed #(# at #ly #="# of the #. This #,# not #er #al #='#ful #ive #less #est #ize #ous #"""


rfcTransformsSrc : String
rfcTransformsSrc =
    """     !! ! ,  *!  &!  " !  ) *   * -  ! # !  #!*!  +  ,$ !  -  %  .  / #   0  1 .  "   2  3!*   4%  ! # /   5  6  7  8 0  1 &   $   9 +   :  ;  < '  !=  >  ?! 4  @ 4  2  &   A *# (   B  C& ) %  ) !*# *-% A +! *.  D! %'  & E *6  F  G% ! *A *%  H! D  I!+!  J!+   K +- *4! A  L!*4  M  N +6  O!*% +.! K *G  P +%(  ! G *D +D  Q +# *K!*G!+D!+# +G +A +4!+% +K!+4!*D!+K!*K"""


charCodeAt : Int -> String -> Int
charCodeAt n str =
    case String.uncons (String.dropLeft n str) of
        Nothing ->
            0

        Just ( c, _ ) ->
            Char.toCode c


unsafeGet : Int -> Array Int -> Int
unsafeGet i arr =
    Array.get i arr
        |> Maybe.withDefault 0


update : Int -> (a -> a) -> Array a -> Array a
update index f arr =
    case Array.get index arr of
        Nothing ->
            arr

        Just v ->
            Array.set index (f v) arr
