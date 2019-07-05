module Transforms exposing (Transforms, new, rfc_transforms)

import Array exposing (Array)


rfc_transforms : Transforms
rfc_transforms =
    new 121 167 50
        |> unpackTransforms rfcPrefixSuffixSrc rfcTransformsSrc


type alias Transforms =
    { numTransforms : Int
    , triplets : Array Int
    , prefixSuffixStorage : Array Int
    , prefixSuffixHeads : Array Int
    , params : Array Int
    }


new : Int -> Int -> Int -> Transforms
new numTransforms prefixSuffixLen prefixSuffixCount =
    { numTransforms = numTransforms
    , params = Array.repeat numTransforms 0
    , triplets = Array.repeat (3 * numTransforms) 0
    , prefixSuffixStorage = Array.repeat prefixSuffixLen 0
    , prefixSuffixHeads = Array.repeat (prefixSuffixCount + 1) 0
    }


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
            if i < 363 then
                go2 (i + 1) (Array.set i (charCodeAt i transformsSrc - 32) currentTransforms)

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

