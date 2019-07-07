import brotli

import glob
import os

test_file_format = """
module Gutenberg.{} exposing(text, bytes) 

text : String
text = \"""{}\"""

bytes : List Int 
bytes = {}
"""

gutenberg_format = """
module Gutenberg exposing (tests)

import Bytes
import Bytes.Encode as Encode 
import Bytes.Decode as Decode 
import Internal


import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

{}

pipeline name expected raw =
            test name <|
                \_ ->
                    let
                        data =
                            raw
                                |> List.map Encode.unsignedInt8
                                |> Encode.sequence
                                |> Encode.encode
                    in
                    case Internal.decode data of
                        Err e ->
                            Expect.fail e

                        Ok v ->
                            Decode.decode (Decode.string (Bytes.width v)) v
                                |> Expect.equal (Just expected)

tests = 
    describe "gutenberg book tests" 
        [ {}  ]
"""

def write_elm_file():
    with open('/home/folkertdev/temp.txt') as f:
        contents = f.read()
        bs = contents.encode('utf-8')
        # print("uncompressed: ", [b for b in bs ] )
        # print()
        # print(int32(bs))
        cs = chunks(bs, 4)
        # print([v for v in bs ] )

        # result = [ "0x{:X}".format(combine(v)) for v in cs ] 
        bs = brotli.compress(bs)
        result = [ v for v in bs ] 

        # result = "[" + ",".join(result) + "]"
         
        with open('/home/folkertdev/elm/elm-brotli/tests/Generated.elm', 'w') as output:
            output.write(file_format.format(contents, result))


if __name__ == '__main__':
    bases = [] 
    for sourcePath in glob.glob("sources/**"):
        bases.append(os.path.splitext(os.path.basename(sourcePath))[0])

    for sourcePath in glob.glob("sources/**"):
        with open(sourcePath) as source:
            contents = source.read() 
            bs = contents.encode('utf-8')

            bs = brotli.compress(bs)
            result = [ v for v in bs ] 

            base = os.path.basename(sourcePath)
            destName = os.path.splitext(base)[0]

            with open(destName + ".elm", 'w') as dst: 
                dst.write(test_file_format.format(destName, contents, result))


    imports = "\n".join("import Gutenberg." + name + " as " + name for name in bases)

    tests = ", ".join("pipeline \"" + name + "\" " + name + ".text " + name + ".bytes" for name in bases) 

    with open("../Gutenberg.elm", 'w') as dst:
        dst.write(gutenberg_format.format(imports, tests))

