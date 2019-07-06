module Brotli exposing (decode)

import Bytes exposing (Bytes)
import Internal


decode : Bytes -> Result String Bytes
decode =
    Internal.decode
