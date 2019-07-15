module DistanceRingBuffer exposing (DistanceRingBuffer, empty, get, getAtCurrentPosition, getPosition, push, set, setPosition)

import Array exposing (Array)


empty : Int -> DistanceRingBuffer
empty n =
    DistanceRingBuffer (Array.repeat n 0) 0


type DistanceRingBuffer
    = DistanceRingBuffer (Array Int) Int


push : Int -> DistanceRingBuffer -> DistanceRingBuffer
push value (DistanceRingBuffer array position) =
    DistanceRingBuffer (Array.set position value array) (position + 1)


set : Int -> Int -> DistanceRingBuffer -> DistanceRingBuffer
set index value (DistanceRingBuffer array position) =
    DistanceRingBuffer (Array.set index value array) position


get : Int -> DistanceRingBuffer -> Int
get index (DistanceRingBuffer array position) =
    case Array.get index array of
        Nothing ->
            0

        Just value ->
            value


getAtCurrentPosition : DistanceRingBuffer -> Int
getAtCurrentPosition (DistanceRingBuffer array position) =
    case Array.get position array of
        Nothing ->
            0

        Just value ->
            value


setPosition : Int -> DistanceRingBuffer -> DistanceRingBuffer
setPosition newPosition (DistanceRingBuffer array _) =
    DistanceRingBuffer array newPosition


getPosition : DistanceRingBuffer -> Int
getPosition (DistanceRingBuffer _ position) =
    position
