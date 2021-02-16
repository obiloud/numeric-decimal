module Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..), getRounder)

import Basics.Extra exposing (odd, quotRem, signum)


{-| Rounding Algorythm
-}
type RoundingAlgorythm
    = RoundDown
    | RoundUp
    | RoundTowardsZero
    | RoundAwayFromZero
    | HalfUp
    | HalfDown
    | HalfTowardsZero
    | HalfAwayFromZero
    | HalfToEven
    | HalfToOdd


getRounder : RoundingAlgorythm -> (Int -> Int -> Int)
getRounder algorythm =
    case algorythm of
        RoundDown ->
            roundDown

        RoundUp ->
            roundToZero

        RoundTowardsZero ->
            roundToZero

        RoundAwayFromZero ->
            roundToZero

        HalfUp ->
            roundToZero

        HalfDown ->
            roundToZero

        HalfTowardsZero ->
            roundToZero

        HalfAwayFromZero ->
            roundToZero

        HalfToEven ->
            roundHalfEven

        HalfToOdd ->
            roundToZero


roundDown : Int -> Int -> Int
roundDown c e =
    let
        b =
            10 ^ e

        ( q, r ) =
            quotRem c b
    in
    if c >= 0 || r == 0 then
        q

    else
        q - 1


roundToZero : Int -> Int -> Int
roundToZero c e =
    c // (10 ^ e)


roundHalfEven : Int -> Int -> Int
roundHalfEven c e =
    let
        b =
            10 ^ e

        ( q, r ) =
            quotRem c b
                |> Tuple.mapSecond ((*) 2)
    in
    if e == 0 then
        c

    else if Basics.abs r == b && odd q then
        q + signum r

    else if Basics.abs r == b then
        q

    else if r > b then
        q + 1

    else if signum r < 0 && Basics.abs r > e then
        q - 1

    else
        q
