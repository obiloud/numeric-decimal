module Numeric.Decimal.Rounding exposing
    ( RoundingAlgorythm(..)
    , getRounder
    )

{-|


# Definition

@docs RoundingAlgorythm


# Helpers

@docs getRounder

-}

import Numeric.Integer exposing (odd, quotRem, signum)
import Numeric.Nat as Nat exposing (Nat)


{-| Rounding Algorythm
-}
type RoundingAlgorythm
    = RoundDown
      -- | RoundUp
    | RoundTowardsZero
      -- | RoundAwayFromZero
      -- | HalfUp
      -- | HalfDown
      -- | HalfTowardsZero
      -- | HalfAwayFromZero
    | HalfToEven



-- | HalfToOdd


{-| Returns rounding function matched by `RoundongAlgorythm`
-}
getRounder : RoundingAlgorythm -> (Nat -> Int -> Int)
getRounder algorythm =
    case algorythm of
        RoundDown ->
            roundDown

        -- RoundUp ->
        --     roundToZero
        RoundTowardsZero ->
            roundToZero

        -- RoundAwayFromZero ->
        --     roundToZero
        -- HalfUp ->
        --     roundToZero
        -- HalfDown ->
        --     roundToZero
        -- HalfTowardsZero ->
        --     roundToZero
        -- HalfAwayFromZero ->
        --     roundToZero
        HalfToEven ->
            roundHalfEven



-- HalfToOdd ->
--     roundToZero


roundDown : Nat -> Int -> Int
roundDown e c =
    let
        b =
            10 ^ Nat.unwrap e

        ( q, r ) =
            quotRem c b
    in
    if c >= 0 || r == 0 then
        q

    else
        q - 1


roundToZero : Nat -> Int -> Int
roundToZero e c =
    c // (10 ^ Nat.unwrap e)


roundHalfEven : Nat -> Int -> Int
roundHalfEven s c =
    let
        e =
            Nat.unwrap s

        b =
            10 ^ e

        ( q, r ) =
            quotRem c b
                |> Tuple.mapSecond ((*) 2)
    in
    if e == 0 then
        c

    else if abs r == b && odd q then
        q + signum r

    else if abs r == b then
        q

    else if r > b then
        q + 1

    else if signum r < 0 && abs r > e then
        q - 1

    else
        q
