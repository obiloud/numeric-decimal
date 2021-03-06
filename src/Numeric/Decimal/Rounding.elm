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

import Numeric.Integer exposing (odd, quot, quotRem, signum)
import Numeric.Nat as Nat exposing (Nat)


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



-- | HalfToOdd


{-| Returns rounding function matched by `RoundongAlgorythm`
-}
getRounder : RoundingAlgorythm -> (Nat -> Int -> Int)
getRounder algorythm =
    case algorythm of
        RoundDown ->
            roundDown

        RoundUp ->
            roundUp

        RoundTowardsZero ->
            roundToZero

        RoundAwayFromZero ->
            roundAwayFromZero

        HalfUp ->
            roundHalfUp

        HalfDown ->
            roundHalfDown

        HalfTowardsZero ->
            roundHalfToZero

        HalfAwayFromZero ->
            roundHalfAwayFromZero

        HalfToEven ->
            roundHalfEven



-- HalfToOdd ->
--     roundToZero


roundDown : Nat -> Int -> Int
roundDown e c =
    let
        b =
            10 ^ Nat.toInt e

        ( q, r ) =
            quotRem c b
    in
    if c >= 0 || r == 0 then
        q

    else
        q - 1


roundUp : Nat -> Int -> Int
roundUp e c =
    let
        b =
            10 ^ Nat.toInt e

        ( q, r ) =
            quotRem c b
    in
    if c <= 0 || r == 0 then
        q

    else
        q + 1


roundToZero : Nat -> Int -> Int
roundToZero e c =
    quot c (10 ^ Nat.toInt e)


roundAwayFromZero : Nat -> Int -> Int
roundAwayFromZero e c =
    let
        b =
            10 ^ Nat.toInt e

        ( q, r ) =
            quotRem c b
    in
    if c == 0 || r == 0 then
        q

    else if c < 0 then
        q - 1

    else
        q + 1


roundHalfUp : Nat -> Int -> Int
roundHalfUp s c =
    let
        e =
            Nat.toInt s

        b =
            10 ^ e

        ( q, r ) =
            quotRem c b
                |> Tuple.mapSecond ((*) 2)
    in
    if e == 0 then
        c

    else if r >= b then
        q + 1

    else if signum r < 0 && abs r > b then
        q - 1

    else
        q


roundHalfToZero : Nat -> Int -> Int
roundHalfToZero s c =
    let
        e =
            Nat.toInt s

        b =
            10 ^ e

        ( q, r ) =
            quotRem c b
                |> Tuple.mapSecond ((*) 2)
    in
    if e == 0 then
        c

    else if r > b then
        q + 1

    else if signum r < 0 && abs r > b then
        q - 1

    else
        q


roundHalfAwayFromZero : Nat -> Int -> Int
roundHalfAwayFromZero s c =
    let
        e =
            Nat.toInt s

        b =
            10 ^ e

        ( q, r ) =
            quotRem c b
                |> Tuple.mapSecond ((*) 2)
    in
    if e == 0 then
        c

    else if r >= b then
        q + 1

    else if signum r < 0 && abs r >= b then
        q - 1

    else
        q


roundHalfDown : Nat -> Int -> Int
roundHalfDown s c =
    let
        e =
            Nat.toInt s

        b =
            10 ^ e

        ( q, r ) =
            quotRem c b
                |> Tuple.mapSecond ((*) 2)
    in
    if e == 0 then
        c

    else if r > b then
        q + 1

    else if signum r < 0 && abs r >= b then
        q - 1

    else
        q


roundHalfEven : Nat -> Int -> Int
roundHalfEven s c =
    let
        e =
            Nat.toInt s

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
