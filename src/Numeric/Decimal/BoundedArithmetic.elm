module Numeric.Decimal.BoundedArithmetic exposing
    ( addBounded
    , fromIntBounded
    , subtractBounded
    )

import Numeric.Integer exposing (maxBound, minBound, signum)


addBounded : Int -> Int -> Result String Int
addBounded x y =
    let
        signX =
            signum x

        signY =
            signum y

        sameSign =
            signX == signY
    in
    if sameSign && signX == 1 && x > maxBound - y then
        Err "Overflow"

    else if sameSign && signX == -1 && x < minBound - y then
        Err "Underflow"

    else
        Ok (x + y)


subtractBounded : Int -> Int -> Result String Int
subtractBounded x y =
    let
        signY =
            signum y
    in
    if signY == -1 && x > maxBound + y then
        Err "Overflow"

    else if signY == 1 && x < minBound + y then
        Err "Underflow"

    else
        Ok (x - y)


fromIntBounded : Int -> Result String Int
fromIntBounded x =
    if x > maxBound then
        Err "Overflow"

    else if x < minBound then
        Err "Underflow"

    else
        Ok x
