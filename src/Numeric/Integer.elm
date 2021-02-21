module Numeric.Integer exposing (div, even, maxBound, minBound, odd, quot, quotRem, signum)


maxBound : Int
maxBound =
    2 ^ 53 - 1


minBound : Int
minBound =
    -maxBound


signum : Int -> Int
signum x =
    if x == 0 then
        0

    else if x < 0 then
        -1

    else
        1


div : Int -> Int -> Int
div a b =
    Basics.toFloat a / Basics.toFloat b |> Basics.floor


quot : Int -> Int -> Int
quot a b =
    Basics.toFloat a / Basics.toFloat b |> Basics.truncate


quotRem : Int -> Int -> ( Int, Int )
quotRem a b =
    ( quot a b, Basics.remainderBy b a )


even : Int -> Bool
even x =
    Basics.modBy 2 x == 0


odd : Int -> Bool
odd =
    not << even
