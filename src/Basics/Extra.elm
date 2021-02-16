module Basics.Extra exposing (even, maxBound, minBound, odd, quotRem, signum)


maxBound : Int
maxBound =
    2 ^ 53


minBound : Int
minBound =
    -2 ^ 53


signum : Int -> Int
signum x =
    if x == 0 then
        x

    else if x < 0 then
        -1

    else
        1


quotRem : Int -> Int -> ( Int, Int )
quotRem a b =
    ( a // b, Basics.remainderBy b a )


even : Int -> Bool
even x =
    Basics.modBy 2 x == 0


odd : Int -> Bool
odd x =
    Basics.modBy 2 x /= 0