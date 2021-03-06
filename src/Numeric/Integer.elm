module Numeric.Integer exposing
    ( maxBound, minBound
    , truncate, quot, rem, div, mod, quotRem, divMod
    , signum, even, odd, gcd, lcm
    )

{-|


# Bounds

@docs maxBound, minBound


# Integral

@docs truncate, quot, rem, div, mod, quotRem, divMod


# Numeric functions

@docs signum, even, odd, gcd, lcm

-}


{-| Max safe integer.
-}
maxBound : Int
maxBound =
    2 ^ 53 - 1


{-| Min safe integer.
-}
minBound : Int
minBound =
    -maxBound


{-| Sign of x number. The functions abs and signum should satisfy the law:

    abs x * signum x == x

For real numbers, the signum is either -1 (negative), 0 (zero) or 1 (positive).

-}
signum : Int -> Int
signum x =
    if x == 0 then
        0

    else if x < 0 then
        -1

    else
        1


{-| `truncate x` returns the integer nearest `x` between zero and `x`
-}
truncate : Float -> Int
truncate x =
    if x < 0 then
        Basics.ceiling x

    else
        Basics.floor x


{-| Integer division truncated towards negative infinity.
-}
div : Int -> Int -> Int
div x y =
    Basics.toFloat x / Basics.toFloat y |> Basics.floor


{-| Integer division truncated towards zero.
-}
quot : Int -> Int -> Int
quot x y =
    Basics.toFloat x / Basics.toFloat y |> truncate


{-| integer modulus, satisfying

    div x y * y + mod x y == x

-}
mod : Int -> Int -> Int
mod x y =
    Basics.modBy y x


{-| integer remainder, satisfying

    quot x y * y + rem x y == x

-}
rem : Int -> Int -> Int
rem x y =
    Basics.remainderBy y x


{-| simultaneous quot and rem
-}
quotRem : Int -> Int -> ( Int, Int )
quotRem x y =
    ( quot x y, rem x y )


{-| simultaneous div and mod
-}
divMod : Int -> Int -> ( Int, Int )
divMod x y =
    ( div x y, mod x y )


{-| even predicate
-}
even : Int -> Bool
even x =
    Basics.modBy 2 x == 0


{-| odd predicate
-}
odd : Int -> Bool
odd =
    not << even


{-| gcd x y is the non-negative factor of both x and y of which every common factor of x and y is also x factor;
for example gcd 4 2 = 2, gcd (-4) 6 = 2, gcd 0 4 = 4. gcd 0 0 = 0.
(That is, the common divisor that is "greatest" in the divisibility preordering.)

Note: Since for signed fixed-width integer types, abs minBound < 0, the result may be negative if one of the arguments
is minBound (and necessarily is if the other is 0 or minBound) for such types.

-}
gcd : Int -> Int -> Int
gcd x y =
    if x == 0 then
        y

    else
        gcd (rem y x) x


{-| lcm x y is the smallest positive integer that both `x` and `y` divide.
-}
lcm : Int -> Int -> Int
lcm x y =
    if x == 0 || y == 0 then
        0

    else
        abs (quot x (gcd x y) * y)
