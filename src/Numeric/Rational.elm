module Numeric.Rational exposing
    ( Rational
    , ratio, ratioBounded
    , fromInt, toFloat, toString, inverse
    , toPropperFraction, toNumerator, toDenominator, truncate, round, ceiling, floor
    , compare, greaterThan, lessThan
    , add, subtract, multiply, divide, power
    , addBounded, subtractBounded, multiplyBounded, divideBounded, powerBounded
    )

{-|


# Definition

@docs Rational


# Build ratio

@docs ratio, ratioBounded


# Conversion

@docs fromInt, toFloat, toString, inverse


# Extracting components of fractions

@docs toPropperFraction, toNumerator, toDenominator, truncate, round, ceiling, floor


# Comparison

@docs compare, greaterThan, lessThan


# Arithmetic

@docs add, subtract, multiply, divide, power


# Bounded arithmetic

@docs addBounded, subtractBounded, multiplyBounded, divideBounded, powerBounded

-}

import Numeric.ArithmeticError exposing (ArithmeticError(..))
import Numeric.Integer exposing (div, even, gcd, maxBound, minBound)


{-| Arbitrary-precision rational numbers, represented as a ratio of two `Int` values.
-}
type Rational
    = Rational Int Int


{-| Converts `Int` to `Rational`.
-}
fromInt : Int -> Rational
fromInt n =
    Rational n 1


{-| Forms the ratio of two integers.
-}
ratio : Int -> Int -> Rational
ratio n d =
    let
        gcd_ =
            gcd n d
    in
    Rational (div n gcd_) (div d gcd_)


{-| Add two Rationals.
-}
add : Rational -> Rational -> Rational
add (Rational n1 d1) (Rational n2 d2) =
    ratio ((n1 * d2) + (n2 * d1)) (d1 * d2)


{-| Subtract one Rational from another.
-}
subtract : Rational -> Rational -> Rational
subtract (Rational n1 d1) (Rational n2 d2) =
    ratio ((n1 * d2) - (n2 * d1)) (d1 * d2)


{-| Multiply two Rationals.
-}
multiply : Rational -> Rational -> Rational
multiply (Rational n1 d1) (Rational n2 d2) =
    ratio (n1 * n2) (d1 * d2)


{-| Divide two Rationals.
-}
divide : Rational -> Rational -> Rational
divide (Rational n1 d1) (Rational n2 d2) =
    ratio (n1 * d2) (d1 * n2)


{-| Rase `Rational` to the power.
-}
power : Int -> Rational -> Rational
power pow (Rational n d) =
    ratio (n ^ pow) (d ^ pow)


{-| Inverse numerator and denominator of the `Rational`.
-}
inverse : Rational -> Rational
inverse (Rational n d) =
    Rational d n


{-| Compare two Rationals.
-}
compare : Rational -> Rational -> Order
compare (Rational n1 d1) (Rational n2 d2) =
    Basics.compare (n1 * d2) (n2 * d1)


{-| Compare two Rationals, greater than precidate.
-}
greaterThan : Rational -> Rational -> Bool
greaterThan a b =
    compare a b == GT


{-| Compare two Rationals, less than precidate.
-}
lessThan : Rational -> Rational -> Bool
lessThan a b =
    compare a b == LT


{-| Printing `Rational` to `String` representation.
-}
toString : Rational -> String
toString r =
    let
        ( int, ( num, den ) ) =
            toPropperFraction r
                |> Tuple.mapSecond (\rem -> ( toNumerator rem, toDenominator rem ))
    in
    case ( int, num ) of
        ( _, 0 ) ->
            String.fromInt int

        ( 0, _ ) ->
            String.fromInt num ++ "/" ++ String.fromInt den

        ( _, _ ) ->
            String.fromInt int ++ " + " ++ String.fromInt num ++ "/" ++ String.fromInt den


{-| The function `toProperFraction` takes a real fractional number `x` and returns a pair `( n, f )` such that `x = n + f`, and:

  - `n` is an integral number with the same sign as `x`; and
  - `f` is a ratio with the same type and sign as `x`, and with absolute value less than `1`.

-}
toPropperFraction : Rational -> ( Int, Rational )
toPropperFraction (Rational n d) =
    ( div n d, ratio (remainderBy d n) d )


{-| Extract the numerator of the ratio in reduced form: the numerator and denominator have no common factor and the denominator is positive.
-}
toNumerator : Rational -> Int
toNumerator (Rational n _) =
    n


{-| Extract the denominator of the ratio in reduced form: the numerator and denominator have no common factor and the denominator is positive.
-}
toDenominator : Rational -> Int
toDenominator (Rational _ d) =
    d


{-| `truncate x` returns the integer nearest `x` between zero and `x`.
-}
truncate : Rational -> Int
truncate =
    toPropperFraction >> Tuple.first


{-| `round x` returns the nearest integer to `x`: the even integer if `x` is equily distant between two integers.
-}
round : Rational -> Int
round x =
    let
        ( int, rem ) =
            toPropperFraction x |> Tuple.mapSecond toFloat

        m =
            if rem < 0 then
                int - 1

            else
                int + 1

        sig =
            abs rem - 0.5
    in
    if sig < 0 then
        int

    else if sig == 0 && even int then
        int

    else
        m


{-| `ceiling x` returns the least integer not less than `x`.
-}
ceiling : Rational -> Int
ceiling x =
    let
        ( int, rem ) =
            toPropperFraction x |> Tuple.mapSecond toFloat
    in
    if rem > 0 then
        int + 1

    else
        int


{-| `floor x` returns the greatest integer not greater than `x`.
-}
floor : Rational -> Int
floor x =
    let
        ( int, rem ) =
            toPropperFraction x |> Tuple.mapSecond toFloat
    in
    if rem > 0 then
        int - 1

    else
        int


{-| Converts Rational to `Float`
-}
toFloat : Rational -> Float
toFloat (Rational n d) =
    Basics.toFloat n / Basics.toFloat d



-- BOUNDED


{-| Forms the ratio of two integers while checking for `Overflow`/`Underflow`.
-}
ratioBounded : Int -> Int -> Result ArithmeticError Rational
ratioBounded n d =
    let
        gcd_ =
            gcd n d

        num =
            div n gcd_

        den =
            div d gcd_
    in
    if num > maxBound || den > maxBound then
        Err Overflow

    else if num < minBound || den < minBound then
        Err Underflow

    else
        Ok (Rational num den)


{-| Adds two Rationals while checking for `Overflow`/`Underflow`.
-}
addBounded : Rational -> Rational -> Result ArithmeticError Rational
addBounded (Rational n1 d1) (Rational n2 d2) =
    ratioBounded ((n1 * d2) + (n2 * d1)) (d1 * d2)


{-| Subtract one Rational from another while checking for `Overflow`/`Underflow`.
-}
subtractBounded : Rational -> Rational -> Result ArithmeticError Rational
subtractBounded (Rational n1 d1) (Rational n2 d2) =
    ratioBounded ((n1 * d2) - (n2 * d1)) (d1 * d2)


{-| Multiply two Rationals while checking for `Overflow`/`Underflow`.
-}
multiplyBounded : Rational -> Rational -> Result ArithmeticError Rational
multiplyBounded (Rational n1 d1) (Rational n2 d2) =
    ratioBounded (n1 * n2) (d1 * d2)


{-| Divide two Rationals while checking for `Overflow`/`Underflow`.
-}
divideBounded : Rational -> Rational -> Result ArithmeticError Rational
divideBounded (Rational n1 d1) (Rational n2 d2) =
    ratioBounded (n1 * d2) (d1 * n2)


{-| Rase `Rational` to the power while checking for `Overflow`/`Underflow`
-}
powerBounded : Int -> Rational -> Result ArithmeticError Rational
powerBounded pow (Rational n d) =
    ratioBounded (n ^ pow) d
