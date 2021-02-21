module Numeric.Rational exposing
    ( Rational
    , add
    , addBounded
    , compare
    , divide
    , divideBounded
    , fraction
    , fractionBounded
    , fromInt
    , greaterThan
    , greatestCommonDenominator
    , inverse
    , lessThan
    , multiply
    , multiplyBounded
    , power
    , powerBounded
    , subtract
    , subtractBounded
    , toDenominator
    , toFloat
    , toFraction
    , toNumerator
    , toParts
    , toString
    , truncate
    )

import Numeric.Integer exposing (div, maxBound, minBound)


type Rational
    = Rational Int Int


fromInt : Int -> Rational
fromInt n =
    Rational n 1


fraction : Int -> Int -> Rational
fraction n d =
    let
        gcd =
            greatestCommonDenominator n d
    in
    Rational (div n gcd) (div d gcd)


greatestCommonDenominator : Int -> Int -> Int
greatestCommonDenominator a b =
    if a == 0 then
        b

    else
        greatestCommonDenominator (remainderBy a b) a


add : Rational -> Rational -> Rational
add (Rational n1 d1) (Rational n2 d2) =
    fraction ((n1 * d2) + (n2 * d1)) (d1 * d2)


subtract : Rational -> Rational -> Rational
subtract (Rational n1 d1) (Rational n2 d2) =
    fraction ((n1 * d2) - (n2 * d1)) (d1 * d2)


multiply : Rational -> Rational -> Rational
multiply (Rational n1 d1) (Rational n2 d2) =
    fraction (n1 * n2) (d1 * d2)


divide : Rational -> Rational -> Rational
divide (Rational n1 d1) (Rational n2 d2) =
    fraction (n1 * d2) (d1 * n2)


power : Int -> Rational -> Rational
power pow (Rational n d) =
    fraction (n ^ pow) d


inverse : Rational -> Rational
inverse (Rational n d) =
    Rational d n


compare : Rational -> Rational -> Order
compare (Rational n1 d1) (Rational n2 d2) =
    Basics.compare (n1 * d2) (n2 * d1)


greaterThan : Rational -> Rational -> Bool
greaterThan a b =
    compare a b == GT


lessThan : Rational -> Rational -> Bool
lessThan a b =
    compare a b == LT


toString : Rational -> String
toString r =
    let
        p =
            toParts r
    in
    case ( p.int, p.num ) of
        ( _, 0 ) ->
            String.fromInt p.int

        ( 0, _ ) ->
            String.fromInt p.num ++ "/" ++ String.fromInt p.den

        ( _, _ ) ->
            String.fromInt p.int ++ " + " ++ String.fromInt p.num ++ "/" ++ String.fromInt p.den


toParts : Rational -> { int : Int, num : Int, den : Int }
toParts (Rational n d) =
    { int = div n d
    , num = remainderBy d n
    , den = d
    }


toFraction : Rational -> { num : Int, den : Int }
toFraction (Rational n d) =
    { num = n, den = d }


toNumerator : Rational -> Int
toNumerator (Rational n _) =
    n


toDenominator : Rational -> Int
toDenominator (Rational _ d) =
    d


truncate : Rational -> Int
truncate =
    toParts >> .int


toFloat : Rational -> Float
toFloat (Rational n d) =
    Basics.toFloat n / Basics.toFloat d



-- BOUNDED


fractionBounded : Int -> Int -> Result String Rational
fractionBounded n d =
    let
        gcd =
            greatestCommonDenominator n d

        num =
            div n gcd

        den =
            div d gcd
    in
    if num > maxBound || den > maxBound then
        Err "Overflow"

    else if num < minBound || den < minBound then
        Err "Underflow"

    else
        Ok (Rational num den)


addBounded : Rational -> Rational -> Result String Rational
addBounded (Rational n1 d1) (Rational n2 d2) =
    fractionBounded ((n1 * d2) + (n2 * d1)) (d1 * d2)


subtractBounded : Rational -> Rational -> Result String Rational
subtractBounded (Rational n1 d1) (Rational n2 d2) =
    fractionBounded ((n1 * d2) - (n2 * d1)) (d1 * d2)


multiplyBounded : Rational -> Rational -> Result String Rational
multiplyBounded (Rational n1 d1) (Rational n2 d2) =
    fractionBounded (n1 * n2) (d1 * d2)


divideBounded : Rational -> Rational -> Result String Rational
divideBounded (Rational n1 d1) (Rational n2 d2) =
    fractionBounded (n1 * d2) (d1 * n2)


powerBounded : Int -> Rational -> Result String Rational
powerBounded pow (Rational n d) =
    fractionBounded (n ^ pow) d
