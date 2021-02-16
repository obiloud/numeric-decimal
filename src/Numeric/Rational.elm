module Numeric.Rational exposing
    ( Rational
    , add
    , compare
    , divide
    , fraction
    , fromInt
    , greaterThan
    , greatestCommonDenominator
    , inverse
    , lessThan
    , multiply
    , power
    , subtract
    , toDenominator
    , toFloat
    , toFraction
    , toNumerator
    , toParts
    , toString
    , truncate
    )


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
    Rational (n // gcd) (d // gcd)


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
    { int = n // d
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
