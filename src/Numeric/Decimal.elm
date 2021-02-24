module Numeric.Decimal exposing
    ( Decimal
    , succeed, map, map2, andMap
    , fromInt, fromDecimalBounded, fromRational, fromRationalBounded, toRational, withRounding
    , fromString, toString
    , toInt, toNumerator, toDenominator, splitDecimal
    , getPrecision, roundDecimal, scaleUp, scaleUpBounded
    , add, subtract, multiply, divide
    , addBounded, subtractBounded, multiplyBounded, divideBounded
    )

{-|


# Definition

@docs Decimal


# Applicative

@docs succeed, map, map2, andMap


# Conversion

@docs fromInt, fromDecimalBounded, fromRational, fromRationalBounded, toRational, withRounding


# Parsing and Printing

@docs fromString, toString


# Unwrapping

@docs toInt, toNumerator, toDenominator, splitDecimal


# Rounding and scaling

@docs getPrecision, roundDecimal, scaleUp, scaleUpBounded


# Simple arithmetic

@docs add, subtract, multiply, divide


# Bounded arithmetic

@docs addBounded, subtractBounded, multiplyBounded, divideBounded

-}

import Numeric.Decimal.BoundedArithmetic as Arithmetic
import Numeric.Decimal.Rounding as Rounding exposing (RoundingAlgorythm)
import Numeric.Integer exposing (maxBound, minBound, quotRem)
import Numeric.Nat as Nat exposing (Nat)
import Numeric.Rational as Rational exposing (Rational)
import Parser exposing ((|.), (|=), Parser)


{-| Decimal number with precision parameter (i.e. number of digits following decimal point) and rounding strategy.

The `s` type variable is for a phantom type and it is here to provide type level safety of arithmetic operations on Decimals.
There is nothing on the type level that is restricting you from adding two decimals with different precision parameter.

This could result in something you didn't anticipate. Precision and rounding strategy of the left operand will be used for the resulting decimal.
The second operand is not scaled or rounded down to match the first operand (i.e. `1.2 + 1.25 == 13.7`).

But there is a way out of this with the help of a custom phantom type:

    type Dollars
        = Dollars

    type Pennies
        = Pennies

    dollars : Int -> Decimal Dollars Int
    dollars =
        Decimal.succeed HalfToEven nat0

    pennies : Int -> Decimal Pennies Int
    pennies =
        Decimal.succeed HalfToEven nat2

    let
        x = pennies 120

        y = dollars 210
    in
    D.add x y

    -- TYPE MISMATCH ----- ~/decimal/tests/DecimalTest.elm

    -- The 2nd argument to `add` is not what I expect:

    -- 105| D.add x y
    --              ^
    -- This `y` value is a:

    --     Decimal Dollars Int

    -- But `add` needs the 2nd argument to be:

    --     Decimal Pennies Int

The `p` is for precision. In more advanced type system this could be arbitrary Intergal type (Integer, Int8, Int16, Int32, Int64 etc.)
but in Elm it is difficult to achieve that level of abstraction.
To support `applicative` chaining of operations there is `p` type variable (wrapped value can be a function).

-}
type Decimal s p
    = Decimal RoundingAlgorythm Nat p


{-| A Decimal that succeeds without scaling or rounding.

    import Numeric.Decimal as Decimal
    import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
    import Numeric.Nat exposing (nat2)

    Decimal.succeed RoundDown nat2 100
        |> Decimal.toString
        -- 1.00

-}
succeed : RoundingAlgorythm -> Nat -> p -> Decimal s p
succeed r s p =
    Decimal r s p


{-| toInt underlying representation for the decimal number. No rounding will be done.

    import Numeric.Decimal as Decimal
    import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
    import Numeric.Nat exposing (nat2)

    Decimal.toInt (Decimal.succeed RoundDown nat2 100) -- 100

-}
toInt : Decimal s p -> p
toInt (Decimal _ _ p) =
    p


{-| Get the toNumerator. Same as `toInt`.
-}
toNumerator : Decimal s Int -> Int
toNumerator (Decimal _ _ n) =
    n


{-| Get the Decimal denominator. Always will be a power of `10`.

    import Numeric.Decimal as Decimal
    import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
    import Numeric.Nat exposing (nat3)

    Decimal.succeed RoundDown nat3 8
        |> Decimal.toDenominator
        -- (10 ^ 3) = 1000

-}
toDenominator : Decimal s p -> Int
toDenominator (Decimal _ s _) =
    10 ^ Nat.toInt s


{-| Split the number at the decimal point, i.e. whole number and the fraction.

    import Numeric.Decimal as Decimal
    import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
    import Numeric.Nat exposing (nat3)

    D.succeed RoundTowardsZero nat3 1234
        |> D.splitDecimal
        -- (1, 234)

-}
splitDecimal : Decimal s Int -> ( Int, Int )
splitDecimal (Decimal _ s p) =
    quotRem p (10 ^ Nat.toInt s)


{-| Transform wrapped value with a given function.

    import Numeric.Decimal as Decimal
    import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
    import Numeric.Nat exposing (nat2)

    Decimal.map sqrt (Decimal.succeed RoundDown nat2 25)
        |> Decimal.toString
        -- 0.50

-}
map : (a -> b) -> Decimal s a -> Decimal s b
map f (Decimal r s p) =
    Decimal r s (f p)


{-| Apply a function on two decimal values.

    import Numeric.Decimal as Decimal
    import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
    import Numeric.Nat exposing (nat2)

    Decimal.map2 (+)
        (Decimal.succeed RoundDown nat2 234)
        (Decimal.succeed RoundDown nat2 567)
        |> Decimal.toString
        -- 8.01

-}
map2 : (a -> b -> c) -> Decimal s a -> Decimal s b -> Decimal s c
map2 f (Decimal r s p1) (Decimal _ _ p2) =
    Decimal r s (f p1 p2)


{-| Apply wrapped function inside `Decimal` on another decimal value.

    import Numeric.Decimal as Decimal
    import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
    import Numeric.Nat exposing (nat2)

    Decimal.succeed RoundDown nat2 (\x -> x + 1)
        |> Decimal.andMap (Decimal.succeed RoundDown nat2 567)
        |> Decimal.toString
        -- 5.68

-}
andMap : Decimal s a -> Decimal s (a -> b) -> Decimal s b
andMap =
    map2 (|>)


{-| Use different rounding strategy.
-}
withRounding : RoundingAlgorythm -> Decimal s p -> Decimal s p
withRounding r (Decimal _ s p) =
    Decimal r s p


{-| Rounding down to a number of decimals.

    import Numeric.Decimal as Decimal
    import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
    import Numeric.Nat exposing (nat1, nat2)

    Decimal.succeed RoundDown nat2 123  -- 1.23
        |> Decimal.roundDown nat1
        |> Decimal.toString
        -- 1.2

-}
roundDecimal : Nat -> Decimal s Int -> Decimal s Int
roundDecimal k (Decimal r s d) =
    Rounding.getRounder r (Nat.subtract s k) d |> Decimal r k


{-| Get precision of the Decimal (number of fractional digits)

    import Numeric.Decimal as Decimal
    import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
    import Numeric.Nat exposing (nat1, nat2)

    Decimal.succeed RoundDown nat2 123
        |> Decimal.getPrecision
        -- (Nat 2)

-}
getPrecision : Decimal s p -> Nat
getPrecision (Decimal _ s _) =
    s


{-| Increase the precision of a `Decimal`, use `roundDecimal` for the inverse.

    import Numeric.Decimal as Decimal
    import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
    import Numeric.Nat exposing (nat2, nat3)

    Decimal.succeed RoundDown nat2 123  -- 1.23
        |> Decimal.scaleUp nat3
        |> Decimal.toString
        -- 1.230

-}
scaleUp : Nat -> Decimal s Int -> Decimal s Int
scaleUp k (Decimal r s p) =
    Decimal r k (p * (10 ^ Nat.toInt (Nat.subtract k s)))


{-| Increase the precision of a `Decimal` while checking for `Overflow`/`Underflow`, use `roundDecimal` if inverse is desired.
-}
scaleUpBounded : Nat -> Decimal s Int -> Result String (Decimal s Int)
scaleUpBounded k (Decimal r s p) =
    Arithmetic.fromIntBounded (10 ^ Nat.toInt (Nat.subtract k s))
        |> Result.andThen (Arithmetic.multiplyBounded p)
        |> Result.map (Decimal r k)



-- ARITHMETIC


{-| Add two Decimals.
-}
add : Decimal s Int -> Decimal s Int -> Decimal s Int
add =
    map2 (+)


{-| Subtract one Decimal from another.
-}
subtract : Decimal s Int -> Decimal s Int -> Decimal s Int
subtract =
    map2 (-)


{-| Multiply two Decimals.
-}
multiply : Decimal s Int -> Decimal s Int -> Decimal s Int
multiply (Decimal r s1 d1) (Decimal _ s2 d2) =
    Decimal r (Nat.add s1 s2) (d1 * d2) |> roundDecimal s1


{-| Divide two Decimals. Operation can fail if divisor is zero.
-}
divide : Decimal s Int -> Decimal s Int -> Result String (Decimal s Int)
divide (Decimal r s d1) (Decimal _ _ d2) =
    if d2 == 0 then
        Err "Divide by zero"

    else
        Rational.ratio d1 d2
            |> fromRational r s


{-| Convert `Int` to `Decimal` while performing necessary scaling.

    import Numeric.Decimal as Decimal
    import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
    import Numeric.Nat exposing (nat3)

    Decimal.fromInt RoundDown nat3 7
        |> Decimal.toString
        -- 7.000

-}
fromInt : RoundingAlgorythm -> Nat -> Int -> Decimal s Int
fromInt r s p =
    Decimal r s (p * 10 ^ Nat.toInt s)


{-| Convert a `Decimal` to another `Decimal` while checking for `Overflow`/`Underflow`.
-}
fromDecimalBounded : Decimal s Int -> Result String (Decimal s Int)
fromDecimalBounded (Decimal r s d) =
    Arithmetic.fromIntBounded d |> Result.map (Decimal r s)


{-| Convert `Rational` to `Decimal`.
-}
fromRational : RoundingAlgorythm -> Nat -> Rational -> Result String (Decimal s Int)
fromRational r s rational =
    let
        den =
            Rational.toDenominator rational
    in
    if den == 0 then
        Err "Divide by zero"

    else
        let
            d =
                10 ^ (Nat.toInt s + 1)
        in
        Rational.ratio d 1
            |> Rational.multiply rational
            |> Rational.truncate
            |> succeed r (Nat.successor s)
            |> roundDecimal s
            |> Ok


{-| Convert from `Rational` to `Decimal` while checking for `Overflow`/`Underflow`.
-}
fromRationalBounded : RoundingAlgorythm -> Nat -> Rational -> Result String (Decimal s Int)
fromRationalBounded r s rational =
    let
        den =
            Rational.toDenominator rational
    in
    if den == 0 then
        Err "Divide by zero"

    else
        let
            d =
                10 ^ (Nat.toInt s + 1)
        in
        Rational.ratioBounded d 1
            |> Result.andThen (Rational.multiplyBounded rational)
            |> Result.map Rational.truncate
            |> Result.map (succeed r (Nat.add s (Nat.fromInt 1)))
            |> Result.map (roundDecimal s)
            |> Result.andThen fromDecimalBounded


{-| Converting `Decimal` to `Rational`.
-}
toRational : Decimal s Int -> Rational
toRational (Decimal _ s d) =
    Rational.ratio d (10 ^ Nat.toInt s)


{-| Add two Decimals while checking for `Overflow`/`Underflow`.
-}
addBounded : Decimal s Int -> Decimal s Int -> Result String (Decimal s Int)
addBounded (Decimal r1 s1 d1) (Decimal _ _ d2) =
    Arithmetic.addBounded d1 d2 |> Result.map (Decimal r1 s1)


{-| Subtract one Decimal from another while checking for `Overflow`/`Underflow`.
-}
subtractBounded : Decimal s Int -> Decimal s Int -> Result String (Decimal s Int)
subtractBounded (Decimal r1 s1 d1) (Decimal _ _ d2) =
    Arithmetic.subtractBounded d1 d2 |> Result.map (Decimal r1 s1)


{-| Multiply two Decimal while checking for `Overflow`/`Underflow`.
-}
multiplyBounded : Decimal s Int -> Decimal s Int -> Result String (Decimal s Int)
multiplyBounded (Decimal r s1 d1) (Decimal _ s2 d2) =
    let
        c =
            d1 * d2
    in
    if c > maxBound then
        Err "Overflow"

    else if c < minBound then
        Err "Underflow"

    else
        Decimal r (Nat.add s1 s2) c
            |> roundDecimal s1
            |> Ok


{-| Divide two Decimals while checking for `Overflow`/`Underflow` and `Division by zero`.
-}
divideBounded : Decimal s Int -> Decimal s Int -> Result String (Decimal s Int)
divideBounded (Decimal r s d1) (Decimal _ _ d2) =
    if d2 == 0 then
        Err "Divide by zero"

    else
        Rational.ratioBounded d1 d2
            |> Result.andThen (fromRationalBounded r s)


fromIntsScaleBounded : RoundingAlgorythm -> Nat -> Int -> Int -> Result String (Decimal s Int)
fromIntsScaleBounded r s x y =
    (x * (10 ^ Nat.toInt s)) + y |> Arithmetic.fromIntBounded |> Result.map (Decimal r s)



-- SHOWING


{-| Printing Decimal to `String` representation.
-}
toString : Decimal s Int -> String
toString (Decimal _ s p) =
    let
        e =
            Nat.toInt s

        ( q, r ) =
            quotRem p (10 ^ e)

        formatted =
            String.fromInt q ++ "." ++ String.padLeft e '0' (Basics.abs r |> String.fromInt)
    in
    if e == 0 then
        String.fromInt p

    else if r == 0 then
        String.fromInt q ++ "." ++ String.repeat e "0"

    else if r < 0 && q == 0 then
        "-" ++ formatted

    else
        formatted



-- PARSING


{-| Parse `String` to `Decimal` while chacking for formatting and `Overflow`/`Underflow`.
-}
fromString : RoundingAlgorythm -> Nat -> String -> Result String (Decimal s Int)
fromString r s str =
    Parser.run (parseDecimalBounded r s) str
        |> Result.mapError deadEndsToString


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString =
    let
        endToString e =
            case e.problem of
                Parser.Problem x ->
                    x

                _ ->
                    ""
    in
    List.foldl (\e a -> a ++ endToString e) ""


parseDecimalBounded : RoundingAlgorythm -> Nat -> Parser (Decimal s Int)
parseDecimalBounded r s =
    Parser.succeed toCoefficient
        |= parseSign
        |= parseDigits
        |. Parser.oneOf [ Parser.symbol ".", Parser.succeed () ]
        |= parseFractionalPart s
        |> Parser.andThen
            (\( a, b ) ->
                case fromIntsScaleBounded r s a b of
                    Ok d ->
                        Parser.succeed d

                    Err e ->
                        Parser.problem e
            )


toCoefficient : (Int -> Int) -> String -> String -> ( Int, Int )
toCoefficient negate decimal fractional =
    if String.isEmpty fractional then
        String.toInt decimal
            |> Maybe.map (negate >> (\x -> ( x, 0 )))
            |> Maybe.withDefault ( 0, 0 )

    else
        Maybe.map2 Tuple.pair
            (String.toInt decimal |> Maybe.map negate)
            (String.toInt fractional |> Maybe.map negate)
            |> Maybe.withDefault ( 0, 0 )


parseSign : Parser (Int -> Int)
parseSign =
    Parser.oneOf
        [ Parser.symbol "-" |> Parser.map ((*) -1 |> always)
        , Parser.symbol "+" |> Parser.map (always identity)
        , Parser.succeed identity
        ]


parseDigits : Parser String
parseDigits =
    Parser.chompWhile Char.isDigit
        |> Parser.getChompedString


parseFractionalPart : Nat -> Parser String
parseFractionalPart s =
    Parser.oneOf
        [ parseDigits
            |> Parser.andThen
                (\x ->
                    if String.length x > Nat.toInt s then
                        Parser.problem ("Too much text after the decimal: " ++ x)

                    else
                        Parser.succeed (String.padRight (Nat.toInt s) '0' x)
                )
        , Parser.succeed ""
        ]
