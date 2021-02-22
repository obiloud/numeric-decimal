module Numeric.Decimal exposing
    ( Decimal
    , succeed, map, map2, andMap
    , fromInt, fromDecimalBounded, fromRational, fromRationalBounded, toRational, withRounding
    , fromString, toString
    , unwrap, toNumerator, toDenominator, splitDecimal
    , getScale, roundDecimal, scaleUp, scaleUpBounded
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

@docs unwrap, toNumerator, toDenominator, splitDecimal


# Rounding and scaling

@docs getScale, roundDecimal, scaleUp, scaleUpBounded


# Simple arithmetic

@docs add, subtract, multiply, divide


# Bounded arithmetic

@docs addBounded, subtractBounded, multiplyBounded, divideBounded

-}

import Numeric.Decimal.BoundedArithmetic as Arithmetic
import Numeric.Decimal.Rounding as Rounding exposing (RoundingAlgorythm)
import Numeric.Integer exposing (maxBound, minBound, quotRem)
import Numeric.Rational as Rational exposing (Rational)
import Parser exposing ((|.), (|=), Parser)


{-| Decimal number with scaling parameter (i.e. number of digits after decimal point) and rounding strategy.
-}
type Decimal p
    = Decimal RoundingAlgorythm Nat p


type alias Nat =
    Int


{-| Create `Decimal` with `rounding`, `scaling` and `toNumerator`.
-}
succeed : RoundingAlgorythm -> Nat -> p -> Decimal p
succeed r s p =
    Decimal r s p


{-| Unwrap underlying representation for the decimal number. No rounding will be done.
-}
unwrap : Decimal p -> p
unwrap (Decimal _ _ p) =
    p


{-| Get the toNumerator. Same as `unwrap`.
-}
toNumerator : Decimal Int -> Int
toNumerator (Decimal _ _ n) =
    n


{-| Get the decimal toDenominator. Always will be a multiple of `10`.
-}
toDenominator : Decimal p -> Int
toDenominator (Decimal _ s _) =
    10 ^ s


{-| Split the number at the decimal point, i.e. whole number and the fraction.
-}
splitDecimal : Decimal Int -> ( Int, Int )
splitDecimal (Decimal _ s p) =
    quotRem p (s ^ 10)


{-| Transform wrapped value with a given function.
-}
map : (a -> b) -> Decimal a -> Decimal b
map f (Decimal r s p) =
    Decimal r s (f p)


{-| Apply a function on two decimal values.
-}
map2 : (a -> b -> c) -> Decimal a -> Decimal b -> Decimal c
map2 f (Decimal r s p1) (Decimal _ _ p2) =
    Decimal r s (f p1 p2)


{-| Apply wrapped function inside `Decimal` on another decimal value.
-}
andMap : Decimal a -> Decimal (a -> b) -> Decimal b
andMap =
    map2 (|>)


{-| Use different rounding strategy.
-}
withRounding : RoundingAlgorythm -> Decimal p -> Decimal p
withRounding r (Decimal _ s p) =
    Decimal r s p


{-| Rounding Decimal down to a number of decimals.
-}
roundDecimal : Nat -> Decimal Int -> Decimal Int
roundDecimal k (Decimal r s d) =
    Rounding.getRounder r d (s - k) |> Decimal r k


{-| Get a scale of the Decimal
-}
getScale : Decimal p -> Nat
getScale (Decimal _ s _) =
    s


{-| Increase the precision of a `Decimal`, use `roundDecimal` for the inverse.
-}
scaleUp : Nat -> Decimal Int -> Decimal Int
scaleUp k (Decimal r s p) =
    Decimal r k (p * (10 ^ Basics.abs (k - s)))


{-| Increase the precision of a `Decimal` backed by a bounded type, use `roundDecimal` if inverse is desired.
-}
scaleUpBounded : Nat -> Decimal Int -> Result String (Decimal Int)
scaleUpBounded k (Decimal r s p) =
    Arithmetic.fromIntBounded (10 ^ k - s)
        |> Result.andThen (Arithmetic.multiplyBounded p)
        |> Result.map (Decimal r k)


{-| Add two Decimals.
-}
add : Decimal Int -> Decimal Int -> Decimal Int
add =
    map2 (+)


{-| Subtract one Decimal from another.
-}
subtract : Decimal Int -> Decimal Int -> Decimal Int
subtract =
    map2 (-)


{-| Multiply two Decimals.
-}
multiply : Decimal Int -> Decimal Int -> Decimal Int
multiply (Decimal r s1 d1) (Decimal _ s2 d2) =
    Decimal r (s1 + s2) (d1 * d2) |> roundDecimal s1


{-| Divide two Decimals.
-}
divide : Decimal Int -> Decimal Int -> Result String (Decimal Int)
divide (Decimal r s d1) (Decimal _ _ d2) =
    if d2 == 0 then
        Err "Divide by zero"

    else
        Rational.fraction d1 d2
            |> fromRational r s


{-| Convert `Int` to `Decimal` while performing necessary scaling.
-}
fromInt : RoundingAlgorythm -> Nat -> Int -> Decimal Int
fromInt r s p =
    Decimal r s (p * 10 ^ s)


{-| Convert a `Decimal` to another `Decimal` while checking for `Overflow`/`Underflow`.
-}
fromDecimalBounded : Decimal Int -> Result String (Decimal Int)
fromDecimalBounded (Decimal r s d) =
    Arithmetic.fromIntBounded d |> Result.map (Decimal r s)


{-| Convert `Rational` to `Decimal`.
-}
fromRational : RoundingAlgorythm -> Nat -> Rational -> Result String (Decimal Int)
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
                10 ^ (s + 1)
        in
        Rational.fraction d 1
            |> Rational.multiply rational
            |> Rational.truncate
            |> succeed r (s + 1)
            |> roundDecimal s
            |> Ok


{-| Convert from `Rational` to `Decimal` while checking for `Overflow`/`Underflow`.
-}
fromRationalBounded : RoundingAlgorythm -> Nat -> Rational -> Result String (Decimal Int)
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
                10 ^ (s + 1)
        in
        Rational.fractionBounded d 1
            |> Result.andThen (Rational.multiplyBounded rational)
            |> Result.map Rational.truncate
            |> Result.map (succeed r (s + 1))
            |> Result.map (roundDecimal s)
            |> Result.andThen fromDecimalBounded


{-| Converting `Decimal` to `Rational`.
-}
toRational : Decimal Int -> Rational
toRational (Decimal _ s d) =
    Rational.fraction d (10 ^ s)


{-| Add two Decimals while checking for `Overflow`/`Underflow`.
-}
addBounded : Decimal Int -> Decimal Int -> Result String (Decimal Int)
addBounded (Decimal r1 s1 d1) (Decimal _ _ d2) =
    Arithmetic.addBounded d1 d2 |> Result.map (Decimal r1 s1)


{-| Subtract one Decimal from another while checking for `Overflow`/`Underflow`.
-}
subtractBounded : Decimal Int -> Decimal Int -> Result String (Decimal Int)
subtractBounded (Decimal r1 s1 d1) (Decimal _ _ d2) =
    Arithmetic.subtractBounded d1 d2 |> Result.map (Decimal r1 s1)


{-| Multiply two Decimal while checking for `Overflow`/`Underflow`.
-}
multiplyBounded : Decimal Int -> Decimal Int -> Result String (Decimal Int)
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
        Decimal r (s1 + s2) c
            |> roundDecimal s1
            |> Ok


{-| Divide two Decimals while checking for `Overflow`/`Underflow`.
-}
divideBounded : Decimal Int -> Decimal Int -> Result String (Decimal Int)
divideBounded (Decimal r s d1) (Decimal _ _ d2) =
    if d2 == 0 then
        Err "Divide by zero"

    else
        Rational.fractionBounded d1 d2
            |> Result.andThen (fromRationalBounded r s)


fromIntsScaleBounded : RoundingAlgorythm -> Nat -> Int -> Int -> Result String (Decimal Int)
fromIntsScaleBounded r s x y =
    (x * (10 ^ s)) + y |> Arithmetic.fromIntBounded |> Result.map (Decimal r s)



-- SHOWING


{-| Printing Decimal to String representation.
-}
toString : Decimal Int -> String
toString (Decimal _ s p) =
    let
        ( q, r ) =
            quotRem p (10 ^ s)

        formatted =
            String.fromInt q ++ "." ++ String.padLeft s '0' (Basics.abs r |> String.fromInt)
    in
    if s == 0 then
        String.fromInt p

    else if r == 0 then
        String.fromInt q ++ "." ++ String.repeat s "0"

    else if r < 0 && q == 0 then
        "-" ++ formatted

    else
        formatted



-- PARSING


{-| Parse `String` to `Decimal` while chacking for formatting and `Overflow`/`Underflow`.
-}
fromString : RoundingAlgorythm -> Nat -> String -> Result String (Decimal Int)
fromString r s str =
    Parser.run (parseDecimalBounded r (abs s)) str
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


parseDecimalBounded : RoundingAlgorythm -> Nat -> Parser (Decimal Int)
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
                    if String.length x > s then
                        Parser.problem ("Too much text after the decimal: " ++ x)

                    else
                        Parser.succeed (String.padRight s '0' x)
                )
        , Parser.succeed ""
        ]
