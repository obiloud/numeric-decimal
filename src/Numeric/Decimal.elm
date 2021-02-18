module Numeric.Decimal exposing
    ( Decimal
    , add
    , addBounded
    , andMap
    , divide
    , divideBounded
    , fromInt
    , fromString
    , getScale
    , map
    , map2
    , multiply
    , multiplyBounded
    , roundDecimal
    , scaleUp
    , subtract
    , subtractBounded
    , succeed
    , toString
    , withRounding
    )

import Numeric.Decimal.BoundedArithmetic as Arithmetic
import Numeric.Decimal.Rounding as Rounding exposing (RoundingAlgorythm)
import Numeric.Integer exposing (maxBound, minBound, quotRem)
import Numeric.Rational as Rational exposing (Rational)
import Parser exposing ((|.), (|=), Parser)


type Decimal p
    = Decimal RoundingAlgorythm Nat p


type alias Nat =
    Int


succeed : RoundingAlgorythm -> Nat -> p -> Decimal p
succeed r s p =
    Decimal r s p


map : (a -> b) -> Decimal a -> Decimal b
map f (Decimal r s p) =
    Decimal r s (f p)


map2 : (a -> b -> c) -> Decimal a -> Decimal b -> Decimal c
map2 f (Decimal r s p1) (Decimal _ _ p2) =
    Decimal r s (f p1 p2)


andMap : Decimal a -> Decimal (a -> b) -> Decimal b
andMap =
    map2 (|>)


withRounding : RoundingAlgorythm -> Decimal p -> Decimal p
withRounding r (Decimal _ s p) =
    Decimal r s p


roundDecimal : Nat -> Decimal Int -> Decimal Int
roundDecimal k (Decimal r s d) =
    Rounding.getRounder r d (s - k) |> Decimal r k


getScale : Decimal p -> Nat
getScale (Decimal _ s _) =
    s


scaleUp : Nat -> Decimal Int -> Decimal Int
scaleUp k (Decimal r s p) =
    Decimal r k (p * (10 ^ Basics.abs (k - s)))


add : Decimal Int -> Decimal Int -> Decimal Int
add =
    map2 (+)


subtract : Decimal Int -> Decimal Int -> Decimal Int
subtract =
    map2 (-)


multiply : Decimal Int -> Decimal Int -> Decimal Int
multiply (Decimal r s1 d1) (Decimal _ s2 d2) =
    Decimal r (s1 + s2) (d1 * d2) |> roundDecimal s1


divide : Decimal Int -> Decimal Int -> Result String (Decimal Int)
divide (Decimal r s d1) (Decimal _ _ d2) =
    if d2 == 0 then
        Err "Divide by zero"

    else
        Rational.fraction d1 d2
            |> Result.andThen (fromRational r s)


fromInt : RoundingAlgorythm -> Nat -> Int -> Decimal Int
fromInt r s p =
    Decimal r s (p * 10 ^ s)


fromDecimalBounded : Decimal Int -> Result String (Decimal Int)
fromDecimalBounded (Decimal r s d) =
    Arithmetic.fromIntBounded d |> Result.map (Decimal r s)


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
            |> Result.andThen (Rational.multiply rational)
            |> Result.map Rational.truncate
            |> Result.map (succeed r (s + 1))
            |> Result.map (roundDecimal s)


fromRationalBounded : RoundingAlgorythm -> Nat -> Rational -> Result String (Decimal Int)
fromRationalBounded r s f =
    fromRational r s f |> Result.andThen fromDecimalBounded


addBounded : Decimal Int -> Decimal Int -> Result String (Decimal Int)
addBounded (Decimal r1 s1 d1) (Decimal _ _ d2) =
    Arithmetic.addBounded d1 d2 |> Result.map (Decimal r1 s1)


subtractBounded : Decimal Int -> Decimal Int -> Result String (Decimal Int)
subtractBounded (Decimal r1 s1 d1) (Decimal _ _ d2) =
    Arithmetic.subtractBounded d1 d2 |> Result.map (Decimal r1 s1)


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


divideBounded : Decimal Int -> Decimal Int -> Result String (Decimal Int)
divideBounded (Decimal r s d1) (Decimal _ _ d2) =
    if d2 == 0 then
        Err "Divide by zero"

    else
        Rational.fraction d1 d2
            |> Result.andThen (fromRationalBounded r s)


fromIntsScaleBounded : RoundingAlgorythm -> Nat -> Int -> Int -> Result String (Decimal Int)
fromIntsScaleBounded r s x y =
    (x * (10 ^ s)) + y |> Arithmetic.fromIntBounded |> Result.map (Decimal r s)



-- SHOWING


toString : Decimal Int -> String
toString (Decimal _ s p) =
    let
        ( q, r ) =
            quotRem p (10 ^ s)

        formatted =
            String.fromInt q ++ "." ++ String.padRight s '0' (Basics.abs r |> String.fromInt)
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


fromString : RoundingAlgorythm -> Nat -> String -> Result String (Decimal Int)
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
