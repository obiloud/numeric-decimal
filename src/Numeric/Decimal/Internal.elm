module Numeric.Decimal.Internal exposing
    ( Decimal
    , andMap
    , divide
    , fromInt
    , fromString
    , getScale
    , map
    , map2
    , minus
    , multiply
    , plus
    , scaleUp
    , succeed
    , toString
    , withRounding
    )

import Numeric.Rational as Rational exposing (Rational)
import Parser exposing ((|.), (|=), Parser)


type Decimal r p
    = Decimal r Nat p


type alias Nat =
    Int


succeed : r -> Nat -> p -> Decimal r p
succeed r s p =
    Decimal r s p


map : (a -> b) -> Decimal r a -> Decimal r b
map f (Decimal r s p) =
    Decimal r s (f p)


map2 : (a -> b -> c) -> Decimal r a -> Decimal r b -> Decimal r c
map2 f (Decimal r s p1) (Decimal _ _ p2) =
    Decimal r s (f p1 p2)


andMap : Decimal r a -> Decimal r (a -> b) -> Decimal r b
andMap =
    map2 (|>)


withRounding : r1 -> Decimal r p -> Decimal r1 p
withRounding r (Decimal _ s p) =
    Decimal r s p


getScale : Decimal r p -> Nat
getScale (Decimal _ s _) =
    s


scaleUp : Nat -> Decimal r Int -> Decimal r Int
scaleUp k (Decimal r s p) =
    Decimal r k (p * (10 ^ Basics.abs (k - s)))



-- scaleUpBounded : Decimal r Int -> Result Bounded (Decimal r Int)
-- scaleUpBounded (Decimal r s p) =
--     fromIntBounded (p * (10 ^ s))
--         |> Result.map (timesBounded >> Decimal r s)


fromInt : r -> Nat -> Int -> Decimal r Int
fromInt r s p =
    Decimal r s (p * 10 ^ s)


plus : Decimal r Int -> Decimal r Int -> Decimal r Int
plus =
    map2 (+)


minus : Decimal r Int -> Decimal r Int -> Decimal r Int
minus =
    map2 (-)


divide : Decimal r Int -> Decimal r Int -> Result String (Decimal r Int)
divide (Decimal r s d1) (Decimal _ _ d2) =
    if d2 == 0 then
        Err "Divide by zero"

    else
        fromRational r s (Rational.fraction d1 d2)


fromRational : r -> Nat -> Rational -> Result String (Decimal r Int)
fromRational r s rational =
    let
        { num, den } =
            Rational.toFraction rational
    in
    if den == 0 then
        Err "Divide by zero"

    else
        let
            d =
                10 ^ (s + 1)

            { int } =
                Rational.multiply rational (Rational.fraction d 1) |> Rational.toParts
        in
        succeed r (s + 1) int |> roundDecimal s |> Ok


multiply : Decimal r Int -> Decimal r Int -> Decimal r Int
multiply (Decimal r s1 d1) (Decimal _ s2 d2) =
    Decimal r (s1 + s2) (d1 * d2) |> roundDecimal s1



-- RoundToZero


roundDecimal : Nat -> Decimal r Int -> Decimal r Int
roundDecimal k (Decimal r s d) =
    d // (10 ^ Basics.abs (s - k)) |> Decimal r k



-- BOUNDED


type Bounded
    = Overflow
    | Underflow


boundedToString : Bounded -> String
boundedToString x =
    case x of
        Overflow ->
            "Overflow"

        Underflow ->
            "Underflow"


fromIntBounded : Int -> Result Bounded Int
fromIntBounded x =
    if x > 9007199254740991 then
        Err Overflow

    else if x < -9007199254740991 then
        Err Underflow

    else
        Ok x


fromIntScaleBounded : r -> Nat -> Int -> Result Bounded (Decimal r Int)
fromIntScaleBounded r s x =
    fromIntBounded (x * (10 ^ s))
        |> Result.map (Decimal r s)


fromIntsScaleBounded : r -> Nat -> Int -> Int -> Result Bounded (Decimal r Int)
fromIntsScaleBounded r s x y =
    (x * (10 ^ s)) + y |> fromIntBounded |> Result.map (Decimal r s)



-- SHOWING


toString : Decimal r Int -> String
toString (Decimal _ s p) =
    let
        b =
            10 ^ s

        q =
            p // b

        r =
            Basics.remainderBy b p

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


fromString : r -> Nat -> String -> Result String (Decimal r Int)
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


parseDecimalBounded : r -> Nat -> Parser (Decimal r Int)
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
                        Parser.problem (boundedToString e)
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
