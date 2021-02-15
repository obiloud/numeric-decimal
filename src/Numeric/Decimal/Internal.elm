module Numeric.Decimal.Internal exposing
    ( Decimal
    , andMap
    , fromInt
    , fromString
    , map
    , map2
    , succeed
    , toString
    )

import Parser exposing ((|.), (|=), Parser)


type Decimal r p
    = Decimal r Int p


succeed : r -> Int -> p -> Decimal r p
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



--


fromInt : r -> Int -> Int -> Decimal r Int
fromInt r s p =
    Decimal r s (p * 10 ^ s)


fromInts : r -> Int -> Int -> Int -> Decimal r Int
fromInts r s x y =
    (x * 10 ^ s)
        + y
        |> Decimal r s



-- BOUNDED


type Bounded
    = Overflow
    | Underflow


fromIntBounded : Int -> Result Bounded Int
fromIntBounded x =
    if x > 9007199254740991 then
        Err Overflow

    else if x < -9007199254740991 then
        Err Underflow

    else
        Ok x


fromIntScaleBounded : r -> Int -> Int -> Result Bounded (Decimal r Int)
fromIntScaleBounded r s x =
    fromIntBounded (x * (10 ^ s))
        |> Result.map (fromInt r s)



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


fromString : r -> Int -> String -> Result String (Decimal r Int)
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


parseDecimalBounded : r -> Int -> Parser (Decimal r Int)
parseDecimalBounded r s =
    Parser.succeed (constructDecimal r s)
        |= parseSign
        |= parseDigits
        |. Parser.oneOf [ Parser.symbol ".", Parser.succeed () ]
        |= parseFractionalPart s


constructDecimal : r -> Int -> (Int -> Int) -> String -> String -> Decimal r Int
constructDecimal r s negate decimal fractional =
    if String.isEmpty fractional then
        fromInt r s (String.toInt decimal |> Maybe.map negate |> Maybe.withDefault 0)

    else
        fromInts r
            s
            (String.toInt decimal |> Maybe.map negate |> Maybe.withDefault 0)
            (String.toInt fractional |> Maybe.map negate |> Maybe.withDefault 0)


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


parseFractionalPart : Int -> Parser String
parseFractionalPart s =
    Parser.oneOf
        [ parseDigits
            |> Parser.andThen
                (\x ->
                    if String.length x > s then
                        Parser.problem ("Too much text after the decimal: " ++ x)

                    else
                        Parser.succeed x
                )
        , Parser.succeed ""
        ]
