module DecimalTest exposing (suite)

import Expect
import Numeric.ArithmeticError exposing (ArithmeticError(..))
import Numeric.Decimal as Decimal exposing (Decimal)
import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
import Numeric.Integer exposing (maxBound, minBound)
import Numeric.Nat exposing (nat0, nat1, nat2, nat3, nat4)
import Test exposing (Test, describe, test)


type Pennies
    = Pennies



-- type Dollars
--     = Dollars


pennies : Int -> Decimal Pennies Int
pennies =
    Decimal.succeed RoundTowardsZero nat2



-- dollars : Int -> Decimal Dollars Int
-- dollars =
--     Decimal.succeed RoundTowardsZero nat0


int32 : Int
int32 =
    2 ^ 31 - 1


int32negative : Int
int32negative =
    -2 ^ 31 - 1


suite : Test
suite =
    describe "Decimal specs"
        [ describe "Construct"
            [ test "Pure 10" <|
                \_ ->
                    let
                        x =
                            Decimal.succeed RoundTowardsZero nat0 10
                    in
                    Expect.equal (Decimal.toString x) "10"
            , test "From Int 10" <|
                \_ ->
                    let
                        x =
                            Decimal.fromInt RoundTowardsZero nat0 10
                    in
                    Expect.equal (Decimal.toString x) "10"
            , test "From Int 1234" <|
                \_ ->
                    let
                        x =
                            Decimal.fromInt RoundTowardsZero nat4 1234
                    in
                    Expect.equal (Decimal.toString x) "1234.0000"
            , test "Pure 1.23" <|
                \_ ->
                    let
                        x =
                            Decimal.succeed RoundTowardsZero nat2 123
                    in
                    Expect.equal (Decimal.toString x) "1.23"
            , test "Pure 1.234" <|
                \_ ->
                    let
                        x =
                            Decimal.succeed RoundTowardsZero nat3 -1234
                    in
                    Expect.equal (Decimal.toString x) "-1.234"
            ]
        , describe "Destructure"
            [ test "Unwrap decimal" <|
                \_ ->
                    Expect.equal (Decimal.succeed RoundTowardsZero nat3 1234 |> Decimal.splitDecimal) ( 1, 234 )
            ]
        , describe "Parse `fromString`"
            [ test "parse 12.34" <|
                \_ ->
                    Expect.equal (Decimal.fromString RoundTowardsZero nat2 "12.34") (Decimal.succeed RoundTowardsZero nat2 1234 |> Ok)
            , test "parse -12.3" <|
                \_ ->
                    Expect.equal (Decimal.fromString RoundTowardsZero nat2 "-12.3" |> Result.map Decimal.toString) (Ok "-12.30")
            , test "parse 333" <|
                \_ ->
                    Expect.equal (Decimal.fromString RoundTowardsZero nat4 "333" |> Result.map Decimal.toString) (Ok "333.0000")
            , test "parse 33.333333333" <|
                \_ ->
                    Expect.equal (Decimal.fromString RoundTowardsZero nat4 "33.333333333" |> Result.map Decimal.toString) (Err (ParsingProblem "Too much text after the decimal: 333333333"))
            , test "parse 9007199254740995" <|
                \_ ->
                    Expect.equal (Decimal.fromString RoundTowardsZero nat2 "9007199254740991" |> Result.map Decimal.toString) (Err Overflow)
            , test "parse -9007199254740995" <|
                \_ ->
                    Expect.equal (Decimal.fromString RoundTowardsZero nat2 "-9007199254740991" |> Result.map Decimal.toString) (Err Underflow)
            ]
        , describe "Arithmetic"
            [ --     describe "Type safe"
              --     [ test "Add dollars to pennies" <|
              --         \_ ->
              --             let
              --                 x =
              --                     pennies 120
              --                 y =
              --                     dollars 210
              --             in
              --             Expect.equal (Decimal.add x y |> Decimal.toString) "3.30"
              --     ]
              -- ,
              describe "No bounds"
                [ test "Add two decimals 1.2 + 2.1" <|
                    \_ ->
                        let
                            a =
                                pennies 120

                            b =
                                pennies 210
                        in
                        Expect.equal (Decimal.add a b |> Decimal.toString) "3.30"
                , test "Subtract 1.2 - 2.1 scaled up" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromString RoundTowardsZero nat1 "1.2"

                            b =
                                Decimal.fromString RoundTowardsZero nat1 "2.1"
                        in
                        Expect.equal (Result.map2 Decimal.subtract a b |> Result.map (Decimal.scaleUp nat2 >> Decimal.toString)) (Ok "-0.90")
                , test "Subtract 1.2 - 2.1" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromString RoundTowardsZero nat2 "1.2"

                            b =
                                Decimal.fromString RoundTowardsZero nat2 "2.1"
                        in
                        Expect.equal (Result.map2 Decimal.subtract a b |> Result.map Decimal.toString) (Ok "-0.90")
                , test "Divide 124 / 4" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromString RoundTowardsZero nat2 "124"

                            b =
                                Decimal.fromString RoundTowardsZero nat2 "4"
                        in
                        Expect.equal (Result.map2 Decimal.divide a b |> Result.andThen (Result.map Decimal.toString)) (Ok "31.00")
                , test "Multiply 1.25 * 4.00" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromString RoundTowardsZero nat2 "1.25"

                            b =
                                Decimal.fromString RoundTowardsZero nat2 "4.00"
                        in
                        Expect.equal (Result.map2 Decimal.multiply a b |> Result.map Decimal.toString) (Ok "5.00")
                ]
            , describe "Bounded"
                [ test "Scale up bounded" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromInt RoundTowardsZero nat0 1000
                        in
                        Expect.equal (Decimal.scaleUpBounded nat1 a |> Result.map Decimal.toString) (Ok "1000.0")
                , test "Scale up bounded Overflow" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromInt RoundTowardsZero nat0 maxBound
                        in
                        Expect.equal (Decimal.scaleUpBounded nat1 a) (Err Overflow)
                , test "adding bounded overflow" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromInt RoundTowardsZero nat0 maxBound

                            b =
                                Decimal.fromInt RoundTowardsZero nat0 1
                        in
                        Expect.equal (Decimal.addBounded a b) (Err Overflow)
                , test "adding bounded underflow" <|
                    \_ ->
                        let
                            a =
                                Decimal.succeed RoundTowardsZero nat0 minBound

                            b =
                                Decimal.succeed RoundTowardsZero nat0 -1
                        in
                        Expect.equal (Decimal.addBounded a b) (Err Underflow)
                , test "adding bounded Ok" <|
                    \_ ->
                        let
                            a =
                                Decimal.succeed RoundTowardsZero nat2 123

                            b =
                                Decimal.succeed RoundTowardsZero nat2 456
                        in
                        Expect.equal (Decimal.addBounded a b |> Result.map Decimal.toString) (Ok "5.79")
                , test "adding bounded parsed Ok" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromString RoundTowardsZero nat2 "1.23"

                            b =
                                Decimal.fromString RoundTowardsZero nat2 "4.56"
                        in
                        Expect.equal (Result.map2 Decimal.addBounded a b |> Result.andThen (Result.map Decimal.toString)) (Ok "5.79")
                , test "subtracting bounded overflow" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromInt RoundTowardsZero nat0 maxBound

                            b =
                                Decimal.fromInt RoundTowardsZero nat0 -1
                        in
                        Expect.equal (Decimal.subtractBounded a b) (Err Overflow)
                , test "subtracting bounded underflow" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromInt RoundTowardsZero nat0 minBound

                            b =
                                Decimal.fromInt RoundTowardsZero nat0 1
                        in
                        Expect.equal (Decimal.subtractBounded a b) (Err Underflow)
                , test "division bounded Ok" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromInt RoundTowardsZero nat0 10

                            b =
                                Decimal.fromInt RoundTowardsZero nat0 2
                        in
                        Expect.equal (Decimal.divideBounded a b |> Result.map Decimal.toString) (Ok "5")
                , test "division bounded Err" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromInt RoundTowardsZero nat0 1

                            b =
                                Decimal.fromInt RoundTowardsZero nat0 0
                        in
                        Expect.equal (Decimal.divideBounded a b) (Err DivisionByZero)
                , test "division bounded Overflow" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromInt RoundTowardsZero nat0 minBound

                            b =
                                Decimal.fromInt RoundTowardsZero nat0 -1
                        in
                        Expect.equal (Decimal.divideBounded a b) (Err Overflow)
                , test "division bounded Underflow" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromInt RoundTowardsZero nat0 maxBound

                            b =
                                Decimal.fromInt RoundTowardsZero nat0 -1
                        in
                        Expect.equal (Decimal.divideBounded a b) (Err Underflow)
                , test "multiply bounded OK" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromInt RoundTowardsZero nat3 1000

                            b =
                                Decimal.fromInt RoundTowardsZero nat3 125
                        in
                        Expect.equal (Decimal.multiplyBounded a b |> Result.map Decimal.toString) (Ok "125000.000")
                , test "multiply bounded Overflow" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromInt RoundTowardsZero nat3 maxBound

                            b =
                                Decimal.fromInt RoundTowardsZero nat3 2
                        in
                        Expect.equal (Decimal.multiplyBounded a b) (Err Overflow)
                , test "multiply bounded Underflow" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromInt RoundTowardsZero nat3 minBound

                            b =
                                Decimal.fromInt RoundTowardsZero nat3 2
                        in
                        Expect.equal (Decimal.multiplyBounded a b) (Err Underflow)
                ]
            ]
        , describe "Rounding"
            [ describe "RoundDown"
                [ test "Round down" <|
                    \_ ->
                        let
                            x =
                                Decimal.fromString RoundDown nat1 (String.fromInt int32 ++ ".2")
                        in
                        Expect.equal (Result.map (Decimal.roundDecimal nat0 >> Decimal.toString) x) (String.fromInt int32 |> Ok)
                , test "Round down negative" <|
                    \_ ->
                        let
                            x =
                                Decimal.fromString RoundDown nat1 (String.fromInt int32negative ++ ".2")
                        in
                        Expect.equal (Result.map (Decimal.roundDecimal nat0 >> Decimal.toString) x) (String.fromInt (int32negative - 1) |> Ok)
                ]
            , describe "RoundUp"
                [ test "Round up" <|
                    \_ ->
                        let
                            x =
                                Decimal.fromString RoundUp nat1 (String.fromInt int32 ++ ".2")
                        in
                        Expect.equal (Result.map (Decimal.roundDecimal nat0 >> Decimal.toString) x) (String.fromInt (int32 + 1) |> Ok)
                , test "Round up negative" <|
                    \_ ->
                        let
                            x =
                                Decimal.fromString RoundUp nat1 (String.fromInt int32negative ++ ".2")
                        in
                        Expect.equal (Result.map (Decimal.roundDecimal nat0 >> Decimal.toString) x) (String.fromInt int32negative |> Ok)
                ]
            , describe "RoundTowardsZero"
                [ test "Round to zero" <|
                    \_ ->
                        let
                            x =
                                Decimal.fromString RoundTowardsZero nat1 (String.fromInt int32 ++ ".2")
                        in
                        Expect.equal (Result.map (Decimal.roundDecimal nat0 >> Decimal.toString) x) (String.fromInt int32 |> Ok)
                , test "Round to zero negative" <|
                    \_ ->
                        let
                            x =
                                Decimal.fromString RoundTowardsZero nat1 (String.fromInt int32negative ++ ".2")
                        in
                        Expect.equal (Result.map (Decimal.roundDecimal nat0 >> Decimal.toString) x) (String.fromInt int32negative |> Ok)
                ]
            , describe "HalfToEven"
                [ test "Round half even 1.25 - one decimal" <|
                    \_ ->
                        let
                            a =
                                Decimal.fromString HalfToEven nat2 "1.25"
                        in
                        Expect.equal (Result.map (Decimal.roundDecimal nat1 >> Decimal.toString) a) (Ok "1.2")
                , test "Round half even 0.5 - integer " <|
                    \_ ->
                        let
                            a =
                                Decimal.fromString HalfToEven nat1 "0.5"
                        in
                        Expect.equal (Result.map (Decimal.roundDecimal nat0 >> Decimal.toString) a) (Ok "0")
                , test "Round half even 1.5 - integer " <|
                    \_ ->
                        let
                            a =
                                Decimal.fromString HalfToEven nat1 "1.5"
                        in
                        Expect.equal (Result.map (Decimal.roundDecimal nat0 >> Decimal.toString) a) (Ok "2")
                , test "Round half even 1.4 - integer " <|
                    \_ ->
                        let
                            a =
                                Decimal.fromString HalfToEven nat1 "1.4"
                        in
                        Expect.equal (Result.map (Decimal.roundDecimal nat0 >> Decimal.toString) a) (Ok "1")
                , test "Round half even to positive signed 32bit integer" <|
                    \_ ->
                        let
                            x =
                                Decimal.fromString HalfToEven nat2 (String.fromInt int32 ++ ".50")
                        in
                        Expect.equal (Result.map (Decimal.roundDecimal nat0) x) (Decimal.succeed HalfToEven nat0 (int32 + 1) |> Ok)
                , test "Round half even to negative signed 32bit integer" <|
                    \_ ->
                        let
                            x =
                                Decimal.fromString HalfToEven nat2 (String.fromInt int32negative ++ ".50")
                        in
                        Expect.equal (Result.map (Decimal.roundDecimal nat0) x) (Decimal.succeed HalfToEven nat0 (int32negative - 1) |> Ok)
                ]
            ]
        ]
