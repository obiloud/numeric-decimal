module DecimalTest exposing (suite)

import Expect
import Numeric.Decimal as D
import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
import Numeric.Integer exposing (maxBound, minBound)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Decimal specs"
        [ describe "Construct"
            [ test "Pure 10" <|
                \_ ->
                    let
                        x =
                            D.succeed RoundTowardsZero 0 10
                    in
                    Expect.equal (D.toString x) "10"
            , test "From Int 10" <|
                \_ ->
                    let
                        x =
                            D.fromInt RoundTowardsZero 0 10
                    in
                    Expect.equal (D.toString x) "10"
            , test "From Int 1234" <|
                \_ ->
                    let
                        x =
                            D.fromInt RoundTowardsZero 4 1234
                    in
                    Expect.equal (D.toString x) "1234.0000"
            , test "Pure 1.23" <|
                \_ ->
                    let
                        x =
                            D.succeed RoundTowardsZero 2 123
                    in
                    Expect.equal (D.toString x) "1.23"
            , test "Pure 1.234" <|
                \_ ->
                    let
                        x =
                            D.succeed RoundTowardsZero 3 -1234
                    in
                    Expect.equal (D.toString x) "-1.234"
            ]
        , describe "Parse `fromString`"
            [ test "parse 12.34" <|
                \_ ->
                    Expect.equal (D.fromString RoundTowardsZero 2 "12.34") (D.succeed RoundTowardsZero 2 1234 |> Ok)
            , test "parse -12.3" <|
                \_ ->
                    Expect.equal (D.fromString RoundTowardsZero 2 "-12.3" |> Result.map D.toString) (Ok "-12.30")
            , test "parse 333" <|
                \_ ->
                    Expect.equal (D.fromString RoundTowardsZero 4 "333" |> Result.map D.toString) (Ok "333.0000")
            , test "parse 33.333333333" <|
                \_ ->
                    Expect.equal (D.fromString RoundTowardsZero 4 "33.333333333" |> Result.map D.toString) (Err "Too much text after the decimal: 333333333")
            , test "parse 9007199254740995" <|
                \_ ->
                    Expect.equal (D.fromString RoundTowardsZero 2 "9007199254740991" |> Result.map D.toString) (Err "Overflow")
            , test "parse -9007199254740995" <|
                \_ ->
                    Expect.equal (D.fromString RoundTowardsZero 2 "-9007199254740991" |> Result.map D.toString) (Err "Underflow")
            ]
        , describe "Arithmetic"
            [ describe "No bounds"
                [ test "Add two decimals 1.2 + 2.1" <|
                    \_ ->
                        let
                            a =
                                D.fromString RoundTowardsZero 2 "1.2"

                            b =
                                D.fromString RoundTowardsZero 2 "2.1"
                        in
                        Expect.equal (Result.map2 D.plus a b |> Result.map D.toString) (Ok "3.30")
                , test "Subtract 1.2 - 2.1 scaled up" <|
                    \_ ->
                        let
                            a =
                                D.fromString RoundTowardsZero 1 "1.2"

                            b =
                                D.fromString RoundTowardsZero 1 "2.1"
                        in
                        Expect.equal (Result.map2 D.minus a b |> Result.map (D.scaleUp 2 >> D.toString)) (Ok "-0.90")
                , test "Subtract 1.2 - 2.1" <|
                    \_ ->
                        let
                            a =
                                D.fromString RoundTowardsZero 2 "1.2"

                            b =
                                D.fromString RoundTowardsZero 2 "2.1"
                        in
                        Expect.equal (Result.map2 D.minus a b |> Result.map D.toString) (Ok "-0.90")
                , test "Divide 124 / 4" <|
                    \_ ->
                        let
                            a =
                                D.fromString RoundTowardsZero 2 "124"

                            b =
                                D.fromString RoundTowardsZero 2 "4"
                        in
                        Expect.equal (Result.map2 D.divide a b |> Result.andThen (Result.map D.toString)) (Ok "31.00")
                , test "Multiply 1.25 * 4.00" <|
                    \_ ->
                        let
                            a =
                                D.fromString RoundTowardsZero 2 "1.25"

                            b =
                                D.fromString RoundTowardsZero 2 "4.00"
                        in
                        Expect.equal (Result.map2 D.multiply a b |> Result.map D.toString) (Ok "5.00")
                ]
            , describe "Bounded"
                [ test "adding bounded overflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero 0 maxBound

                            b =
                                D.fromInt RoundTowardsZero 0 1
                        in
                        Expect.equal (D.plusBounded a b) (Err "Overflow")
                , test "adding bounded underflow" <|
                    \_ ->
                        let
                            a =
                                D.succeed RoundTowardsZero 0 minBound

                            b =
                                D.succeed RoundTowardsZero 0 -1
                        in
                        Expect.equal (D.plusBounded a b) (Err "Underflow")
                , test "adding bounded Ok" <|
                    \_ ->
                        let
                            a =
                                D.succeed RoundTowardsZero 2 123

                            b =
                                D.succeed RoundTowardsZero 2 456
                        in
                        Expect.equal (D.plusBounded a b |> Result.map D.toString) (Ok "5.79")
                , test "adding bounded parsed Ok" <|
                    \_ ->
                        let
                            a =
                                D.fromString RoundTowardsZero 2 "1.23"

                            b =
                                D.fromString RoundTowardsZero 2 "4.56"
                        in
                        Expect.equal (Result.map2 D.plusBounded a b |> Result.andThen (Result.map D.toString)) (Ok "5.79")
                , test "subtracting bounded overflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero 0 maxBound

                            b =
                                D.fromInt RoundTowardsZero 0 -1
                        in
                        Expect.equal (D.minusBounded a b) (Err "Overflow")
                , test "subtracting bounded underflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero 0 minBound

                            b =
                                D.fromInt RoundTowardsZero 0 1
                        in
                        Expect.equal (D.minusBounded a b) (Err "Underflow")
                , test "division bounded Ok" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero 0 10

                            b =
                                D.fromInt RoundTowardsZero 0 2
                        in
                        Expect.equal (D.divideBounded a b |> Result.map D.toString) (Ok "5")
                , test "division bounded Err" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero 0 1

                            b =
                                D.fromInt RoundTowardsZero 0 0
                        in
                        Expect.equal (D.divideBounded a b) (Err "Divide by zero")
                , test "division bounded Overflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero 0 minBound

                            b =
                                D.fromInt RoundTowardsZero 0 -1
                        in
                        Expect.equal (D.divideBounded a b) (Err "Overflow")
                , test "division bounded Underflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero 0 maxBound

                            b =
                                D.fromInt RoundTowardsZero 0 -1
                        in
                        Expect.equal (D.divideBounded a b) (Err "Underflow")
                , test "multiply bounded OK" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero 3 1000

                            b =
                                D.fromInt RoundTowardsZero 3 125
                        in
                        Expect.equal (D.multiplyBounded a b |> Result.map D.toString) (Ok "125000.000")
                , test "multiply bounded Overflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero 3 maxBound

                            b =
                                D.fromInt RoundTowardsZero 3 2
                        in
                        Expect.equal (D.multiplyBounded a b) (Err "Overflow")
                , test "multiply bounded Underflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero 3 minBound

                            b =
                                D.fromInt RoundTowardsZero 3 2
                        in
                        Expect.equal (D.multiplyBounded a b) (Err "Underflow")
                ]
            ]
        , describe "Rounding"
            [ describe "HalfToEven"
                [ test "Round half even 1.25 - one decimal" <|
                    \_ ->
                        let
                            a =
                                D.fromString HalfToEven 2 "1.25"
                        in
                        Expect.equal (Result.map (D.roundDecimal 1 >> D.toString) a) (Ok "1.2")
                , test "Round half even 0.5 - integer " <|
                    \_ ->
                        let
                            a =
                                D.fromString HalfToEven 1 "0.5"
                        in
                        Expect.equal (Result.map (D.roundDecimal 0 >> D.toString) a) (Ok "0")
                , test "Round half even 1.5 - integer " <|
                    \_ ->
                        let
                            a =
                                D.fromString HalfToEven 1 "1.5"
                        in
                        Expect.equal (Result.map (D.roundDecimal 0 >> D.toString) a) (Ok "2")
                , test "Round half even 1.4 - integer " <|
                    \_ ->
                        let
                            a =
                                D.fromString HalfToEven 1 "1.4"
                        in
                        Expect.equal (Result.map (D.roundDecimal 0 >> D.toString) a) (Ok "1")
                ]
            ]
        ]
