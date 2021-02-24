module DecimalTest exposing (suite)

import Expect
import Numeric.Decimal as D exposing (Decimal)
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
    D.succeed RoundTowardsZero nat2



-- dollars : Int -> Decimal Dollars Int
-- dollars =
--     D.succeed RoundTowardsZero nat0


suite : Test
suite =
    describe "Decimal specs"
        [ describe "Construct"
            [ test "Pure 10" <|
                \_ ->
                    let
                        x =
                            D.succeed RoundTowardsZero nat0 10
                    in
                    Expect.equal (D.toString x) "10"
            , test "From Int 10" <|
                \_ ->
                    let
                        x =
                            D.fromInt RoundTowardsZero nat0 10
                    in
                    Expect.equal (D.toString x) "10"
            , test "From Int 1234" <|
                \_ ->
                    let
                        x =
                            D.fromInt RoundTowardsZero nat4 1234
                    in
                    Expect.equal (D.toString x) "1234.0000"
            , test "Pure 1.23" <|
                \_ ->
                    let
                        x =
                            D.succeed RoundTowardsZero nat2 123
                    in
                    Expect.equal (D.toString x) "1.23"
            , test "Pure 1.234" <|
                \_ ->
                    let
                        x =
                            D.succeed RoundTowardsZero nat3 -1234
                    in
                    Expect.equal (D.toString x) "-1.234"
            ]
        , describe "Destructure"
            [ test "Unwrap decimal" <|
                \_ ->
                    Expect.equal (D.succeed RoundTowardsZero nat3 1234 |> D.splitDecimal) ( 1, 234 )
            ]
        , describe "Parse `fromString`"
            [ test "parse 12.34" <|
                \_ ->
                    Expect.equal (D.fromString RoundTowardsZero nat2 "12.34") (D.succeed RoundTowardsZero nat2 1234 |> Ok)
            , test "parse -12.3" <|
                \_ ->
                    Expect.equal (D.fromString RoundTowardsZero nat2 "-12.3" |> Result.map D.toString) (Ok "-12.30")
            , test "parse 333" <|
                \_ ->
                    Expect.equal (D.fromString RoundTowardsZero nat4 "333" |> Result.map D.toString) (Ok "333.0000")
            , test "parse 33.333333333" <|
                \_ ->
                    Expect.equal (D.fromString RoundTowardsZero nat4 "33.333333333" |> Result.map D.toString) (Err "Too much text after the decimal: 333333333")
            , test "parse 9007199254740995" <|
                \_ ->
                    Expect.equal (D.fromString RoundTowardsZero nat2 "9007199254740991" |> Result.map D.toString) (Err "Overflow")
            , test "parse -9007199254740995" <|
                \_ ->
                    Expect.equal (D.fromString RoundTowardsZero nat2 "-9007199254740991" |> Result.map D.toString) (Err "Underflow")
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
              --             Expect.equal (D.add x y |> D.toString) "3.30"
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
                        Expect.equal (D.add a b |> D.toString) "3.30"
                , test "Subtract 1.2 - 2.1 scaled up" <|
                    \_ ->
                        let
                            a =
                                D.fromString RoundTowardsZero nat1 "1.2"

                            b =
                                D.fromString RoundTowardsZero nat1 "2.1"
                        in
                        Expect.equal (Result.map2 D.subtract a b |> Result.map (D.scaleUp nat2 >> D.toString)) (Ok "-0.90")
                , test "Subtract 1.2 - 2.1" <|
                    \_ ->
                        let
                            a =
                                D.fromString RoundTowardsZero nat2 "1.2"

                            b =
                                D.fromString RoundTowardsZero nat2 "2.1"
                        in
                        Expect.equal (Result.map2 D.subtract a b |> Result.map D.toString) (Ok "-0.90")
                , test "Divide 124 / 4" <|
                    \_ ->
                        let
                            a =
                                D.fromString RoundTowardsZero nat2 "124"

                            b =
                                D.fromString RoundTowardsZero nat2 "4"
                        in
                        Expect.equal (Result.map2 D.divide a b |> Result.andThen (Result.map D.toString)) (Ok "31.00")
                , test "Multiply 1.25 * 4.00" <|
                    \_ ->
                        let
                            a =
                                D.fromString RoundTowardsZero nat2 "1.25"

                            b =
                                D.fromString RoundTowardsZero nat2 "4.00"
                        in
                        Expect.equal (Result.map2 D.multiply a b |> Result.map D.toString) (Ok "5.00")
                ]
            , describe "Bounded"
                [ test "Scale up bounded" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero nat0 1000
                        in
                        Expect.equal (D.scaleUpBounded nat1 a |> Result.map D.toString) (Ok "1000.0")
                , test "Scale up bounded Overflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero nat0 maxBound
                        in
                        Expect.equal (D.scaleUpBounded nat1 a) (Err "Overflow")
                , test "adding bounded overflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero nat0 maxBound

                            b =
                                D.fromInt RoundTowardsZero nat0 1
                        in
                        Expect.equal (D.addBounded a b) (Err "Overflow")
                , test "adding bounded underflow" <|
                    \_ ->
                        let
                            a =
                                D.succeed RoundTowardsZero nat0 minBound

                            b =
                                D.succeed RoundTowardsZero nat0 -1
                        in
                        Expect.equal (D.addBounded a b) (Err "Underflow")
                , test "adding bounded Ok" <|
                    \_ ->
                        let
                            a =
                                D.succeed RoundTowardsZero nat2 123

                            b =
                                D.succeed RoundTowardsZero nat2 456
                        in
                        Expect.equal (D.addBounded a b |> Result.map D.toString) (Ok "5.79")
                , test "adding bounded parsed Ok" <|
                    \_ ->
                        let
                            a =
                                D.fromString RoundTowardsZero nat2 "1.23"

                            b =
                                D.fromString RoundTowardsZero nat2 "4.56"
                        in
                        Expect.equal (Result.map2 D.addBounded a b |> Result.andThen (Result.map D.toString)) (Ok "5.79")
                , test "subtracting bounded overflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero nat0 maxBound

                            b =
                                D.fromInt RoundTowardsZero nat0 -1
                        in
                        Expect.equal (D.subtractBounded a b) (Err "Overflow")
                , test "subtracting bounded underflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero nat0 minBound

                            b =
                                D.fromInt RoundTowardsZero nat0 1
                        in
                        Expect.equal (D.subtractBounded a b) (Err "Underflow")
                , test "division bounded Ok" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero nat0 10

                            b =
                                D.fromInt RoundTowardsZero nat0 2
                        in
                        Expect.equal (D.divideBounded a b |> Result.map D.toString) (Ok "5")
                , test "division bounded Err" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero nat0 1

                            b =
                                D.fromInt RoundTowardsZero nat0 0
                        in
                        Expect.equal (D.divideBounded a b) (Err "Divide by zero")
                , test "division bounded Overflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero nat0 minBound

                            b =
                                D.fromInt RoundTowardsZero nat0 -1
                        in
                        Expect.equal (D.divideBounded a b) (Err "Overflow")
                , test "division bounded Underflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero nat0 maxBound

                            b =
                                D.fromInt RoundTowardsZero nat0 -1
                        in
                        Expect.equal (D.divideBounded a b) (Err "Underflow")
                , test "multiply bounded OK" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero nat3 1000

                            b =
                                D.fromInt RoundTowardsZero nat3 125
                        in
                        Expect.equal (D.multiplyBounded a b |> Result.map D.toString) (Ok "125000.000")
                , test "multiply bounded Overflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero nat3 maxBound

                            b =
                                D.fromInt RoundTowardsZero nat3 2
                        in
                        Expect.equal (D.multiplyBounded a b) (Err "Overflow")
                , test "multiply bounded Underflow" <|
                    \_ ->
                        let
                            a =
                                D.fromInt RoundTowardsZero nat3 minBound

                            b =
                                D.fromInt RoundTowardsZero nat3 2
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
                                D.fromString HalfToEven nat2 "1.25"
                        in
                        Expect.equal (Result.map (D.roundDecimal nat1 >> D.toString) a) (Ok "1.2")
                , test "Round half even 0.5 - integer " <|
                    \_ ->
                        let
                            a =
                                D.fromString HalfToEven nat1 "0.5"
                        in
                        Expect.equal (Result.map (D.roundDecimal nat0 >> D.toString) a) (Ok "0")
                , test "Round half even 1.5 - integer " <|
                    \_ ->
                        let
                            a =
                                D.fromString HalfToEven nat1 "1.5"
                        in
                        Expect.equal (Result.map (D.roundDecimal nat0 >> D.toString) a) (Ok "2")
                , test "Round half even 1.4 - integer " <|
                    \_ ->
                        let
                            a =
                                D.fromString HalfToEven nat1 "1.4"
                        in
                        Expect.equal (Result.map (D.roundDecimal nat0 >> D.toString) a) (Ok "1")
                ]
            ]
        ]
