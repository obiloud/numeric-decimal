module DecimalTest exposing (suite)

import Expect
import Numeric.Decimal exposing (RoundingAlgorythm(..))
import Numeric.Decimal.Internal as D
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
        , describe "Parse"
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
            ]
        ]
