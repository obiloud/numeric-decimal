module RationalTest exposing (suite)

import Expect
import Numeric.Rational as R
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Test rationals"
        [ test "Rounding rational" <|
            \_ ->
                Expect.equal (R.ratio 5 2 |> R.round) 2
        , test "Rounding rational to even" <|
            \_ ->
                Expect.equal (R.ratio 195 10 |> R.round) 20
        , test "Power 2" <|
            \_ ->
                Expect.equal (R.ratio 2 3 |> R.power 2) (R.ratio 4 3)
        , test "Power -2" <|
            \_ ->
                Expect.equal (R.fromInt 2 |> R.power -2 |> R.toString) (R.ratio 1 4 |> R.toString)
        ]
