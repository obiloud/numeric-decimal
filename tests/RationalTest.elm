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
        ]
