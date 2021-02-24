module IntegerTest exposing (suite)

import Expect
import Numeric.Integer exposing (div, gcd, lcm, mod, quot, rem, signum)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Integer laws tests"
        [ test "signum abs law" <|
            \_ ->
                Expect.equal (abs -3 * signum -3) -3
        , test "quot rem laws" <|
            \_ ->
                let
                    x =
                        7

                    y =
                        3
                in
                Expect.equal (quot x y * y + rem x y) x
        , test "div mod laws" <|
            \_ ->
                let
                    x =
                        9

                    y =
                        2
                in
                Expect.equal (div x y * y + mod x y) x
        , test "gcm" <|
            \_ ->
                let
                    x =
                        18

                    y =
                        6
                in
                Expect.equal (gcd x y) 6
        , test "lcm" <|
            \_ ->
                let
                    x =
                        9

                    y =
                        2
                in
                Expect.equal (lcm x y) 18
        ]
