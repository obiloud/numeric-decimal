module BoundedArithmeticTest exposing (suite)

import Expect
import Numeric.Decimal.BoundedArithmetic as Arithmetic
import Numeric.Integer exposing (maxBound, minBound)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Bounded Arithemtic"
        [ test "Upper bound" <|
            \_ -> Expect.notEqual maxBound (maxBound + 1)
        , test "Guard from out of bounds Overflow" <|
            \_ ->
                Expect.equal (Arithmetic.fromIntBounded (maxBound + 1)) (Err "Overflow")
        , test "Guard from out of bounds Underflow" <|
            \_ ->
                Expect.equal (Arithmetic.fromIntBounded (minBound - 1)) (Err "Underflow")
        , describe "Bounded addition"
            [ test "Add" (\_ -> Expect.equal 1 1)
            ]
        , describe "Bounded subtraction"
            [ test "Subtract" (\_ -> Expect.equal 1 1)
            ]
        , describe "Bounded multiplication"
            [ test "Multiply" (\_ -> Expect.equal 1 1)
            ]
        ]
