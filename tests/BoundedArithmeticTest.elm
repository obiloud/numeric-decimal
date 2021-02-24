module BoundedArithmeticTest exposing (suite)

import Expect
import Numeric.ArithmeticError exposing (ArithmeticError(..))
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
                Expect.equal (Arithmetic.fromIntBounded (maxBound + 1)) (Err Overflow)
        , test "Guard from out of bounds Underflow" <|
            \_ ->
                Expect.equal (Arithmetic.fromIntBounded (minBound - 1)) (Err Underflow)
        , describe "Bounded addition"
            [ test "Add in bound" <|
                \_ -> Expect.equal (Arithmetic.addBounded 12345 32142) (Ok 44487)
            , test "Add out of bounds Overflow" <|
                \_ -> Expect.equal (Arithmetic.addBounded maxBound 1) (Err Overflow)
            , test "Add out of bounds Underflow" <|
                \_ -> Expect.equal (Arithmetic.addBounded minBound -1) (Err Underflow)
            ]
        , describe "Bounded subtraction"
            [ test "Subtract in bound" <|
                \_ -> Expect.equal (Arithmetic.subtractBounded 32142 12345) (Ok 19797)
            , test "Subtract out of bounds Overflow" <|
                \_ -> Expect.equal (Arithmetic.subtractBounded maxBound -1) (Err Overflow)
            , test "Subtract out of bounds Underflow" <|
                \_ -> Expect.equal (Arithmetic.subtractBounded minBound 1) (Err Underflow)
            ]
        , describe "Bounded multiplication"
            [ test "Multiply in bound" <|
                \_ -> Expect.equal (Arithmetic.multiplyBounded 2 5) (Ok 10)
            , test "Multiply out of bounds Overflow" <|
                \_ -> Expect.equal (Arithmetic.multiplyBounded (2 ^ 26) (2 ^ 27)) (Err Overflow)
            , test "Multiply out of bounds Underflow" <|
                \_ -> Expect.equal (Arithmetic.multiplyBounded (2 ^ 26) (-2 ^ 27)) (Err Underflow)
            ]
        ]
