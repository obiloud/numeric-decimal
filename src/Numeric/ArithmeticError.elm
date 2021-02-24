module Numeric.ArithmeticError exposing
    ( ArithmeticError(..)
    , parse, toString
    )

{-|


# Errors

@docs ArithmeticError


# Parsing / printing

@docs parse, toString

-}


{-| Arithmetic errors
-}
type ArithmeticError
    = Overflow
    | Underflow
    | DivisionByZero
    | ParsingProblem String


{-| Parse error
-}
parse : String -> ArithmeticError
parse str =
    case str of
        "Overflow" ->
            Overflow

        "Underflow" ->
            Underflow

        "Division by zero" ->
            DivisionByZero

        _ ->
            ParsingProblem str


{-| Print error
-}
toString : ArithmeticError -> String
toString err =
    case err of
        Overflow ->
            "Overflow"

        Underflow ->
            "Underflow"

        DivisionByZero ->
            "Division by zero"

        ParsingProblem problem ->
            "ParsingProblem: " ++ problem
