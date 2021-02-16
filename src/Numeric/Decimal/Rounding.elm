module Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))

{-| Rounding Algorythm
-}


type RoundingAlgorythm
    = RoundDown
    | RoundUp
    | RoundTowardsZero
    | RoundAwayFromZero
    | HalfUp
    | HalfDown
    | HalfTowardsZero
    | HalfAwayFromZero
    | HalfToEven
    | HalfToOdd
