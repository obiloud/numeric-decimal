module Numeric.Nat exposing
    ( Nat
    , fromIntAbs, fromIntOrZero, toInt
    , add
    , nat0, nat1, nat2, nat3, nat4, nat5, successor
    )

{-|


# Definition

@docs Nat


# Conversion

@docs fromIntAbs, fromIntOrZero, toInt


# Arithmetic

@docs add


# Primitives

@docs nat0, nat1, nat2, nat3, nat4, nat5, successor

-}


{-| Naturals with zero.
-}
type Nat
    = Nat Int


{-| Nat from `Int` absolute value.

    fromIntAbs 2 -- (Nat 2)

    fromIntAbs -3 -- (Nat 3)

-}
fromIntAbs : Int -> Nat
fromIntAbs =
    abs >> Nat


{-| Nat from positive `Int`s, otherwise 0

    fromIntAbs 2 -- (Nat 2)

    fromIntAbs -3 -- (Nat 0)

-}
fromIntOrZero : Int -> Nat
fromIntOrZero x =
    if x > 0 then
        Nat x

    else
        nat0


{-| toInt Nat
-}
toInt : Nat -> Int
toInt (Nat x) =
    x


{-| Add two Nats.
-}
add : Nat -> Nat -> Nat
add (Nat x) (Nat y) =
    Nat (x + y)


{-| Increment by 1
-}
successor : Nat -> Nat
successor (Nat x) =
    Nat (x + 1)


{-| Nat 0
-}
nat0 : Nat
nat0 =
    fromIntAbs 0


{-| Nat 1
-}
nat1 : Nat
nat1 =
    successor nat0


{-| Nat 2
-}
nat2 : Nat
nat2 =
    successor nat1


{-| Nat 3
-}
nat3 : Nat
nat3 =
    successor nat2


{-| Nat 4
-}
nat4 : Nat
nat4 =
    successor nat3


{-| Nat 5
-}
nat5 : Nat
nat5 =
    successor nat4
