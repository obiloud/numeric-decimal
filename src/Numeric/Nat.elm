module Numeric.Nat exposing
    ( Nat
    , succeed, unwrap
    , add, subtract
    , nat0, nat1, nat2, nat3, nat4, nat5, successor
    )

{-|


# Definition

@docs Nat


# Methods

@docs succeed, unwrap


# Arithmetic

@docs add, subtract


# primitives

@docs nat0, nat1, nat2, nat3, nat4, nat5, successor

-}


{-| Naturals with zero.
-}
type Nat a
    = Nat Int


{-| Nat from Int
-}
succeed : Int -> Nat a
succeed =
    abs >> Nat


{-| Unwrap Nat
-}
unwrap : Nat a -> Int
unwrap (Nat x) =
    x


{-| Add two Nats.
-}
add : Nat a -> Nat a -> Nat a
add (Nat x) (Nat y) =
    Nat (x + y)


{-| Subtract one Nat from another.
-}
subtract : Nat a -> Nat a -> Nat a
subtract (Nat x) (Nat y) =
    Nat (x - y)


{-| Increment by 1
-}
successor : Nat a -> Nat a
successor (Nat x) =
    Nat (x + 1)


{-| Nat 0
-}
nat0 : Nat a
nat0 =
    succeed 0


{-| Nat 1
-}
nat1 : Nat a
nat1 =
    successor nat0


{-| Nat 2
-}
nat2 : Nat a
nat2 =
    successor nat1


{-| Nat 3
-}
nat3 : Nat a
nat3 =
    successor nat2


{-| Nat 4
-}
nat4 : Nat a
nat4 =
    successor nat3


{-| Nat 5
-}
nat5 : Nat a
nat5 =
    successor nat4
