module ProrataTest exposing (suite)

import Dict exposing (Dict)
import Expect
import Numeric.Decimal as Decimal exposing (Decimal)
import Numeric.Decimal.Rounding as Decimal
import Numeric.Rational as Rational
import Test exposing (Test, describe, test)


totalLoanAmount : Int
totalLoanAmount =
    1400000


principal : String
principal =
    "33922.81"


interest : String
interest =
    "11067.43"


sellerFee : String
sellerFee =
    "101.23"


participants : Dict String Int
participants =
    Dict.fromList
        [ ( "participant 1", 37 )
        , ( "participant 2", 32 )
        , ( "participant 3", 19 )
        , ( "participant 4", 2 )
        ]


succeed : Int -> Decimal Int
succeed =
    Decimal.succeed Decimal.HalfToEven 2


parse : String -> Result String (Decimal Int)
parse =
    Decimal.fromString Decimal.HalfToEven 2


fromInt : Int -> Decimal Int
fromInt =
    Decimal.fromInt Decimal.HalfToEven 2



--


principalD : Decimal Int
principalD =
    parse principal
        |> Result.withDefault (succeed 0)


interestD : Decimal Int
interestD =
    parse interest
        |> Result.withDefault (succeed 0)


percentSold : Decimal Int
percentSold =
    participants
        |> Dict.toList
        |> List.map Tuple.second
        |> List.foldl (+) 0
        |> succeed


sellerFeeD : Decimal Int
sellerFeeD =
    let
        sellerFeeR =
            parse sellerFee
                |> Result.map Decimal.toRational
                |> Result.withDefault (Rational.fromInt 1)
    in
    Rational.divide (Rational.fraction 100 100) (Decimal.toRational percentSold)
        |> Rational.multiply sellerFeeR
        |> Decimal.fromRational Decimal.HalfToEven 2
        |> Result.withDefault (succeed 0)


borrowerPayment : Decimal Int
borrowerPayment =
    Decimal.add principalD interestD


soldToParticipants : Decimal Int
soldToParticipants =
    Decimal.multiply borrowerPayment percentSold


totalDisbursement : Decimal Int
totalDisbursement =
    let
        fee =
            parse sellerFee
    in
    Result.map2 Decimal.subtract (Ok soldToParticipants) fee
        |> Result.withDefault soldToParticipants


participation : String -> Decimal Int
participation key =
    Dict.get key participants
        |> Maybe.withDefault 0
        |> succeed


principalBreakdown : Decimal Int -> Decimal Int
principalBreakdown =
    Decimal.multiply principalD


interestBreakdown : Decimal Int -> Decimal Int
interestBreakdown =
    Decimal.multiply interestD


sellerFeeBreakdown : Decimal Int -> Decimal Int
sellerFeeBreakdown =
    Decimal.multiply sellerFeeD


totalBreakdown : Decimal Int -> Decimal Int
totalBreakdown commited =
    let
        payment =
            Decimal.add
                (principalBreakdown commited
                 -- |> Debug.log "principal"
                )
                (interestBreakdown commited
                 -- |> Debug.log "interest"
                )

        -- |> Debug.log "payment"
    in
    Decimal.subtract payment
        (sellerFeeBreakdown commited
         -- |> Debug.log "sellerFee"
        )



-- |> Debug.log "total"


suite : Test
suite =
    describe "Test decimal for calulating pro rata"
        [ test "Total borrower payment" <|
            \_ ->
                borrowerPayment
                    |> Decimal.toString
                    |> Expect.equal "44990.24"
        , test "Sold to participants" <|
            \_ ->
                soldToParticipants
                    |> Decimal.toString
                    |> Expect.equal "40491.22"
        , test "Total disbursement (after fees)" <|
            \_ ->
                totalDisbursement
                    |> Decimal.toString
                    |> Expect.equal "40389.99"
        , describe "Principal breakdown per participant"
            [ test "Principal participant 1" <|
                \_ ->
                    principalBreakdown (participation "participant 1")
                        |> Decimal.toString
                        |> Expect.equal "12551.44"
            , test "Principal participant 2" <|
                \_ ->
                    principalBreakdown (participation "participant 2")
                        |> Decimal.toString
                        |> Expect.equal "10855.30"
            , test "Principal participant 3" <|
                \_ ->
                    principalBreakdown (participation "participant 3")
                        |> Decimal.toString
                        |> Expect.equal "6445.33"
            , test "Principal participant 4" <|
                \_ ->
                    principalBreakdown (participation "participant 4")
                        |> Decimal.toString
                        |> Expect.equal "678.46"
            ]
        , describe "Interest breakdown per participant"
            [ test "Interest participant 1" <|
                \_ ->
                    interestBreakdown (participation "participant 1")
                        |> Decimal.toString
                        |> Expect.equal "4094.95"
            , test "Interest participant 2" <|
                \_ ->
                    interestBreakdown (participation "participant 2")
                        |> Decimal.toString
                        |> Expect.equal "3541.58"
            , test "Interest participant 3" <|
                \_ ->
                    interestBreakdown (participation "participant 3")
                        |> Decimal.toString
                        |> Expect.equal "2102.81"
            , test "Interest participant 4" <|
                \_ ->
                    interestBreakdown (participation "participant 4")
                        |> Decimal.toString
                        |> Expect.equal "221.35"
            ]
        , describe "Seller fee breakdown per participant"
            [ test "Fee participant 1" <|
                \_ ->
                    sellerFeeBreakdown (participation "participant 1")
                        |> Decimal.toString
                        |> Expect.equal "41.62"
            , test "Fee participant 2" <|
                \_ ->
                    sellerFeeBreakdown (participation "participant 2")
                        |> Decimal.toString
                        |> Expect.equal "35.99"
            , test "Fee participant 3" <|
                \_ ->
                    sellerFeeBreakdown (participation "participant 3")
                        |> Decimal.toString
                        |> Expect.equal "21.37"
            , test "Fee participant 4" <|
                \_ ->
                    sellerFeeBreakdown (participation "participant 4")
                        |> Decimal.toString
                        |> Expect.equal "2.25"
            ]
        , describe "Total breakdown per participant"
            [ test "Total participant 1" <|
                \_ ->
                    totalBreakdown (participation "participant 1")
                        |> Decimal.toString
                        |> Expect.equal "16604.77"
            , test "Total participant 2" <|
                \_ ->
                    -- 14360.88 --penny more
                    totalBreakdown (participation "participant 2")
                        |> Decimal.toString
                        |> Expect.equal "14360.89"
            , test "Total participant 3" <|
                \_ ->
                    totalBreakdown (participation "participant 3")
                        |> Decimal.toString
                        |> Expect.equal "8526.77"
            , test "Total participant 4" <|
                \_ ->
                    totalBreakdown (participation "participant 4")
                        |> Decimal.toString
                        |> Expect.equal "897.56"
            ]
        , describe "Total sum"
            [ test "Summ of all breakdowns" <|
                \_ ->
                    participants
                        |> Dict.keys
                        |> List.foldl (Decimal.add << totalBreakdown << participation) (succeed 0)
                        |> Expect.equal totalDisbursement
            ]
        , test "Rounding weirdly" <|
            \_ ->
                Decimal.succeed Decimal.HalfToEven 9 14360883911111
                    |> Decimal.roundDecimal 2
                    |> Decimal.toString
                    |> Expect.equal "14360.88"
        ]
