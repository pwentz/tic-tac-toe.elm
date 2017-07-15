module SquareTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Square exposing (..)
import Config exposing (Symbol(X, O))
import Position


suite : Test
suite =
    describe "Square"
        [ describe "createEmpty"
            [ test "it can take a position and create an EmptySquare" <|
                \_ ->
                    createEmpty (Position.create 6)
                        |> Maybe.map isEmpty
                        |> Expect.equal (Just True)
            , test "it returns Nothing if passed an InvalidPosition" <|
                \_ ->
                    createEmpty (Position.create 9)
                        |> Expect.equal Nothing
            ]
        , describe "create"
            [ test "it takes a position and a symbol and creates a Square" <|
                \_ ->
                    create (Position.create 6) X
                        |> Maybe.map isEmpty
                        |> Expect.equal (Just False)
            , test "it returns Nothing when passed an InvalidPosition" <|
                \_ ->
                    create (Position.create 9) X
                        |> Expect.equal Nothing
            ]
        , describe "map"
            [ test "it takes a (Square -> a) function but doesn't operate over fn if EmptySquare" <|
                \_ ->
                    createEmpty (Position.create 6)
                        |> Maybe.andThen (Square.map isEmpty)
                        |> Expect.equal Nothing
            , test "it does operate if not an EmptySquare" <|
                \_ ->
                    create (Position.create 6) X
                        |> Maybe.andThen (Square.map isEmpty)
                        |> Expect.equal (Just False)
            ]
        , describe "position"
            [ test "it takes a Square and returns the position of that square" <|
                \_ ->
                    createEmpty (Position.create 3)
                        |> Maybe.map Square.position
                        |> Expect.equal (Just (Position.create 3))
            ]
        , describe "symbol"
            [ test "it takes a Square and returns the symbol of that Square" <|
                \_ ->
                    create (Position.create 1) O
                        |> Maybe.andThen Square.symbol
                        |> Expect.equal (Just O)
            , test "it returns Nothing if passed an EmptySquare" <|
                \_ ->
                    createEmpty (Position.create 1)
                        |> Maybe.andThen Square.symbol
                        |> Expect.equal Nothing
            ]
        ]
