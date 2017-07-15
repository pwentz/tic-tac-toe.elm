module PositionTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Position exposing (..)
import Square


suite : Test
suite =
    describe "Position"
        [ describe "create"
            [ test "it takes an input and returns a valid position" <|
                \_ ->
                    create 6
                        |> isValid
                        |> Expect.equal True
            , test "if input is out of bounds of game, it returns an invalid position" <|
                \_ ->
                    create 12
                        |> isValid
                        |> Expect.equal False
            ]
        , describe "map"
            [ test "it takes a (Int -> a) function and maps over position" <|
                \_ ->
                    create 5
                        |> map Square.createEmpty
                        |> Expect.equal (Just (Square.createEmpty (Position.create 5)))
            , test "if position is invalid, it does not map over and returns nothing" <|
                \_ ->
                    create 9
                        |> map Square.createEmpty
                        |> Expect.equal Nothing
            ]
        , describe "value"
            [ test "it takes a Position and returns the value of that Position" <|
                \_ ->
                    create 8
                        |> Position.value
                        |> Expect.equal (Just 8)
            ]
        ]
