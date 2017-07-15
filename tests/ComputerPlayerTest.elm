module ComputerPlayerTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import ComputerPlayer exposing (..)
import Config exposing (Symbol(X, O))
import Board
import Position
import TestHelper as Helper


suite : Test
suite =
    describe "ComputerPlayer"
        [ describe "makeMove"
            [ test "it can select a move that will win the game" <|
                \_ ->
                    let
                        cpu =
                            X

                        squares =
                            [ ( Position.create 0, O )
                            , ( Position.create 7, O )
                            , ( Position.create 4, cpu )
                            , ( Position.create 6, cpu )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.andThen (makeMove cpu)
                            |> Expect.equal (Just (Position.create 2))
            , test "it can select a move that will prevent opponent from winning" <|
                \_ ->
                    let
                        cpu =
                            X

                        squares =
                            [ ( Position.create 3, O )
                            , ( Position.create 4, O )
                            , ( Position.create 6, cpu )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.andThen (makeMove cpu)
                            |> Expect.equal (Just (Position.create 5))
            , test "it can select a move that will block opponent's forking attempt" <|
                \_ ->
                    let
                        cpu =
                            X

                        squares =
                            [ ( Position.create 5, O )
                            , ( Position.create 4, cpu )
                            , ( Position.create 7, O )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.andThen (makeMove cpu)
                            |> Expect.equal (Just (Position.create 8))
            , test "it always takes center, if possible, on first turn" <|
                \_ ->
                    let
                        cpu =
                            X

                        squares =
                            [ ( Position.create 0, O ) ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.andThen (makeMove cpu)
                            |> Expect.equal (Just (Position.create 4))
            ]
        ]
