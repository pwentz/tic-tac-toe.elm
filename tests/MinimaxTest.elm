module MinimaxTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Minimax exposing (..)
import Config exposing (Symbol(X, O))
import Board
import GameTree as Tree
import Position
import TestHelper as Helper


suite : Test
suite =
    describe "Minimax"
        [ describe "tally"
            [ test "it can score the positions and sort them by score" <|
                \_ ->
                    let
                        cpu =
                            O

                        squares =
                            [ ( Position.create 0, cpu )
                            , ( Position.create 1, X )
                            , ( Position.create 4, cpu )
                            , ( Position.create 6, X )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.map ((List.filterMap Tree.score) << (tally cpu))
                            |> Expect.equal (Just [ 9, 0, 0, 0, 0 ])
            , test "it gives the highest score to the position that can lead to victory" <|
                \_ ->
                    let
                        cpu =
                            O

                        squares =
                            [ ( Position.create 0, cpu )
                            , ( Position.create 1, X )
                            , ( Position.create 4, cpu )
                            , ( Position.create 6, X )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.map (tally cpu)
                            |> Maybe.andThen List.head
                            |> Maybe.andThen Tree.diffSquare
                            |> Expect.equal (Just 8)
            , test "it returns a negative score for positions that lead to the opponent winning" <|
                \_ ->
                    let
                        cpu =
                            O

                        squares =
                            [ ( Position.create 7, cpu )
                            , ( Position.create 1, cpu )
                            , ( Position.create 4, X )
                            , ( Position.create 6, X )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.map ((List.filterMap Tree.score) << (tally cpu))
                            |> Expect.equal (Just [ 0, -9, -9, -9, -9 ])
            , test "it returns a higher score for position that block opponents forks" <|
                \_ ->
                    let
                        cpu =
                            X

                        squares =
                            [ ( Position.create 6, O )
                            , ( Position.create 2, O )
                            , ( Position.create 4, cpu )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.map (tally cpu)
                            |> Maybe.andThen List.head
                            |> Maybe.andThen Tree.diffSquare
                            |> Expect.equal (Just 1)
            , test "it can block forks at different positions" <|
                \_ ->
                    let
                        cpu =
                            X

                        squares =
                            [ ( Position.create 7, O )
                            , ( Position.create 3, O )
                            , ( Position.create 4, cpu )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.map (tally cpu)
                            |> Maybe.andThen List.head
                            |> Maybe.andThen Tree.diffSquare
                            |> Expect.equal (Just 6)
            , test "it gives higher score to position that can win than those that let opponent win" <|
                \_ ->
                    let
                        cpu =
                            X

                        squares =
                            [ ( Position.create 0, cpu )
                            , ( Position.create 3, cpu )
                            , ( Position.create 4, O )
                            , ( Position.create 7, O )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.map (tally cpu)
                            |> Maybe.andThen List.head
                            |> Maybe.andThen Tree.diffSquare
                            |> Expect.equal (Just 6)
            ]
        ]
