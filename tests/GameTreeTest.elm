module GameTreeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import GameTree exposing (..)
import Config exposing (Symbol(X, O))
import Board
import Position
import TestHelper as Helper


suite : Test
suite =
    describe "GameTree"
        [ describe "value"
            [ test "it contains the original state of the board" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, O )
                            , ( Position.create 1, X )
                            , ( Position.create 2, O )
                            , ( Position.create 3, X )
                            , ( Position.create 4, O )
                            , ( Position.create 5, X )
                            , ( Position.create 6, O )
                            , ( Position.create 7, X )
                            ]

                        board =
                            Helper.updateSquares squares Board.create
                    in
                        board
                            |> Maybe.map create
                            |> Maybe.andThen state
                            |> Expect.equal board
            , test "the root has a diffSquare of Nothing" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, O )
                            , ( Position.create 1, X )
                            , ( Position.create 2, O )
                            , ( Position.create 3, X )
                            , ( Position.create 4, O )
                            , ( Position.create 5, X )
                            , ( Position.create 6, O )
                            , ( Position.create 7, X )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.map create
                            |> Maybe.andThen diffSquare
                            |> Expect.equal Nothing
            , test "it creates a node with score of 0" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, O )
                            , ( Position.create 1, X )
                            , ( Position.create 2, O )
                            , ( Position.create 3, X )
                            , ( Position.create 4, O )
                            , ( Position.create 5, X )
                            , ( Position.create 6, O )
                            , ( Position.create 7, X )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.map create
                            |> Maybe.andThen score
                            |> Expect.equal (Just 0)
            , test "it creates a node with depth of 0" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, O )
                            , ( Position.create 1, X )
                            , ( Position.create 2, O )
                            , ( Position.create 3, X )
                            , ( Position.create 4, O )
                            , ( Position.create 5, X )
                            , ( Position.create 6, O )
                            , ( Position.create 7, X )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.map create
                            |> Maybe.andThen depth
                            |> Expect.equal (Just 0)
            , test "it creates a leaf node with no children" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, O )
                            , ( Position.create 1, X )
                            , ( Position.create 2, O )
                            , ( Position.create 3, X )
                            , ( Position.create 4, O )
                            , ( Position.create 5, X )
                            , ( Position.create 6, O )
                            , ( Position.create 7, X )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.map create
                            |> Maybe.andThen children
                            |> Expect.equal (Just [])
            ]
        , describe "createChildren"
            [ test "it creates a child for each available space in board" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, O )
                            , ( Position.create 1, X )
                            , ( Position.create 2, O )
                            , ( Position.create 4, O )
                            , ( Position.create 5, X )
                            , ( Position.create 6, O )
                            , ( Position.create 7, X )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.map ((createChildren X) << create)
                            |> Maybe.andThen children
                            |> Maybe.map List.length
                            |> Expect.equal (Just 2)
            ]
        , describe "updateRootScore"
            [ test "it takes a score and adds score to node" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, O )
                            , ( Position.create 1, X )
                            , ( Position.create 5, X )
                            , ( Position.create 6, O )
                            , ( Position.create 7, X )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.map create
                            |> Maybe.map (\x -> addToScore x 12)
                            |> Maybe.andThen score
                            |> Expect.equal (Just 12)
            ]
        , describe "stateMap"
            [ test "it takes a predicate function to apply to the board of node" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, O )
                            , ( Position.create 1, X )
                            , ( Position.create 2, O )
                            , ( Position.create 3, O )
                            , ( Position.create 4, O )
                            , ( Position.create 5, X )
                            , ( Position.create 6, O )
                            , ( Position.create 7, X )
                            ]
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.map ((createChildren X) << create)
                            |> Maybe.andThen (stateMap (Board.isAvailable (Position.create 8)))
                            |> Expect.equal (Just True)
            ]
        , describe "prune"
            [ test "it takes a (Tree -> Bool) predicate and returns tree with children removed where predicate is true" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, O )
                            , ( Position.create 1, X )
                            , ( Position.create 3, O )
                            , ( Position.create 4, O )
                            , ( Position.create 5, X )
                            , ( Position.create 6, O )
                            , ( Position.create 7, X )
                            ]

                        dummyPredicate tree =
                            List.isEmpty (Maybe.withDefault [] (children tree))
                    in
                        Board.create
                            |> Helper.updateSquares squares
                            |> Maybe.map ((prune dummyPredicate) << (createChildren X) << create)
                            |> Maybe.andThen children
                            |> Maybe.map List.length
                            |> Expect.equal (Just 0)
            ]
        ]
