module BoardTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Board exposing (..)
import Config exposing (Symbol(X, O))
import Position
import Square
import TestHelper as Helper


suite : Test
suite =
    describe "Board"
        [ describe "center"
            [ test "it returns the center Position of the board" <|
                \_ ->
                    Expect.equal (Position.create 4) Board.center
            ]
        , describe "updateSquare"
            [ test "it returns Nothing if passed an invalid position" <|
                \_ ->
                    let
                        pos =
                            Position.create 12
                    in
                        Expect.equal Nothing (updateSquare pos X create)
            , test "It returns nothing if symbol at position is taken" <|
                \_ ->
                    let
                        pos =
                            Position.create 2
                    in
                        create
                            |> updateSquare pos X
                            |> Maybe.andThen (updateSquare pos X)
                            |> Expect.equal Nothing
            , test "it returns board with newly added position" <|
                \_ ->
                    let
                        pos =
                            Position.create 2

                        newBoard =
                            updateSquare pos X create
                    in
                        newBoard
                            |> Maybe.map getBoard
                            |> Maybe.andThen List.head
                            |> Maybe.map Square.position
                            |> Expect.equal (Just pos)
            , test "it returns board with newly added symbol" <|
                \_ ->
                    let
                        pos =
                            Position.create 2

                        newBoard =
                            updateSquare pos X create
                    in
                        newBoard
                            |> Maybe.map getBoard
                            |> Maybe.andThen List.head
                            |> Maybe.andThen Square.symbol
                            |> Expect.equal (Just X)
            ]
        , describe "isAvailable"
            [ test "it returns False if passed an invalid position" <|
                \_ ->
                    let
                        pos =
                            Position.create 12
                    in
                        Expect.equal False (isAvailable pos create)
            , test "it returns False if Position is taken" <|
                \_ ->
                    let
                        pos =
                            Position.create 2
                    in
                        create
                            |> updateSquare pos X
                            |> Maybe.map (isAvailable pos)
                            |> Expect.equal (Just False)
            , test "it returns True if Position is not taken" <|
                \_ ->
                    let
                        pos =
                            Position.create 2
                    in
                        Expect.equal True (isAvailable pos create)
            ]
        , describe "isFull"
            [ test "it returns True if all positions on board are occupied" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, X )
                            , ( Position.create 1, X )
                            , ( Position.create 2, X )
                            , ( Position.create 3, X )
                            , ( Position.create 4, X )
                            , ( Position.create 5, X )
                            , ( Position.create 6, X )
                            , ( Position.create 7, X )
                            , ( Position.create 8, X )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.map isFull
                            |> Expect.equal (Just True)
            , test "it returns False is ANY positions are empty" <|
                \_ ->
                    Expect.equal False (isFull create)
            ]
        , describe "isWinner"
            [ test "can tell if winner is left column" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, X )
                            , ( Position.create 3, X )
                            , ( Position.create 6, X )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.map (isWinner X)
                            |> Expect.equal (Just True)
            , test "can tell if winner is middle column" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 1, X )
                            , ( Position.create 4, X )
                            , ( Position.create 7, X )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.map (isWinner X)
                            |> Expect.equal (Just True)
            , test "can tell if winner is right column" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 2, X )
                            , ( Position.create 5, X )
                            , ( Position.create 8, X )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.map (isWinner X)
                            |> Expect.equal (Just True)
            , test "can tell if winner is top row" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, X )
                            , ( Position.create 1, X )
                            , ( Position.create 2, X )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.map (isWinner X)
                            |> Expect.equal (Just True)
            , test "can tell if winner is middle row" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 3, X )
                            , ( Position.create 4, X )
                            , ( Position.create 5, X )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.map (isWinner X)
                            |> Expect.equal (Just True)
            , test "can tell if winner is bottom row" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 6, X )
                            , ( Position.create 7, X )
                            , ( Position.create 8, X )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.map (isWinner X)
                            |> Expect.equal (Just True)
            , test "can tell if winner is upward diagonal" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 2, O )
                            , ( Position.create 4, O )
                            , ( Position.create 6, O )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.map (isWinner O)
                            |> Expect.equal (Just True)
            , test "can tell if winner is downward diagonal" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, O )
                            , ( Position.create 4, O )
                            , ( Position.create 8, O )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.map (isWinner O)
                            |> Expect.equal (Just True)
            , test "can tell if no winner" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 2, O )
                            , ( Position.create 3, O )
                            , ( Position.create 4, O )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.map (isWinner O)
                            |> Expect.equal (Just False)
            , test "can tell if no winner when passed EmptyBoard" <|
                \_ ->
                    isWinner O create
                        |> Expect.equal False
            ]
        , describe "getForks"
            [ test "it can create a list of forks when given a board and a symbol" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, O )
                            , ( Position.create 1, O )
                            , ( Position.create 2, O )
                            , ( Position.create 3, O )
                            , ( Position.create 4, O )
                            , ( Position.create 6, O )
                            , ( Position.create 8, O )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.map ((List.map .diffSquare) << getForks X)
                            |> Expect.equal (Just [ 5, 7 ])
            ]
        , describe "atPosition"
            [ test "it returns the symbol at the given position" <|
                \_ ->
                    let
                        pos =
                            Position.create 1

                        squares =
                            [ ( Position.create 0, O )
                            , ( pos, X )
                            , ( Position.create 8, O )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.andThen (atPosition pos)
                            |> Expect.equal (Just X)
            , test "it returns Nothing if the square at given position is empty" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, O )
                            , ( Position.create 1, X )
                            , ( Position.create 8, O )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.andThen (atPosition (Position.create 4))
                            |> Expect.equal Nothing
            , test "it returns Nothing if given Position is invalid" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, O )
                            , ( Position.create 1, X )
                            , ( Position.create 8, O )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.andThen (atPosition (Position.create 100))
                            |> Expect.equal Nothing
            ]
        , describe "availableSquares" <|
            [ test "it returns all squares that aren't occupied" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 4, O )
                            , ( Position.create 1, X )
                            , ( Position.create 3, O )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.map (List.length << availableSquares)
                            |> Expect.equal (Just 6)
            , test "it returns a list of EmptySquare's" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 4, O )
                            , ( Position.create 1, X )
                            , ( Position.create 3, O )
                            ]
                    in
                        create
                            |> Helper.updateSquares squares
                            |> Maybe.map ((List.map Square.isEmpty) << availableSquares)
                            |> Maybe.map (List.all ((==) True))
                            |> Expect.equal (Just True)
            ]
        ]
