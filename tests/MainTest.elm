module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import TestHelper as Helper
import Main exposing (..)
import Board exposing (Board)
import Position
import Config exposing (Symbol(X, O))


suite : Test
suite =
    describe "Main"
        [ describe "update/PlayUserTurn"
            [ test "it resets input, sets game over msg, and returns model when game is already over" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, X )
                            , ( Position.create 4, X )
                            , ( Position.create 8, X )
                            , ( Position.create 1, O )
                            ]

                        board =
                            Board.create
                                |> Helper.updateSquares squares
                                |> Maybe.withDefault Board.create

                        model =
                            { board = board
                            , input = "7"
                            , message = "Something"
                            }

                        msg =
                            PlayUserTurn

                        expected =
                            { input = ""
                            , board = board
                            , message = "Refresh the page to play again."
                            }
                    in
                        update msg model
                            |> Expect.equal expected
            , test "it resets input, sets bad input msg, and returns model when input is not a number" <|
                \_ ->
                    let
                        model =
                            { board = Board.create
                            , input = "H"
                            , message = "Something"
                            }

                        msg =
                            PlayUserTurn

                        expected =
                            { input = ""
                            , board = Board.create
                            , message = "Please enter a number: [0-8]"
                            }
                    in
                        update msg model
                            |> Expect.equal expected
            , test "it reset input, sets invalid square message, and returns model when input is not an eligible square" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, X )
                            , ( Position.create 4, X )
                            , ( Position.create 1, O )
                            ]

                        board =
                            Board.create
                                |> Helper.updateSquares squares
                                |> Maybe.withDefault Board.create

                        model =
                            { board = board
                            , input = "4"
                            , message = "Something"
                            }

                        msg =
                            PlayUserTurn

                        expected =
                            { input = ""
                            , board = board
                            , message = "That square is unavailable!"
                            }
                    in
                        update msg model
                            |> Expect.equal expected
            , test "it resets input, sets 'you win' msg, and returns model when user wins" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, Config.userSym )
                            , ( Position.create 4, Config.userSym )
                            , ( Position.create 1, Config.cpuSym )
                            , ( Position.create 2, Config.cpuSym )
                            ]

                        board =
                            Board.create
                                |> Helper.updateSquares squares
                                |> Maybe.withDefault Board.create

                        model =
                            { board = board
                            , input = "8"
                            , message = "Something"
                            }

                        msg =
                            PlayUserTurn

                        expected =
                            { input = ""
                            , message = "You win!"
                            , board =
                                board
                                    |> Helper.updateSquares [ ( Position.create 8, Config.userSym ) ]
                                    |> Maybe.withDefault board
                            }
                    in
                        update msg model
                            |> Expect.equal expected
            , test "it reset input, sets tie msg, and returns model when user ties" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, Config.cpuSym )
                            , ( Position.create 1, Config.userSym )
                            , ( Position.create 2, Config.cpuSym )
                            , ( Position.create 3, Config.cpuSym )
                            , ( Position.create 4, Config.userSym )
                            , ( Position.create 5, Config.userSym )
                            , ( Position.create 6, Config.userSym )
                            , ( Position.create 7, Config.cpuSym )
                            ]

                        board =
                            Board.create
                                |> Helper.updateSquares squares
                                |> Maybe.withDefault Board.create

                        model =
                            { board = board
                            , input = "8"
                            , message = "Something"
                            }

                        msg =
                            PlayUserTurn

                        expected =
                            { input = ""
                            , message = "It's a tie!"
                            , board =
                                board
                                    |> Helper.updateSquares [ ( Position.create 8, Config.userSym ) ]
                                    |> Maybe.withDefault board
                            }
                    in
                        update msg model
                            |> Expect.equal expected
            , test "it resets input, sets the losing msg, and returns model when user plays and cpu then wins" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 0, Config.cpuSym )
                            , ( Position.create 1, Config.userSym )
                            , ( Position.create 3, Config.userSym )
                            , ( Position.create 4, Config.cpuSym )
                            ]

                        board =
                            Board.create
                                |> Helper.updateSquares squares
                                |> Maybe.withDefault Board.create

                        model =
                            { board = board
                            , input = "6"
                            , message = "Something"
                            }

                        msg =
                            PlayUserTurn

                        expected =
                            { input = ""
                            , message = "You lose!"
                            , board =
                                board
                                    |> Helper.updateSquares
                                        [ ( Position.create 6, Config.userSym )
                                        , ( Position.create 8, Config.cpuSym )
                                        ]
                                    |> Maybe.withDefault board
                            }
                    in
                        update msg model
                            |> Expect.equal expected
            , test "it reset input, resets message, and updates board otherwise" <|
                \_ ->
                    let
                        squares =
                            [ ( Position.create 4, Config.cpuSym )
                            , ( Position.create 0, Config.userSym )
                            ]

                        board =
                            Board.create
                                |> Helper.updateSquares squares
                                |> Maybe.withDefault Board.create

                        model =
                            { board = board
                            , input = "3"
                            , message = "Something"
                            }

                        msg =
                            PlayUserTurn

                        expected =
                            { input = ""
                            , message = ""
                            , board =
                                board
                                    |> Helper.updateSquares
                                        [ ( Position.create 3, Config.userSym )
                                        , ( Position.create 6, Config.cpuSym )
                                        ]
                                    |> Maybe.withDefault board
                            }
                    in
                        update msg model
                            |> Expect.equal expected
            ]
        ]
