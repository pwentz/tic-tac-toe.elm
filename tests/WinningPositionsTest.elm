module WinningPositionsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import WinningPositions exposing (..)


suite : Test
suite =
    describe "WinningPositions"
        [ describe "winningColumnPositions"
            [ test "it takes a position and returns all positions needed to win that column" <|
                \_ ->
                    Expect.equal [ 0, 3, 6 ] (winningColumnPositions 3 6)
            , test "it can work for any value in any column" <|
                \_ ->
                    Expect.equal [ 1, 4, 7 ] (winningColumnPositions 3 1)
            , test "it can be applied to 4x4 boards" <|
                \_ ->
                    Expect.equal [ 3, 7, 11, 15 ] (winningColumnPositions 4 11)
            , test "it can be applied to 5x5 boards" <|
                \_ ->
                    Expect.equal [ 2, 7, 12, 17, 22 ] (winningColumnPositions 5 17)
            ]
        , describe "winningRowPositions"
            [ test "it takes a position and returns all positions needed to win that row" <|
                \_ ->
                    Expect.equal [ 3, 4, 5 ] (winningRowPositions 3 3)
            , test "it can work for any value in row" <|
                \_ ->
                    Expect.equal [ 0, 1, 2 ] (winningRowPositions 3 2)
            , test "it can be applied to 4x4 boards" <|
                \_ ->
                    Expect.equal [ 8, 9, 10, 11 ] (winningRowPositions 4 10)
            , test "it can be applied to 5x5 boards" <|
                \_ ->
                    Expect.equal [ 5, 6, 7, 8, 9 ] (winningRowPositions 5 7)
            ]
        , describe "winningDownwardDiagPositions"
            [ test "it takes a row/column size and returns all positions needed to win downward diagonally" <|
                \_ ->
                    Expect.equal [ 0, 4, 8 ] (winningDownwardDiagPositions 3)
            , test "it can be applied to 4x4 boards" <|
                \_ ->
                    Expect.equal [ 0, 5, 10, 15 ] (winningDownwardDiagPositions 4)
            , test "it can be applied to 5x5 boards" <|
                \_ ->
                    Expect.equal [ 0, 6, 12, 18, 24 ] (winningDownwardDiagPositions 5)
            ]
        , describe "winningUpwardDiagPositions"
            [ test "it takes a row/column size and returns positions needed to win upwards diagonally" <|
                \_ ->
                    Expect.equal [ 2, 4, 6 ] (winningUpwardDiagPositions 3)
            , test "it can be applied to 4x4 boards" <|
                \_ ->
                    Expect.equal [ 3, 6, 9, 12 ] (winningUpwardDiagPositions 4)
            , test "it can be applied to 5x5 boards" <|
                \_ ->
                    Expect.equal [ 4, 8, 12, 16, 20 ] (winningUpwardDiagPositions 5)
            ]
        ]
