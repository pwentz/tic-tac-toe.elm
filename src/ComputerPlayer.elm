module ComputerPlayer exposing (makeMove)

import Minimax
import Board exposing (Board)
import GameTree as Tree
import Position exposing (Position)
import Config exposing (Symbol)


makeMove : Symbol -> Board -> Maybe Position
makeMove sym board =
    let
        isFirstTurn =
            board
                |> Board.availableSquares
                |> List.length
                |> ((<=) 1)
    in
        if (isFirstTurn && Board.isAvailable Board.center board) then
            Just Board.center
        else
            board
                |> Minimax.tally sym
                |> List.filterMap Tree.diffSquare
                |> List.head
                |> Maybe.map Position.create
