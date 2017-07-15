module TestHelper exposing (..)

import Board exposing (Board)
import Position exposing (Position)
import Config exposing (Symbol)


updateSquares : List ( Position, Symbol ) -> Board -> Maybe Board
updateSquares squares b =
    let
        update ( p, s ) b =
            Maybe.andThen (Board.updateSquare p s) b
    in
        List.foldl update (Just b) squares
