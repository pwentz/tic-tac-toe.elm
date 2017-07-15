module Minimax exposing (tally)

import Board exposing (Board)
import Config exposing (Symbol)
import GameTree as Tree exposing (GameTree)


tally : Symbol -> Board -> List GameTree
tally sym board =
    let
        scoredForks =
            Tree.create board
                |> Tree.createChildren sym
                |> Tree.children
                |> Maybe.withDefault []
                |> List.map (scoreFork sym)

        isBestPositionUnclear =
            scoredForks
                |> List.filterMap Tree.score
                |> List.all ((==) 0)
    in
        if isBestPositionUnclear then
            scoredForks
                |> List.map (scoreChildren sym)
                |> List.sortWith compareScores
        else
            scoredForks
                |> List.sortWith compareScores


scoreChildren : Symbol -> GameTree -> GameTree
scoreChildren sym fork =
    let
        oppoSym =
            Config.opponentSymbol sym

        hasOpponentLetMeWin =
            (canOpponentWin oppoSym)
    in
        Tree.createChildren oppoSym fork
            |> Tree.prune hasOpponentLetMeWin
            |> Tree.children
            |> Maybe.withDefault []
            |> List.map (Tree.createChildren sym)
            |> List.filterMap Tree.children
            |> List.concat
            |> List.map (scoreFork sym)
            |> List.filterMap Tree.score
            |> List.sum
            |> Tree.addToScore fork


scoreFork : Symbol -> GameTree -> GameTree
scoreFork sym tree =
    let
        depth =
            Maybe.withDefault 0 (Tree.depth tree)

        hasWon =
            Tree.stateMap (Board.isWinner sym) tree
    in
        case hasWon of
            Nothing ->
                tree

            Just hasWon ->
                if hasWon then
                    Tree.addToScore tree (10 - depth)
                else if (canOpponentWin sym tree) then
                    Tree.addToScore tree (-10 + depth)
                else
                    tree


canOpponentWin : Symbol -> GameTree -> Bool
canOpponentWin sym tree =
    let
        oppoSym =
            Config.opponentSymbol sym
    in
        Tree.createChildren oppoSym tree
            |> Tree.children
            |> Maybe.withDefault []
            |> List.filterMap (Tree.stateMap (Board.isWinner oppoSym))
            |> List.any ((==) True)


compareScores : GameTree -> GameTree -> Order
compareScores tree1 tree2 =
    case ( Tree.score tree1, Tree.score tree2 ) of
        ( Nothing, Nothing ) ->
            EQ

        ( Just x, Nothing ) ->
            GT

        ( Nothing, Just y ) ->
            LT

        ( Just x, Just y ) ->
            compare y x
