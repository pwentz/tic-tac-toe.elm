module GameTree
    exposing
        ( GameTree
        , create
        , addToScore
        , stateMap
        , prune
        , createChildren
        , children
        , diffSquare
        , state
        , score
        , depth
        )

import Board exposing (Board)
import Config exposing (Symbol)


type alias MinimaxNode =
    { state : Board, score : Int, depth : Int, diffSquare : Maybe Int }


type alias NodeQuery a =
    GameTree -> Maybe a


type alias Node =
    { value : MinimaxNode, children : List GameTree }


type GameTree
    = EmptyTree
    | TreeNode Node


create : Board -> GameTree
create board =
    (TreeNode
        { value =
            { state = board
            , score = 0
            , depth = 0
            , diffSquare = Nothing
            }
        , children = []
        }
    )


map : (Node -> Node) -> GameTree -> GameTree
map f tree =
    case tree of
        EmptyTree ->
            EmptyTree

        TreeNode x ->
            TreeNode (f x)


addToScore : GameTree -> Int -> GameTree
addToScore tree newScore =
    let
        updateNodeScore ({ value, children } as nodeData) =
            { nodeData
                | value =
                    { value
                        | score = newScore + (.score value)
                    }
            }
    in
        map updateNodeScore tree


prune : (GameTree -> Bool) -> GameTree -> GameTree
prune f tree =
    let
        pruneChildren nodeData =
            { nodeData
                | children =
                    .children nodeData
                        |> List.filter (not << f)
            }
    in
        map pruneChildren tree


createChildren : Symbol -> GameTree -> GameTree
createChildren sym tree =
    let
        addChildren ({ value, children } as nodeData) =
            { nodeData
                | children =
                    (List.map
                        (\node ->
                            (TreeNode
                                { value =
                                    { state = .fork node
                                    , score = 0
                                    , depth = .depth value + 1
                                    , diffSquare = Just (.diffSquare node)
                                    }
                                , children = []
                                }
                            )
                        )
                        (Board.getForks sym (.state value))
                    )
            }
    in
        map addChildren tree


nodeMap : (Node -> a) -> GameTree -> Maybe a
nodeMap f tree =
    case tree of
        EmptyTree ->
            Nothing

        TreeNode node ->
            Just (f node)


stateMap : (Board -> a) -> GameTree -> Maybe a
stateMap f tree =
    nodeMap (f << .state << .value) tree


nodeValue : NodeQuery MinimaxNode
nodeValue tree =
    nodeMap .value tree


children : NodeQuery (List GameTree)
children tree =
    nodeMap .children tree


diffSquare : NodeQuery Int
diffSquare tree =
    nodeValue tree
        |> Maybe.andThen .diffSquare


state : NodeQuery Board
state tree =
    nodeMap (.state << .value) tree


score : NodeQuery Int
score tree =
    nodeMap (.score << .value) tree


depth : NodeQuery Int
depth tree =
    nodeMap (.depth << .value) tree
