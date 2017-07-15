module Square
    exposing
        ( Square
        , createEmpty
        , create
        , isEmpty
        , map
        , position
        , symbol
        )

import Position exposing (Position)
import Config exposing (Symbol)


type Square
    = EmptySquare { position : Position }
    | Square { position : Position, symbol : Symbol }


symbol : Square -> Maybe Symbol
symbol square =
    case square of
        EmptySquare { position } ->
            Nothing

        Square { position, symbol } ->
            Just symbol


position : Square -> Position
position square =
    case square of
        EmptySquare { position } ->
            position

        Square { position, symbol } ->
            position


createEmpty : Position -> Maybe Square
createEmpty pos =
    Position.map (\x -> EmptySquare { position = x }) pos


create : Position -> Symbol -> Maybe Square
create pos sym =
    Position.map (\x -> Square { position = x, symbol = sym }) pos


isEmpty : Square -> Bool
isEmpty sq =
    case sq of
        EmptySquare _ ->
            True

        Square _ ->
            False


map : (Square -> a) -> Square -> Maybe a
map f square =
    case square of
        EmptySquare _ ->
            Nothing

        x ->
            Just (f x)
