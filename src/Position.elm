module Position
    exposing
        ( isValid
        , create
        , Position
        , map
        , value
        )

import Config


type Position
    = InvalidPosition
    | Position Int


value : Position -> Maybe Int
value pos =
    case pos of
        InvalidPosition ->
            Nothing

        Position n ->
            Just n


map : (Position -> a) -> Position -> Maybe a
map f pos =
    case pos of
        InvalidPosition ->
            Nothing

        n ->
            Just (f n)


isValid : Position -> Bool
isValid pos =
    case pos of
        InvalidPosition ->
            False

        Position _ ->
            True


create : Int -> Position
create n =
    if (n >= 0) && (n < Config.dimension ^ 2) then
        Position n
    else
        InvalidPosition
