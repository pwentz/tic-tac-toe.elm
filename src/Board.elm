module Board
    exposing
        ( isFull
        , updateSquare
        , Board
        , isAvailable
        , create
        , getBoard
        , isWinner
        , getForks
        , atPosition
        , center
        , availableSquares
        )

import WinningPositions as Winners
import Position exposing (Position)
import Square exposing (Square)
import Config exposing (Symbol)


type Board
    = Board (List Square)


create : Board
create =
    List.range 0 ((Config.dimension ^ 2) - 1)
        |> List.filterMap (Square.createEmpty << Position.create)
        |> Board


center : Position
center =
    floor ((toFloat Config.dimension ^ 2) / 2)
        |> Position.create


updateSquare : Position -> Symbol -> Board -> Maybe Board
updateSquare pos sym board =
    let
        restOfPositions =
            getBoard board
                |> List.filter (((/=) pos) << Square.position)
    in
        if (isAvailable pos board) then
            Square.create pos sym
                |> Maybe.map (\x -> Board (x :: restOfPositions))
        else
            Nothing


atPosition : Position -> Board -> Maybe Symbol
atPosition pos board =
    getBoard board
        |> List.filter (((==) pos) << Square.position)
        |> List.head
        |> Maybe.andThen (Square.symbol)


isAvailable : Position -> Board -> Bool
isAvailable pos board =
    case ( Position.isValid pos, atPosition pos board ) of
        ( False, _ ) ->
            False

        ( _, Nothing ) ->
            True

        ( _, _ ) ->
            False


isFull : Board -> Bool
isFull board =
    availableSquares board
        |> List.isEmpty


getForks : Symbol -> Board -> List { fork : Board, diffSquare : Int }
getForks sym board =
    let
        positionWithFork sq newBoard =
            Square.position sq
                |> Position.value
                |> Maybe.map
                    (\n ->
                        { fork = newBoard
                        , diffSquare = n
                        }
                    )

        canUpdateBoard sq =
            updateSquare (Square.position sq) sym board
                |> Maybe.andThen (positionWithFork sq)
    in
        List.filterMap canUpdateBoard (availableSquares board)


availableSquares : Board -> List Square
availableSquares board =
    getBoard board
        |> List.filter (\x -> isAvailable (Square.position x) board)


getBoard : Board -> List Square
getBoard (Board squares) =
    squares


isWinner : Symbol -> Board -> Bool
isWinner sym board =
    [ isWinnerRowOrCol sym board (Winners.winningColumnPositions Config.dimension)
    , isWinnerRowOrCol sym board (Winners.winningRowPositions Config.dimension)
    , isWinnerDiag sym board (Winners.winningDownwardDiagPositions Config.dimension)
    , isWinnerDiag sym board (Winners.winningUpwardDiagPositions Config.dimension)
    ]
        |> List.any ((==) True)


isWinnerRowOrCol : Symbol -> Board -> (Int -> List Int) -> Bool
isWinnerRowOrCol sym board f =
    let
        determinant =
            (\xs ->
                List.any
                    (\x ->
                        List.all
                            (\y ->
                                (List.member y xs)
                            )
                            (f x)
                    )
                    xs
            )
    in
        mapSymbolPositions determinant sym board


isWinnerDiag : Symbol -> Board -> List Int -> Bool
isWinnerDiag sym board winningPositions =
    let
        determinant =
            (\xs ->
                (List.all
                    (\y ->
                        (List.member y xs)
                    )
                    winningPositions
                )
            )
    in
        mapSymbolPositions determinant sym board


mapSymbolPositions : (List Int -> Bool) -> Symbol -> Board -> Bool
mapSymbolPositions f sym board =
    getBoard board
        |> List.filter (((==) (Just sym)) << Square.symbol)
        |> List.map Square.position
        |> List.filterMap Position.value
        |> f
