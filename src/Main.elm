module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Board exposing (Board)
import Config exposing (Symbol(X, O))
import ComputerPlayer as Cpu
import Position
import Styles
import GameCopy


main =
    beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { board : Board
    , input : String
    , message : String
    }


initialModel : Model
initialModel =
    { board = Board.create
    , input = ""
    , message = GameCopy.onStart
    }



-- VIEW


symbolAt : Int -> Board -> String
symbolAt n =
    (fromSymbol << (Board.atPosition (Position.create n)))


fromSymbol : Maybe Symbol -> String
fromSymbol sym =
    case sym of
        Nothing ->
            ""

        Just x ->
            toString x


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        didPressEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not enter"
    in
        on "keydown" (Json.andThen didPressEnter keyCode)


view : Model -> Html Msg
view model =
    div
        [ Styles.textStyle ]
        [ h1
            [ Styles.titleStyle ]
            [ text "Tic Tac Toe" ]
        , div
            [ Styles.fillerStyle ]
            []
        , table [ Styles.tableStyle ]
            [ tr []
                [ td
                    [ Styles.squareStyle
                    , Styles.symbolStyles (symbolAt 0 (.board model))
                    ]
                    [ text (symbolAt 0 (.board model)) ]
                , td
                    [ Styles.squareStyle
                    , Styles.symbolStyles (symbolAt 1 (.board model))
                    ]
                    [ text (symbolAt 1 (.board model)) ]
                , td
                    [ Styles.squareStyle
                    , Styles.symbolStyles (symbolAt 2 (.board model))
                    ]
                    [ text (symbolAt 2 (.board model)) ]
                ]
            , tr []
                [ td
                    [ Styles.squareStyle
                    , Styles.symbolStyles (symbolAt 3 (.board model))
                    ]
                    [ text (symbolAt 3 (.board model)) ]
                , td
                    [ Styles.squareStyle
                    , Styles.symbolStyles (symbolAt 4 (.board model))
                    ]
                    [ text (symbolAt 4 (.board model)) ]
                , td
                    [ Styles.squareStyle
                    , Styles.symbolStyles (symbolAt 5 (.board model))
                    ]
                    [ text (symbolAt 5 (.board model)) ]
                ]
            , tr []
                [ td
                    [ Styles.squareStyle
                    , Styles.symbolStyles (symbolAt 6 (.board model))
                    ]
                    [ text (symbolAt 6 (.board model)) ]
                , td
                    [ Styles.squareStyle
                    , Styles.symbolStyles (symbolAt 7 (.board model))
                    ]
                    [ text (symbolAt 7 (.board model)) ]
                , td
                    [ Styles.squareStyle
                    , Styles.symbolStyles (symbolAt 8 (.board model))
                    ]
                    [ text (symbolAt 8 (.board model)) ]
                ]
            ]
        , input
            [ type_ "text"
            , onInput UpdateField
            , onEnter PlayUserTurn
            , value (.input model)
            , maxlength 1
            , Styles.inputStyle
            , Styles.textStyle
            , autofocus True
            ]
            []
        , h5
            [ Styles.messageStyle ]
            [ text (.message model) ]
        ]



-- UPDATE


type Msg
    = UpdateField String
    | PlayUserTurn


isGameOver : Board -> Bool
isGameOver board =
    (Board.isWinner Config.userSym board)
        || (Board.isWinner Config.cpuSym board)
        || (Board.isFull board)


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateField userInput ->
            { model | input = userInput }

        PlayUserTurn ->
            if (isGameOver (.board model)) then
                { model
                    | message = GameCopy.onEnd
                    , input = ""
                }
            else
                case String.toInt (.input model) of
                    Err _ ->
                        { model
                            | message = GameCopy.onBadInput
                            , input = ""
                        }

                    Ok numInput ->
                        case Board.updateSquare (Position.create numInput) Config.userSym (.board model) of
                            Nothing ->
                                { model
                                    | message = GameCopy.onInvalidSquare
                                    , input = ""
                                }

                            Just board ->
                                finishTurn { model | board = board }


finishTurn : Model -> Model
finishTurn model =
    if Board.isWinner Config.userSym (.board model) then
        { model
            | message = GameCopy.onUserWin
            , input = ""
        }
    else
        let
            updateWithCpuMove pos =
                Board.updateSquare pos Config.cpuSym (.board model)

            newBoard =
                Cpu.makeMove Config.cpuSym (.board model)
                    |> Maybe.andThen updateWithCpuMove
        in
            case newBoard of
                Nothing ->
                    { model
                        | message = GameCopy.onTie
                        , input = ""
                    }

                Just board ->
                    if Board.isWinner Config.cpuSym board then
                        { board = board
                        , message = GameCopy.onUserLose
                        , input = ""
                        }
                    else
                        { board = board
                        , message = ""
                        , input = ""
                        }
