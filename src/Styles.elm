module Styles exposing (..)

import Css exposing (..)
import Html exposing (Attribute)
import Html.Attributes


styles cssPairs =
    asPairs cssPairs
        |> Html.Attributes.style


symbolStyles : String -> Attribute msg
symbolStyles sym =
    if sym == "X" then
        styles
            [ color (hex "#DD7373") ]
    else
        styles
            [ color (hex "#51A3A3") ]


tableStyle : Attribute msg
tableStyle =
    styles
        [ margin auto ]


squareStyle : Attribute msg
squareStyle =
    styles
        [ border (px 2)
        , borderColor (hex "#D1D1D1")
        , borderStyle solid
        , height (px 100)
        , width (px 100)
        , fontSize (px 45)
        ]


textStyle : Attribute msg
textStyle =
    styles
        [ color (hex "#EAD94C")
        , textAlign center
        ]


inputStyle : Attribute msg
inputStyle =
    styles
        [ marginTop (px 20)
        , height (px 54)
        , width (px 100)
        , textAlign center
        , fontSize (px 28)
        , backgroundColor (hex "#3B3561")
        ]


messageStyle : Attribute msg
messageStyle =
    styles
        [ fontSize (px 32)
        , marginTop (px 54)
        ]


fillerStyle : Attribute msg
fillerStyle =
    styles
        [ height (px 80) ]


titleStyle : Attribute msg
titleStyle =
    styles
        [ marginTop (px 30)
        , fontSize (px 45)
        ]
