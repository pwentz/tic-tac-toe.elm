module Config exposing (..)


type Symbol
    = X
    | O


userSym : Symbol
userSym =
    O


cpuSym : Symbol
cpuSym =
    X


dimension : Int
dimension =
    3


opponentSymbol : Symbol -> Symbol
opponentSymbol sym =
    case sym of
        X ->
            O

        O ->
            X
