module WinningPositions
    exposing
        ( winningColumnPositions
        , winningRowPositions
        , winningDownwardDiagPositions
        , winningUpwardDiagPositions
        )


winningColumnPositions : Int -> Int -> List Int
winningColumnPositions based n =
    List.range 0 (based - 1)
        |> List.map (\x -> (n % based) + (based * x))


winningRowPositions : Int -> Int -> List Int
winningRowPositions based n =
    List.range 0 (based - 1)
        |> List.map ((+) (rowHead based n))


winningDownwardDiagPositions : Int -> List Int
winningDownwardDiagPositions based =
    List.range 0 (based ^ 2 - 1)
        |> List.filter (\x -> x % (based + 1) == 0)


winningUpwardDiagPositions : Int -> List Int
winningUpwardDiagPositions based =
    List.range 1 (based ^ 2 - 2)
        |> List.filter (\x -> x % (based - 1) == 0)


rowHead : Int -> Int -> Int
rowHead based n =
    case n of
        0 ->
            0

        _ ->
            if n % based == 0 then
                n
            else
                rowHead based (n - 1)
