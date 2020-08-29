module Parameters exposing (Parameters, init, validate, getSize, getColumn, getRow)


type alias Parameters =
    {
        xSize : Int
        , ySize : Int
        , mines : Int
    }


minSize : Int
minSize = 3

maxSize : Int
maxSize = 300

maxMines : Int -> Int -> Int
maxMines x y =
    x * y // 2


init : Parameters
init =
    {
        xSize = 20
        , ySize = 20
        , mines = 50
    }


validateSize : Int -> Maybe String
validateSize size =
    if minSize <= size && size <= maxSize then
        Nothing
    else
        "Choose a size between " ++ ( String.fromInt minSize ) ++ " and " ++ ( String.fromInt maxSize ) |> Just


validateMines : Int -> Int -> Int -> Maybe String
validateMines x y m =
    let
        minimum = 0
        maximum = maxMines x y
    in
    if minimum <= m && m <= maximum then
        Nothing
    else
        "Choose mines between " ++ ( String.fromInt minimum ) ++ " and " ++ ( String.fromInt maximum ) |> Just


validate : Parameters -> Maybe String
validate { xSize, ySize, mines } =
    validateSize xSize  
    |> maybeChainer (validateSize ySize)
    |> maybeChainer (validateMines xSize ySize mines)


-- helper to accomodate the validate... is in fact a reverse Maybe.andThen
maybeChainer : Maybe a -> Maybe a -> Maybe a
maybeChainer ma1 ma2 =
    case ma2 of
        Nothing ->
            ma1

        Just a1 ->
            Just a1
        

getSize : Parameters -> Int
getSize { xSize, ySize } =
    xSize * ySize


getRow : Parameters -> Int -> Int
getRow { xSize } index =
        index // xSize


getColumn : Parameters -> Int -> Int
getColumn { xSize } index =
        modBy xSize index

