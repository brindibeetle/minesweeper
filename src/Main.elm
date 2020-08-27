module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, onDoubleClick)
import Html.Attributes exposing (..)
import Url exposing (Url)
import Array as Array
import Random as Random
import Task as Task

-- minimum = 3
xSize : Int
xSize = 8


-- minimum = 3
ySize : Int
ySize = 8


numberOfMines : Int
numberOfMines = 10


-- MODEL

type alias Model = Array.Array Field


type alias Field = 
    {
      status : FieldStatus
      , content : FieldContent
      , adjacentMines : Int
    }


type FieldStatus =
    Close
    | Open
    | Flag


type FieldContent =
    Empty
    | Mine



main : Program String Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- INIT

init : String -> ( Model, Cmd Msg )
init _ =  
    let
        fields = Array.initialize (xSize * ySize) (\index -> getField index) -- |> setMines numberOfMines |> setAdjacentMines
    in
        ( fields, List.repeat numberOfMines ( Random.generate SetRandomMine minesGenerator ) |> Cmd.batch )


getField : Int -> Field
getField index =
    { status = Close, content = Empty, adjacentMines = 0 }
    

minesGenerator : Random.Generator Int
minesGenerator =
    Random.int 0 (xSize * ySize - 1)


getRandom : Int -> Int
getRandom number =
    number * 2


setMine : Field -> Field 
setMine field =
    { field
    | content = Mine
    }


setAdjacentMines : Model -> Model
setAdjacentMines model =
    Array.indexedMap ( \index field -> ( index, field ) ) model
    |> Array.filter ( \(index, field) -> field.content == Mine )
    |> Array.map ( \(index, _ ) -> index )
    |> Array.foldl ( \index -> doAdjacent index addAdjacentMines ) model


addAdjacentMines : Int -> Model -> Model
addAdjacentMines index model =
    case Array.get index model of
        Nothing ->
            model

        Just field ->
            case field.content of
                Empty ->
                    Array.set 
                        index 
                        { field
                        | adjacentMines = field.adjacentMines + 1
                        }
                        model

                _ ->
                    model

-- UPDATE


type Msg
  = Opened Int
  | Flagged Int
  | PickRandomIndex
  | SetRandomMine Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Opened index ->
        case Array.get index model of
            Nothing ->
                ( model, Cmd.none )

            Just field ->
                case field.status of
                    Close ->
                        ( doOpen index model, Cmd.none )

                    Open ->
                        ( doClose index model, Cmd.none )
                    
                    _ ->
                        ( model, Cmd.none )

    Flagged index ->
        case Array.get index model of
            Nothing ->
                ( model, Cmd.none )

            Just field ->
                case field.status of
                    Close ->
                        ( doFlag index model, Cmd.none )

                    Flag ->
                        ( doClose index model, Cmd.none )
                    
                    _ ->
                        ( model, Cmd.none )

    PickRandomIndex ->
        ( model , Random.generate SetRandomMine minesGenerator )

    SetRandomMine index ->
        case Array.get index model of
            Nothing ->
                ( model, Cmd.none )

            Just field ->
                case field.content of
                    Mine ->
                        ( model, Cmd.none )

                    Empty ->
                        ( Array.set
                            index
                            { field
                            | content = Mine 
                            }
                            model
                        |> doAdjacent index addAdjacentMines
                        , Cmd.none )


doFlag : Int -> Model -> Model
doFlag index model =
    case Array.get index model of
        Nothing ->
            model

        Just field ->
            case field.status of
                Close ->
                    doFlagField index model

                _ ->
                    model


doClose : Int -> Model -> Model
doClose index model =
    case Array.get index model of
        Nothing ->
            model

        Just field ->
            case field.status of
                Open ->
                    doCloseField index model
                
                Flag ->
                    doCloseField index model
                
                _ ->
                    model


doOpen : Int -> Model -> Model
doOpen index model =
    case Array.get index model of
        Nothing ->
            model

        Just field ->
            case ( field.status, field.content, field.adjacentMines ) of
                ( Close, Empty, 0 ) ->
                    doOpenField index model
                    |> doAdjacent index doOpen

                ( Close, _, _ ) ->
                    doOpenField index model

                ( _, _, _ ) ->
                    model




-- VIEW


view : Model -> Document Msg
view model =
  {
    title = "Minesweeper"
    , body = 
        [
            div 
                [ style "display" "grid"
                , style "grid-template-columns" ( "repeat(" ++ ( String.fromInt xSize ) ++ ", 30px )" ) 
                , style "grid-template-rows" ( "repeat(" ++ ( String.fromInt ySize ) ++ ", 30px )" ) 
                ]
            ( Array.toList model
            |> List.indexedMap viewField
            )
        ]

      -- [ button [ onClick Decrement ] [ text "-" ]
      -- , div [] [ text (String.fromInt model) ]
      -- , button [ onClick Increment ] [ text "+" ]
      -- ]
  }


viewField : Int -> Field -> Html Msg
viewField index { status, content, adjacentMines } =
    case ( status, content ) of
        ( Close, _ ) ->
            button [ onClick ( Flagged index ), onDoubleClick ( Opened index ) ] [ text "[]" ]

        ( Open, Empty ) ->
            if adjacentMines == 0 then
                button [ onClick ( Opened index ) ] [ text "." ]
            else
                button [ onClick ( Opened index ) ] [ String.fromInt adjacentMines |> text ]

        ( Open, Mine ) ->
            button [ onClick ( Opened index ) ] [ text "M" ]

        ( Flag, _ ) ->
            button [ onClick ( Flagged index ) ] [ text "F" ]


-- UTILITY

getRow : Int -> Int
getRow index =
        index // xSize


getColumn : Int -> Int
getColumn index =
        modBy xSize index


doAdjacent : Int -> ( Int -> Model -> Model ) -> Model -> Model
doAdjacent index modelFunction model =
    getAdjacent index
    |> Array.foldl modelFunction model


doOpenField : Int -> Model -> Model
doOpenField index model =
    case Array.get index model of
        Nothing ->
            model

        Just field ->
            Array.set index { field | status = Open } model


doCloseField : Int -> Model -> Model
doCloseField index model =
    case Array.get index model of
        Nothing ->
            model

        Just field ->
            Array.set index { field | status = Close } model


doFlagField : Int -> Model -> Model
doFlagField index model =
    case Array.get index model of
        Nothing ->
            model

        Just field ->
            Array.set index { field | status = Flag } model


getAdjacent : Int -> Array.Array Int
getAdjacent index =
    Array.empty
    |> ( if getRow index > 0 && getColumn index > 0                   then Array.push ( index - xSize - 1 ) else identity )
    |> ( if getRow index > 0                                          then Array.push ( index - xSize ) else identity )
    |> ( if getRow index > 0 && getColumn index + 1 < xSize           then Array.push ( index - xSize + 1 ) else identity )

    |> ( if getColumn index > 0                                       then Array.push ( index - 1 ) else identity )
    |> ( if getColumn index + 1 < xSize                               then Array.push ( index + 1 ) else identity )

    |> ( if getRow index + 1 < ySize && getColumn index > 0           then Array.push ( index + xSize - 1 ) else identity )
    |> ( if getRow index + 1 < ySize                                  then Array.push ( index + xSize ) else identity )
    |> ( if getRow index + 1 < ySize && getColumn index + 1 < xSize   then Array.push ( index + xSize + 1 ) else identity )

     
filterAdjacent : Model -> Int -> (Field -> Bool) -> Array.Array Int
filterAdjacent model index filterFunction =
    getAdjacent index
    |> Array.map ( \index1 -> (index1, Array.get index1 model) )
    |> Array.filter ( \(index1, mField) -> 
        case mField of
            Nothing -> False
            Just field -> filterFunction field
        )
    |> Array.map ( \(index1, _) -> index1 )


