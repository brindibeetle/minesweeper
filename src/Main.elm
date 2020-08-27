module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, button, div, text)
-- import Html.Events exposing (onClick, onDoubleClick)
import Html.Attributes exposing (..)
import Url exposing (Url)
import Array as Array
import Random as Random
import Html.Events.Extra.Mouse as ExtraMouse

-- minimum = 3
xSize : Int
xSize = 8


-- minimum = 3
ySize : Int
ySize = 8


numberOfMines : Int
numberOfMines = 10


-- MODEL

type alias Model = 
    {
        fields : Array.Array Field
    }


type alias Field = 
    {
        status : FieldStatus
        , content : FieldContent
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
subscriptions model =
    Sub.none

-- INIT

init : String -> ( Model, Cmd Msg )
init _ =  
    let
        fields = Array.initialize (xSize * ySize) (\index -> getField index)
    in
        ( 
            { fields = fields
            }
            , List.repeat numberOfMines ( Random.generate SetRandomMine minesGenerator )
              |> Cmd.batch 
        )


getField : Int -> Field
getField index =
    { status = Close, content = Empty }
    

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


-- UPDATE


type Msg
  = OpenField Int ExtraMouse.Event
  | CloseField Int ExtraMouse.Event
  | FlagField Int ExtraMouse.Event
  | UnFlagField Int ExtraMouse.Event
  | OpenFieldAndAdjacents Int ExtraMouse.Event
  | OpenAdjacents Int ExtraMouse.Event
  | PickRandomIndex
  | SetRandomMine Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { fields } = model
    in
        case msg of
            OpenField index _ ->
                case Array.get index fields of
                    Nothing ->
                        ( model, Cmd.none )

                    Just field ->
                        ( doOpenField index fields |> setFields model , Cmd.none )

            CloseField index _ ->
                case Array.get index fields of
                    Nothing ->
                        ( model, Cmd.none )

                    Just field ->
                        ( doCloseField index fields |> setFields model, Cmd.none )

            FlagField index _ ->
                case Array.get index fields of
                    Nothing ->
                        ( model, Cmd.none )

                    Just field ->
                        ( doFlagField index fields |> setFields model, Cmd.none )

            UnFlagField index _ ->
                case Array.get index fields of
                    Nothing ->
                        ( model, Cmd.none )

                    Just field ->
                        ( doUnFlagField index fields |> setFields model, Cmd.none )

            OpenFieldAndAdjacents index _ ->
                case Array.get index fields of
                    Nothing ->
                        ( model, Cmd.none )

                    Just field ->
                        ( doOpenFieldAndAdjacents index fields |> setFields model, Cmd.none )

            OpenAdjacents index _ ->
                case Array.get index fields of
                    Nothing ->
                        ( model, Cmd.none )

                    Just field ->
                        ( doOpenAdjacents index fields |> setFields model, Cmd.none )
                                

            PickRandomIndex ->
                ( model , Random.generate SetRandomMine minesGenerator )

            SetRandomMine index ->
                case Array.get index fields of
                    Nothing ->
                        ( model, Cmd.none )

                    Just field ->
                        case field.content of
                            Mine ->
                                ( model, Cmd.none )

                            Empty ->
                                (
                                    { model
                                    | fields =
                                        Array.set
                                            index
                                            { field
                                            | content = Mine 
                                            }
                                            model.fields
                                    }
                                    , Cmd.none
                                )


setFields : Model -> Array.Array Field -> Model
setFields model fields =
    { model
    | fields = fields
    }


doOpenField : Int -> Array.Array Field -> Array.Array Field
doOpenField index fields =
    case Array.get index fields of
        Nothing ->
            fields

        Just field ->
            Array.set index { field | status = Open } fields


doCloseField : Int -> Array.Array Field -> Array.Array Field
doCloseField index fields =
    case Array.get index fields of
        Nothing ->
            fields

        Just field ->
            Array.set index { field | status = Close } fields


doFlagField : Int -> Array.Array Field -> Array.Array Field
doFlagField index fields =
    case Array.get index fields of
        Nothing ->
            fields

        Just field ->
            Array.set index { field | status = Flag } fields


doUnFlagField : Int -> Array.Array Field -> Array.Array Field
doUnFlagField index fields =
    case Array.get index fields of
        Nothing ->
            fields

        Just field ->
            Array.set index { field | status = Close } fields


doOpenFieldAndAdjacents : Int -> Array.Array Field -> Array.Array Field
doOpenFieldAndAdjacents index fields =
    case Array.get index fields of
        Nothing ->
            fields

        Just field ->
            case ( field.status, field.content ) of
                ( Close, Empty ) ->
                    let
                        adjacentMines = filterCountAdjacent index ( \field1 -> field1.content == Mine ) fields
                    in
                        if adjacentMines == 0 then
                            doOpenField index fields
                            |> doAdjacent index doOpenFieldAndAdjacents
                        else
                            doOpenField index fields
                    
                ( Close, _ ) ->
                    doOpenField index fields

                ( _, _ ) ->
                    fields


doOpenAdjacents : Int -> Array.Array Field -> Array.Array Field
doOpenAdjacents index fields =
    getAdjacent index
    |> Array.toList
    |> List.filterMap ( getIndexField fields )
    |> Array.fromList
    |> Array.filter (filterField fieldIsClosed) 
    |> Array.map getIndex
    |> Array.foldl doOpenField fields

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
            ( Array.toList model.fields
            |> List.indexedMap (viewField model)
            )
        ]

      -- [ button [ onClick Decrement ] [ text "-" ]
      -- , div [] [ text (String.fromInt model) ]
      -- , button [ onClick Increment ] [ text "+" ]
      -- ]
  }


viewField : Model -> Int -> Field -> Html Msg
viewField { fields } index { status, content } =
    let
        adjacentMines = filterCountAdjacent index ( \field -> field.content == Mine ) fields
        adjacentFlags = filterCountAdjacent index ( \field -> field.status == Flag ) fields
    in
        case ( status, content ) of
            ( Close, Empty ) ->
                if adjacentMines == 0 then
                    button [ ExtraMouse.onClick ( OpenFieldAndAdjacents index ), ExtraMouse.onContextMenu ( FlagField index ) ] [ text "[]" ]
                else 
                    button [ ExtraMouse.onClick ( OpenField index ), ExtraMouse.onContextMenu ( FlagField index ) ] [ text "[]" ]

            ( Close, Mine ) ->
                button [ ExtraMouse.onClick ( OpenField index ), ExtraMouse.onContextMenu ( FlagField index ) ] [ text "[]" ]

            ( Open, Empty ) ->
                if adjacentMines == 0 then
                    button [ ] [ text "." ]
                else if adjacentMines == adjacentFlags then
                    button [ ExtraMouse.onContextMenu ( OpenAdjacents index ) ] [ String.fromInt adjacentMines |> text ]
                else
                    button [ ] [ String.fromInt adjacentMines |> text ]

            ( Open, Mine ) ->
                button [ ] [ text "M" ]

            ( Flag, _ ) ->
                button [ ExtraMouse.onContextMenu ( UnFlagField index ) ] [ text "F" ]


-- UTILITY

getRow : Int -> Int
getRow index =
        index // xSize


getColumn : Int -> Int
getColumn index =
        modBy xSize index


fieldIsOpen : Field -> Bool
fieldIsOpen { status } =
    case status of
        Open -> True
        _ -> False


fieldIsClosed : Field -> Bool
fieldIsClosed { status } =
    case status of
        Close -> True
        _ -> False

        
doAdjacent : Int -> ( Int -> Array.Array Field -> Array.Array Field ) -> Array.Array Field -> Array.Array Field
doAdjacent index modelFunction fields =
    getAdjacent index
    |> Array.foldl modelFunction fields


getIndexField : Array.Array Field -> Int -> Maybe ( Int, Field )
getIndexField fields index =
    case Array.get index fields of
        Nothing -> 
            Nothing

        Just field -> 
            Just ( index, field )


getIndex : ( Int, Field ) -> Int
getIndex ( index, _ ) =
    index
    

filterField : ( Field -> Bool ) -> ( Int, Field ) -> Bool
filterField fieldFilter ( _, field ) =
    fieldFilter field


filterCountAdjacent : Int -> (Field -> Bool) -> Array.Array Field -> Int
filterCountAdjacent index fieldFilter fields =
    getAdjacent index
    |> Array.toList
    |> List.filterMap ( \index1 -> Array.get index1 fields )
    |> List.filter fieldFilter
    |> List.length


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



