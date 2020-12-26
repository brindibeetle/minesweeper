module Main exposing (..)


import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (Url)
import Array as Array
import Random as Random
import Html.Events.Extra.Mouse as ExtraMouse
import Bootstrap.CDN as CDN
import Bootstrap.Form.Input as Input

import MinesweeperCDN exposing (..)
import Parameters as Parameters exposing (..)




-- MODEL

type alias Model = 
    {
        fields : Array.Array Field
        , parameters : Parameters.Parameters
        , parametersNew : Parameters.Parameters
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
        { init = init Parameters.init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- INIT

init : Parameters -> String -> ( Model, Cmd Msg )
init parameters _ =  
    let
        fields = Array.initialize (Parameters.getSize parameters) (\index -> getField index)
        { mines } = parameters
    in
        ( 
            { fields = fields
            , parameters = parameters
            , parametersNew = parameters
            }
            , List.repeat mines ( Random.generate SetRandomMine (minesGenerator parameters) )
              |> Cmd.batch 
        )


getField : Int -> Field
getField index =
    { status = Close, content = Empty }
    

minesGenerator : Parameters -> Random.Generator Int
minesGenerator parameters =
    Random.int 0 (Parameters.getSize parameters - 1)


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
    | DoNothing ExtraMouse.Event
    | DoClear ExtraMouse.Event
    | DoNew ExtraMouse.Event
    | DoXsize String
    | DoYsize String
    | DoMines String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { fields, parameters, parametersNew } = model
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
                        ( doOpenFieldAndAdjacents parameters index fields |> setFields model, Cmd.none )

            OpenAdjacents index _ ->
                case Array.get index fields of
                    Nothing ->
                        ( model, Cmd.none )

                    Just field ->
                        ( doOpenAdjacents parameters index fields |> setFields model, Cmd.none )
                                

            PickRandomIndex ->
                ( model , Random.generate SetRandomMine (minesGenerator parameters) )

            SetRandomMine index ->
                case Array.get index fields of
                    Nothing ->
                        ( model, Cmd.none )

                    Just field ->
                        case field.content of
                            Mine ->
                                ( model, Random.generate SetRandomMine (minesGenerator parameters) )

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
            DoNothing _ ->
                ( model, Cmd.none )

            DoClear _ ->
                (
                    { model
                    | fields = doClearFields fields 
                    , parametersNew = parameters
                    }
                    , Cmd.none
                )

            DoNew _ ->
                init parametersNew ""

            DoXsize newSize ->
                case String.toInt newSize of
                    Nothing ->
                        ( model, Cmd.none )
                    Just size ->
                        let
                            parameters1 = { parametersNew | xSize = size }
                        in
                            (
                                { model
                                | parametersNew = parameters1
                                }
                                , Cmd.none
                            )

            DoYsize newSize ->
                case String.toInt newSize of
                    Nothing ->
                        ( model, Cmd.none )
                    Just size ->
                        let
                            parameters1 = { parametersNew | ySize = size }
                        in
                            (
                                { model
                                | parametersNew = parameters1
                                }
                                , Cmd.none
                            )

            DoMines newSize ->
                case String.toInt newSize of
                    Nothing ->
                        ( model, Cmd.none )
                    Just size ->
                        let
                            parameters1 = { parametersNew | mines = size }
                        in
                            (
                                { model
                                | parametersNew = parameters1
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


doOpenFieldAndAdjacents : Parameters -> Int -> Array.Array Field -> Array.Array Field
doOpenFieldAndAdjacents parameters index fields =
    case Array.get index fields of
        Nothing ->
            fields

        Just field ->
            case ( field.status, field.content ) of
                ( Close, Empty ) ->
                    let
                        adjacentMines = filterCountAdjacent parameters index ( \field1 -> field1.content == Mine ) fields
                    in
                        if adjacentMines == 0 then
                            doOpenField index fields
                            |> doAdjacent parameters index (doOpenFieldAndAdjacents parameters)
                        else
                            doOpenField index fields
                    
                ( Close, _ ) ->
                    doOpenField index fields

                ( _, _ ) ->
                    fields


doOpenAdjacents : Parameters -> Int -> Array.Array Field -> Array.Array Field
doOpenAdjacents parameters index fields =
    doAdjacent parameters index (doOpenFieldAndAdjacents parameters) fields
    -- getAdjacent index
    -- |> Array.toList
    -- |> List.filterMap ( getIndexField fields )
    -- |> Array.fromList
    -- |> Array.filter (filterField fieldIsClosed) 
    -- |> Array.map getIndex
    -- |> Array.foldl doOpenField fields


doClearFields : Array.Array Field -> Array.Array Field
doClearFields fields =
    Array.map ( \field -> { field | status = Close } ) fields
-- VIEW


view : Model -> Document Msg
view model =
    let
        { flags, mineFlags, openFields } = getStatusReport model.fields
        { xSize, ySize, mines } = model.parameters
        { parametersNew, parameters } = model
        errorMessage = Parameters.validate parametersNew |> Maybe.withDefault "" 
    in
        {
            title = "Minesweeper"
            , body = 
                [
                    CDN.stylesheet
                    , MinesweeperCDN.stylesheet
                    , div 
                        [ class "main"
                        ]
                        [ div
                            [ class "wrapper"]
                            [ div
                                [ class "top-info" ]
                                [
                                    div
                                        [ class "info-text" ]
                                        [ text ( String.fromInt openFields ++ "/" ++ String.fromInt ( xSize * ySize ) ++ " fields, "
                                        ++ String.fromInt mineFlags ++ "/" ++ String.fromInt mines ++ " mines" ) ]
                                    , button [ class "mine-button", ExtraMouse.onClick DoClear ] [ text "clear" ]
                                ]
                            , div
                                [ class "mine-grid"
                                , style "grid-template-columns" ( "repeat(" ++ ( String.fromInt xSize ) ++ ", 30px )" )
                                , style "grid-template-rows" ( "repeat(" ++ ( String.fromInt ySize ) ++ ", 30px )" )
                                ]
                                ( Array.toList model.fields
                                |> List.indexedMap (viewField model)
                                )
                            , div
                                [ class "bottom-info" ]
                                [
                                    div ( if parametersNew.xSize == parameters.xSize then [ class "mine-input" ] else [ class "mine-input", class "mine-input-new" ] )
                                        [ div [ class "mine-label" ] [ text "x size"]
                                        , Input.number [ Input.value (String.fromInt parametersNew.xSize), Input.attrs [ class "mine-value" ], Input.onInput DoXsize ]
                                        ]
                                    , div ( if parametersNew.ySize == parameters.ySize then [ class "mine-input" ] else [ class "mine-input", class "mine-input-new" ] )
                                        [ div [ class "mine-label" ] [ text "y size"]
                                        , Input.number [ Input.value (String.fromInt parametersNew.ySize), Input.attrs [ class "mine-value" ], Input.onInput DoYsize ]
                                        ]
                                    , div ( if parametersNew.mines == parameters.mines then [ class "mine-input" ] else [ class "mine-input", class "mine-input-new" ] )
                                        [ div [ class "mine-label" ] [ text "mines"]
                                        , Input.number [ Input.value (String.fromInt parametersNew.mines), Input.attrs [ class "mine-value" ], Input.onInput DoMines ]
                                        ]
                                    , button [ class "mine-button", ExtraMouse.onClick DoNew, disabled (errorMessage /= "") ] [ text "new" ]
                                ]
                                , div [ class "mine-input-message" ]
                                [ text errorMessage ]
                            ]
                        ]
                ]
        }


viewField : Model -> Int -> Field -> Html Msg
viewField { fields, parameters } index { status, content } =
    let
        adjacentMines = filterCountAdjacent parameters index ( \field -> field.content == Mine ) fields
        adjacentFlags = filterCountAdjacent parameters index ( \field -> field.status == Flag ) fields
    in
        case ( status, content ) of
            ( Close, Empty ) ->
                if adjacentMines == 0 then
                    div [ class "field field-closed", ExtraMouse.onClick ( OpenFieldAndAdjacents index ), ExtraMouse.onContextMenu ( FlagField index ) ] [ text "[]" ]
                else 
                    div [ class "field field-closed", ExtraMouse.onClick ( OpenField index ), ExtraMouse.onContextMenu ( FlagField index ) ] [ text "[]" ]

            ( Close, Mine ) ->
                div [ class "field field-closed", ExtraMouse.onClick ( OpenField index ), ExtraMouse.onContextMenu ( FlagField index ) ] [ text "[]" ]

            ( Open, Empty ) ->
                if adjacentMines == 0 then
                    div [ class "field field-empty", ExtraMouse.onContextMenu DoNothing ] [ text "" ]
                else if adjacentMines == adjacentFlags then
                    div [ class "field field-empty" , ExtraMouse.onContextMenu ( OpenAdjacents index ) ] [ String.fromInt adjacentMines |> text ]
                else
                    div [ class "field field-empty" , ExtraMouse.onContextMenu DoNothing ] [ String.fromInt adjacentMines |> text ]

            ( Open, Mine ) ->
                div [ class "field field-mine" , ExtraMouse.onContextMenu DoNothing ] [ img [ src "images/mine.png", width 20, height 20  ] [] ]

            ( Flag, _ ) ->
                div [ class "field field-flag", ExtraMouse.onContextMenu ( UnFlagField index ) ] [ img [ src "images/flag1.png", width 20, height 20 ] [] ]


-- UTILITY


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

        
doAdjacent : Parameters -> Int -> ( Int -> Array.Array Field -> Array.Array Field ) -> Array.Array Field -> Array.Array Field
doAdjacent parameters index modelFunction fields =
    getAdjacent parameters index
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


filterCountAdjacent : Parameters -> Int -> (Field -> Bool) -> Array.Array Field -> Int
filterCountAdjacent paramateres index fieldFilter fields =
    getAdjacent paramateres index
    |> Array.toList
    |> List.filterMap ( \index1 -> Array.get index1 fields )
    |> List.filter fieldFilter
    |> List.length


getAdjacent : Parameters -> Int -> Array.Array Int
getAdjacent parameters index =
    let 
        { xSize, ySize } = parameters
    in
        Array.empty
        |> ( if Parameters.getRow parameters index > 0 && Parameters.getColumn parameters index > 0                   then Array.push ( index - xSize - 1 ) else identity )
        |> ( if Parameters.getRow parameters index > 0                                                                then Array.push ( index - xSize ) else identity )
        |> ( if Parameters.getRow parameters index > 0 && Parameters.getColumn parameters index + 1 < xSize           then Array.push ( index - xSize + 1 ) else identity )

        |> ( if Parameters.getColumn parameters index > 0                                                             then Array.push ( index - 1 ) else identity )
        |> ( if Parameters.getColumn parameters index + 1 < xSize                                                     then Array.push ( index + 1 ) else identity )

        |> ( if Parameters.getRow parameters index + 1 < ySize && Parameters.getColumn parameters index > 0           then Array.push ( index + xSize - 1 ) else identity )
        |> ( if Parameters.getRow parameters index + 1 < ySize                                                        then Array.push ( index + xSize ) else identity )
        |> ( if Parameters.getRow parameters index + 1 < ySize && Parameters.getColumn parameters index + 1 < xSize   then Array.push ( index + xSize + 1 ) else identity )


getStatusReport : Array.Array Field -> { flags : Int, mineFlags : Int, openFields : Int }
getStatusReport fields =
    Array.foldl 
        ( \{ status, content } statusReport -> 
            let
                { flags, mineFlags, openFields } = statusReport
            in
            case ( status, content ) of
                ( Flag, Mine ) ->
                    { statusReport 
                    | flags = flags + 1
                    , mineFlags = mineFlags + 1
                    , openFields = openFields + 1
                    }

                ( Flag, _ ) ->
                    { statusReport 
                    | flags = flags + 1
                    }
                
                ( Open, _ ) ->
                    { statusReport 
                    | openFields = openFields + 1
                    }

                ( _, _) ->
                    statusReport
        )
        { flags = 0, mineFlags = 0, openFields = 0 }
        fields
