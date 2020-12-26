module MinesweeperCDN exposing (stylesheet)

import Html exposing (Html, node)
import Html.Attributes exposing (href, rel)


stylesheet : Html msg
stylesheet =
    node "link"
        [ rel "stylesheet"
        , href "styles/minesweeper.css"
        ]
        []
