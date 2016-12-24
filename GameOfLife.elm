import Html exposing (..)
import Html.Attributes exposing (..)

main: Program Never Model Msg
main =
    Html.program {
        init = init,
        view = view,
        update = update,
        subscriptions = \_ -> Sub.none
    }

-- MODEL

type Cell = Dead | Alive
type alias Row = List Cell
type alias Board = List Row
type alias Model = Board

init : (Model, Cmd Msg)
init =
    (emptyBoard, Cmd.none)

emptyBoard : Board
emptyBoard =
    List.map emptyRow (List.range 1 5)

emptyRow : Int -> Row
emptyRow i =
    List.map (\m -> Dead) (List.range 1 5)

-- UPDATE

type Msg =
    Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick ->
            (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    div [] [
        h1 [] [ text "Game Of Life" ],
        div [ class "board" ] [
            table [ style [ ("border-collapse", "collapse"), ("border", "1px solid black") ] ] [
                tbody [] (List.map renderRow model)
            ]
        ]
    ]

renderRow : Row -> Html Msg
renderRow row =
    tr [] (List.map renderCell row)

renderCell : Cell -> Html Msg
renderCell box =
    td [ style [ ("border", "1px solid black"), ("height", "50px"), ("width", "50px") ] ] []
