import Html exposing (..)
import Html.Attributes exposing (..)
import Random

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
    ([], Random.generate NewBoard (seedBoard))

seedBoard : Random.Generator Board
seedBoard =
    Random.list 5 seedRow

seedRow : Random.Generator Row
seedRow =
    Random.list 5 seedCell

seedCell : Random.Generator Cell
seedCell =
    Random.map (\b -> if b then Dead else Alive) Random.bool

-- UPDATE

type Msg =
    NewBoard Board |
    Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick ->
            (model, Cmd.none)
        NewBoard board ->
            (board, Cmd.none)

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
renderCell cell =
    td [ style [ ("border", "1px solid black"), ("height", "50px"), ("width", "50px"), ("background-color", backgroundColor cell) ] ] []

backgroundColor : Cell -> String
backgroundColor cell =
    case cell of
        Alive -> "black"
        Dead -> "white"
