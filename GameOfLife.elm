import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Random
import Time

main: Program Never Model Msg
main =
    Html.program {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }

-- MODEL

type Cell = Dead | Alive
type alias Row = Array.Array Cell
type alias Board = Array.Array Row
type alias Model = Board

init : (Model, Cmd Msg)
init =
    (Array.empty, Random.generate NewBoard (seedBoard))

seedBoard : Random.Generator (List (List Cell))
seedBoard =
    Random.list 5 seedRow

seedRow : Random.Generator (List Cell)
seedRow =
    Random.list 5 seedCell

seedCell : Random.Generator Cell
seedCell =
    Random.map (\b -> if b then Dead else Alive) Random.bool

-- UPDATE

type Msg =
    NewBoard (List (List Cell)) |
    Tick Time.Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick t ->
            (nextGeneration model, Cmd.none)
        NewBoard cells ->
            (toBoard cells, Cmd.none)

toBoard : (List (List Cell)) -> Board
toBoard cells =
    Array.fromList (List.map toRow cells)

toRow : (List Cell) -> Row
toRow cells =
    Array.fromList cells

nextGeneration : Board -> Board
nextGeneration board =
    Array.indexedMap (\i r -> (nextGenRow i r board)) board

nextGenRow : Int -> Row -> Board -> Row
nextGenRow rowIndex row board =
    Array.indexedMap (\i c -> (nextGenCell i rowIndex c board)) row

nextGenCell : Int -> Int -> Cell -> Board -> Cell
nextGenCell cellIndex rowIndex cell board =
    case cell of
        Alive -> Dead
        Dead -> Alive

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
      Time.every Time.second Tick

-- VIEW

view : Model -> Html Msg
view model =
    div [] [
        h1 [] [ text "Game Of Life" ],
        div [ class "board" ] [
            table [ style [ ("border-collapse", "collapse"), ("border", "1px solid black") ] ] [
                tbody [] (Array.toList (Array.map renderRow model))
            ]
        ]
    ]

renderRow : Row -> Html Msg
renderRow row =
    tr [] (Array.toList (Array.map renderCell row))

renderCell : Cell -> Html Msg
renderCell cell =
    td [ style [ ("border", "1px solid black"), ("height", "50px"), ("width", "50px"), ("background-color", backgroundColor cell) ] ] []

backgroundColor : Cell -> String
backgroundColor cell =
    case cell of
        Alive -> "black"
        Dead -> "white"
