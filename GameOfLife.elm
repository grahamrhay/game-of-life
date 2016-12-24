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
    Array.indexedMap (\i c -> (nextGenCell c (liveNeighbours rowIndex i board))) row

liveNeighbours : Int -> Int -> Board -> Int
liveNeighbours rowIndex colIndex board =
    liveNeighboursAbove rowIndex colIndex board +
    liveNeighboursAdjacent rowIndex colIndex board +
    liveNeighboursBelow rowIndex colIndex board

liveNeighboursAbove : Int -> Int -> Board -> Int
liveNeighboursAbove rowIndex colIndex board =
    isAlive (rowIndex - 1) (colIndex - 1) board +
    isAlive (rowIndex - 1) (colIndex) board +
    isAlive (rowIndex - 1) (colIndex + 1) board

liveNeighboursAdjacent : Int -> Int -> Board -> Int
liveNeighboursAdjacent rowIndex colIndex board =
    isAlive rowIndex (colIndex - 1) board +
    isAlive rowIndex (colIndex + 1) board

liveNeighboursBelow : Int -> Int -> Board -> Int
liveNeighboursBelow rowIndex colIndex board =
    isAlive (rowIndex + 1) (colIndex - 1) board +
    isAlive (rowIndex + 1) (colIndex) board +
    isAlive (rowIndex + 1) (colIndex + 1) board

isAlive : Int -> Int -> Board -> Int
isAlive rowIndex colIndex board =
    case (getCell colIndex (getRow rowIndex board)) of
        Alive -> 1
        Dead -> 0

getRow : Int -> Board -> Row
getRow i board =
    case Array.get i board of
        Just row -> row
        Nothing ->
            case Array.get ((Array.length board) - 1) board of
                Just row -> row
                Nothing -> Debug.crash "oops"

getCell : Int -> Row -> Cell
getCell i row =
    case Array.get i row of
        Just cell -> cell
        Nothing ->
            case Array.get ((Array.length row) - 1) row of
                Just cell -> cell
                Nothing -> Debug.crash "oops"

nextGenCell : Cell -> Int -> Cell
nextGenCell cell liveNeighbours =
    case cell of
        Alive ->
            if liveNeighbours < 2 then
               Dead
            else if liveNeighbours > 3 then
               Dead
            else
               Alive
        Dead ->
            if liveNeighbours == 3 then
                Alive
            else
                Dead

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
