module Connect4 exposing (..)

-- Connect4
-- A two player game where the aim is to connect 4 pieces of your colour
-- together vertically, horizontaly or diagonally before your opponet does.



import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes as Att
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes
import List
import Array
import Flip
import Array.Extra


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = {
  board: Board,
  turnColour : Colour,
  winner : Colour
  }

type alias Board = Array.Array (Array.Array Colour)
type Colour = Red | Yellow | Empty

init : Model
init = {
  board = Array.repeat 6 (Array.repeat 7 Empty),
  turnColour = Red,
  winner = Empty
  }




-- UPDATE


type Msg
  = Place Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    Place int ->
      let newBoard = (updateArray int 5 (model.board) model.turnColour)
      in
      {-if the game has not been won make the move otherwise do nothing-}
      if model.winner == Empty then
        if check4s newBoard then
            {model | board = newBoard,
                     turnColour = Yellow,
                     winner = model.turnColour}
        else
          {-If the new board is the same as the old (i.e invalid move) dont
          swap the player-}
          if model.board == newBoard then
            model
          else
            if model.turnColour == Red then
              {model | board = newBoard, turnColour = Yellow}
            else
              {model | board = newBoard, turnColour = Red}
      else
        model

{-Checks if a connect 4 has been made-}
check4s: Board -> Bool
check4s board = (check4Across board)
             || (check4Down board)
             || (checkPosDiagonals board)
             || (checkNegDiagonals board)

{-Checks if there are 4 pieces of the same colour in a horizontal row-}
check4Across: Board -> Bool
check4Across board =
   Array.foldl (||) False (Array.map (rowCheckFliped 0 Empty) board)

{-Checks if there a 4 pieces of the same colour in a vertical "row"-}
check4Down: Board -> Bool
check4Down board =
   (check4Across (rotateBoard board))


checkPosDiagonals: Board -> Bool
checkPosDiagonals board =
  (check4Across (rotateBoardPos15 board))

checkNegDiagonals: Board -> Bool
checkNegDiagonals  board =
  (check4Across (rotateBoardNeg15 board))

{- Rotate Board so that we can reuse check4Across to check4Down-}
rotateBoard: Board -> Board
rotateBoard board = let xRange = Array.fromList <| List.range 0 7
                    in
                    Array.map (getColumn board) xRange

{-Array of all the colours in xInt'th column on the board
  used to rotate the board-}
getColumn: Board -> Int -> Array.Array Colour
getColumn board xInt =
  let yRange = Array.fromList <| List.range 0 6
  in
  Array.map (Maybe.withDefault Empty)
    <| Array.map (Array.get xInt)
      <| Array.map (Maybe.withDefault Array.empty)
        <| Array.map (board |> Flip.flip Array.get) yRange




rotateBoardPos15: Board -> Board
rotateBoardPos15 board =
   let xRange = Array.fromList <| List.range 3 8
   in
   Array.map (getPosDiagonals board) xRange

rotateBoardNeg15: Board -> Board
rotateBoardNeg15 board =
  let xRange = Array.fromList <| List.range -3 2
  in
  Array.map (getNegDiagonals board) xRange


getPosDiagonals: Board -> Int -> Array.Array Colour
getPosDiagonals board int =
  let
    yRange = Array.fromList <| List.reverse <| List.range 0 int
    xRange = Array.fromList <| List.range 0 int
  in
  Array.map (Maybe.withDefault Empty)
    <| Array.Extra.map2 (<|) (Array.map Array.get xRange)
     <| Array.map (Maybe.withDefault Array.empty)
      <| Array.map (board |> Flip.flip Array.get) yRange


getNegDiagonals: Board -> Int -> Array.Array Colour
getNegDiagonals board int =
  let
    yRange = Array.fromList <| List.range int 8
    xRange = Array.fromList <| List.range 0 6
  in
  Array.map (Maybe.withDefault Empty)
    <| Array.Extra.map2 (<|) (Array.map Array.get xRange)
     <| Array.map (Maybe.withDefault Array.empty)
      <|Array.map (board |> Flip.flip Array.get) yRange

{-Version of rowCheck with the first and last argument flipped to make currying
  easier-}
rowCheckFliped: Int -> Colour -> Array.Array Colour -> Bool
rowCheckFliped count lastColour row  = rowCheck row count lastColour

{-Check if a connect 4 has been made on a given row
 count is the number of type lastColour that have already been in a row-}
rowCheck: Array.Array Colour -> Int -> Colour -> Bool
rowCheck row count lastColour =
  if count >= 4 then
    True
  else
    if Array.isEmpty row then
      False
    else
      if ((Maybe.withDefault (Empty) (Array.get ((Array.length row) - 1) row))
        == lastColour)
        && lastColour /= Empty then
        rowCheck
          (Array.slice 0 -1 row)
          (count + 1)
          (Maybe.withDefault (Empty) (Array.get ((Array.length row) - 1) row))
      else
        rowCheck
          (Array.slice 0 -1 row)
          1
          (Maybe.withDefault (Empty) (Array.get ((Array.length row) - 1) row))

{-places a piece-}
updateArray : Int -> Int -> Board -> Colour -> Board
updateArray colInt rowInt board colour =
  let
    unitPiece =
      Maybe.withDefault
        (Empty)
        <| Array.get
           colInt
           <| Maybe.withDefault
              Array.empty
              <| Array.get rowInt board

    newRow =
      Array.set
        colInt
        colour
        <| Maybe.withDefault
          Array.empty
          <| Array.get rowInt board
  in
  if colInt < 0 || rowInt < 0 then
    board
  else
    if unitPiece == Red || unitPiece == Yellow then
      updateArray colInt (rowInt-1) board colour
    else
      Array.set rowInt newRow board

-- VIEW


view : Model -> Html Msg
view model =
    div [] [
            Html.h1 [] [text "Connect 4"],
            viewText model,
            (viewBoard model),
            Html.p
              [Att.class "footerText"]
              [text "Made by Domenic Yates. Written in Elm. ",
               Html.a
                [Att.href "https://github.com/Domenic-Y/ElmConnect4"]
                [text "Code Here."]]]


viewText: Model -> Html Msg
viewText model =
    if model.winner == Red then
      Html.h2 [Att.class "textRed"] [text "Red Wins!"]
    else if model.winner == Yellow then
      Html.h2 [Att.class "textYellow"] [text "Yellow Wins!"]
    else
      if model.turnColour == Red then
        Html.h2 [] [text "Red's Turn"]
      else
        Html.h2 [] [text "Yellow's Turn"]


viewBoard: Model -> Html Msg
viewBoard model =
  div
    [Att.class "parent"]
    [div
      [ Att.class "row"]
      <| List.map (viewButton)(List.range 0 6)
    ,div
      []
      Array.toList <| Array.map viewRow (model.board)]

viewRow: Array.Array Colour -> Html Msg
viewRow row =
  div [Att.class "row"] (Array.toList (Array.map viewColour row))

viewButton: Int -> Html Msg
viewButton int =
  button
    [onClick <| Place int, Att.class "colButton"]
    [text <| String.fromInt int]

viewColour: Colour -> Html Msg
viewColour colour =
  case colour of
    Red ->
      div [] [Svg.svg
              [Svg.Attributes.width "100", Svg.Attributes.height "100"]
              [Svg.circle
               [Svg.Attributes.cx "50",
                Svg.Attributes.cy "50",
                Svg.Attributes.r "30",
                Svg.Attributes.fill "red"]
               [ ] ] ]
    Yellow ->
      div [] [Svg.svg
              [Svg.Attributes.width "100", Svg.Attributes.height "100"]
              [Svg.circle
                [Svg.Attributes.cx "50",
                 Svg.Attributes.cy "50",
                 Svg.Attributes.r "30",
                 Svg.Attributes.fill "yellow"]
                [ ] ] ]
    Empty ->
      div [] [Svg.svg [ Svg.Attributes.width "100", Svg.Attributes.height "100"]
                      [Svg.circle
                        [Svg.Attributes.cx "50",
                         Svg.Attributes.cy "50",
                         Svg.Attributes.r "30",
                         Svg.Attributes.fill "gray"]
                       [ ]]]
