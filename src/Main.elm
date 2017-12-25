module Main exposing (..)

import Char exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard exposing (..)


css =
    node "link" [ rel "stylesheet", href "../style/style.css" ] []



-------------------------------------------------------------------------------------------------


type alias Square =
    -- Kvadratek v sudoku mrezi
    { number : Int
    , disabled : Bool
    , clicked : Bool
    }


type alias Row =
    -- Vrstica v sudoku mrezi
    List Square


type alias Sudoku =
    -- Sudoku mreza
    List Row



-------------------------------------------------------------------------------------------------


type Msg
    = Click Int Int
    | Presses Char



-------------------------------------------------------------------------------------------------


view : Sudoku -> Html Msg
view sudoku =
    css :: List.indexedMap viewRow sudoku |> Html.div []


viewRow y row =
    let
        buildHTML el ak =
            case el.disabled of
                True ->
                    ak ++ [ Html.img [ class ("c" ++ toString (List.length ak) ++ " r" ++ toString y), src ("../assets/" ++ "disabled_" ++ toString el.number ++ ".png") ] [] ]

                False ->
                    case el.clicked of
                        True ->
                            ak ++ [ Html.img [ class ("c" ++ toString (List.length ak) ++ " r" ++ toString y), onClick (Click (List.length ak) y), src "../assets/onClick.png" ] [] ]

                        False ->
                            case el.number of
                                0 ->
                                    ak ++ [ Html.img [ class ("c" ++ toString (List.length ak) ++ " r" ++ toString y), onClick (Click (List.length ak) y), src "../assets/empty.png" ] [] ]

                                n ->
                                    ak ++ [ Html.img [ class ("c" ++ toString (List.length ak) ++ " r" ++ toString y), onClick (Click (List.length ak) y), src ("../assets/" ++ toString n ++ ".png") ] [] ]
    in
    List.foldl buildHTML [] row |> Html.div []



-------------------------------------------------------------------------------------------------


update : Msg -> Sudoku -> ( Sudoku, Cmd.Cmd Msg )
update msg sudoku =
    case msg of
        Click x y ->
            ( sudokuClick x y sudoku, Cmd.none )

        Presses code ->
            ( sudokuInput code sudoku, Cmd.none )



-------------------------------------------------------------------------------------------------


sudokuClick : Int -> Int -> Sudoku -> Sudoku
sudokuClick x y sudoku =
    case ( sudoku, y ) of
        ( [], _ ) ->
            sudoku

        ( h :: t, 0 ) ->
            rowClick x h :: t

        ( h :: t, i ) ->
            h :: sudokuClick x (y - 1) t


rowClick : Int -> Row -> Row
rowClick x row =
    case ( row, x ) of
        ( [], _ ) ->
            row

        ( h :: t, 0 ) ->
            toggleClicked h :: t

        ( h :: t, i ) ->
            h :: rowClick (x - 1) t


toggleClicked : Square -> Square
toggleClicked s =
    if s.clicked then
        { s | number = 0, clicked = False }
    else
        { s | clicked = True }



-------------------------------------------------------------------------------------------------


sudokuInput : Char -> Sudoku -> Sudoku
sudokuInput code sudoku =
    List.map (sudokuRowInput code) sudoku


sudokuRowInput code row =
    List.map (sudokuSquareInput code) row


sudokuSquareInput code sq =
    case sq.clicked of
        False ->
            sq

        True ->
            case code of
                '1' ->
                    { sq | number = 1, clicked = False }

                '2' ->
                    { sq | number = 2, clicked = False }

                '3' ->
                    { sq | number = 3, clicked = False }

                '4' ->
                    { sq | number = 4, clicked = False }

                '5' ->
                    { sq | number = 5, clicked = False }

                '6' ->
                    { sq | number = 6, clicked = False }

                '7' ->
                    { sq | number = 7, clicked = False }

                '8' ->
                    { sq | number = 8, clicked = False }

                '9' ->
                    { sq | number = 9, clicked = False }

                _ ->
                    sq



-------------------------------------------------------------------------------------------------


main =
    Html.program { init = ( sudoku2Model sudoku, Cmd.none ), view = view, update = update, subscriptions = subscriptions }



-------------------------------------------------------------------------------------------------


subscriptions : Sudoku -> Sub Msg
subscriptions model =
    Keyboard.presses (\code -> Presses (fromCode code))



-------------------------------------------------------------------------------------------------


sudoku2Model : List (List Int) -> Sudoku
sudoku2Model sudoku =
    List.map list2row sudoku


list2row : List Int -> Row
list2row list =
    List.map int2Square list


int2Square : Int -> Square
int2Square int =
    { number = int, disabled = int /= 0, clicked = False }


sudoku =
    [ [ 0, 0, 3, 0, 2, 0, 6, 0, 0 ]
    , [ 9, 0, 0, 3, 0, 5, 0, 0, 1 ]
    , [ 0, 0, 1, 8, 0, 6, 4, 0, 0 ]
    , [ 0, 0, 8, 1, 0, 2, 9, 0, 0 ]
    , [ 7, 0, 0, 0, 0, 0, 0, 0, 8 ]
    , [ 0, 0, 6, 7, 0, 8, 2, 0, 0 ]
    , [ 0, 0, 2, 6, 0, 9, 5, 0, 0 ]
    , [ 8, 0, 0, 2, 0, 3, 0, 0, 9 ]
    , [ 0, 0, 5, 0, 1, 0, 3, 0, 0 ]
    ]
