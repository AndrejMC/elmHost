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
    { sudoku : List Row
    , checked : Bool
    , correct : Bool
    }



-------------------------------------------------------------------------------------------------


type Msg
    = Click Int Int
    | Presses Char
    | Check



-------------------------------------------------------------------------------------------------


view : Sudoku -> Html Msg
view sudoku =
    Html.div []
        [ css
        , List.indexedMap viewRow sudoku.sudoku |> Html.div [ style [ ( "width", "800px" ) ] ]
        , Html.button [ style [ ( "background-color", "yellow" ), ( "padding", "15px 32px" ), ( "font-size", "16px" ), ( "border-radius", "15px" ) ], onClick Check ] [ text "CHECK SOLUTION" ]
        , Html.div
            [ style
                [ ( "display"
                  , if sudoku.checked then
                        "block"
                    else
                        "none"
                  )
                ]
            ]
            [ Html.text
                (if sudoku.correct then
                    "SOLUTION CORRECT"
                 else
                    "SOLUTION INCORECT"
                )
            ]
        ]


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

        Check ->
            ( { sudoku | checked = True, correct = checkSudoku sudoku.sudoku }, Cmd.none )



------------------------------------------CHECK_SOLUTION------------------------------------------


checkSudoku : List Row -> Bool
checkSudoku s =
    case ( checkRows s, checkCols s, checkBoxes s ) of
        ( True, True, True ) ->
            True

        _ ->
            False


checkRows : List Row -> Bool
checkRows s =
    case s of
        [] ->
            True

        h :: t ->
            if unique h [] then
                checkRows t
            else
                False


checkCols : List Row -> Bool
checkCols s =
    checkRows (rotate s)


checkBoxes : List Row -> Bool
checkBoxes s =
    checkRows (rotate2squares s)


unique : List Square -> List Int -> Bool
unique sqList intList =
    -- preveri ce so v seznamu sami unikati
    case sqList of
        [] ->
            True

        h :: t ->
            if h.number == 0 then
                False
            else if List.member h.number intList then
                False
            else
                unique t (h.number :: intList)


rotate : List Row -> List Row
rotate list =
    -- rotira sudoku iz zapisa po vrsticah na zapis po stolpcih
    case list of
        [ h1 :: t1, h2 :: t2, h3 :: t3, h4 :: t4, h5 :: t5, h6 :: t6, h7 :: t7, h8 :: t8, h9 :: t9 ] ->
            [ h1, h2, h3, h4, h5, h6, h7, h8, h9 ] :: rotate [ t1, t2, t3, t4, t5, t6, t7, t8, t9 ]

        _ ->
            list


rotate2squares : List Row -> List Row
rotate2squares list =
    -- rotira sudoku iz zapisa po vrsticah v zapis po kvadratih
    case list of
        (h1 :: h2 :: h3 :: t1) :: (h4 :: h5 :: h6 :: t2) :: (h7 :: h8 :: h9 :: t3) :: t ->
            [ h1, h2, h3, h4, h5, h6, h7, h8, h9 ] :: rotate2squares (t1 :: t2 :: t3 :: t)

        [] :: [] :: [] :: t ->
            rotate2squares t

        _ ->
            list



-----------------------------------------OnClick-------------------------------------------------


sudokuClick : Int -> Int -> Sudoku -> Sudoku
sudokuClick x y s =
    { s | sudoku = sudokuClick2 x y s.sudoku }


sudokuClick2 : Int -> Int -> List Row -> List Row
sudokuClick2 x y sudoku =
    case ( sudoku, y ) of
        ( [], _ ) ->
            sudoku

        ( h :: t, 0 ) ->
            rowClick x h :: t

        ( h :: t, i ) ->
            h :: sudokuClick2 x (y - 1) t


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



-------------------------------------OnKeyboardInput---------------------------------------------


sudokuInput : Char -> Sudoku -> Sudoku
sudokuInput code s =
    { s | sudoku = List.map (sudokuRowInput code) s.sudoku }


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
    { sudoku = List.map list2row sudoku
    , checked = False
    , correct = False
    }


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
