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
            [ if sudoku.correct then
                Html.p [ style [ ( "color", "green" ) ] ] [ text "SOLUTION CORRECT" ]
              else
                Html.p [ style [ ( "color", "red" ) ] ] [ text "SOLUTION INCORRECT" ]
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
    Html.program { init = ( sudoku2Model (string2ListList sudoku), Cmd.none ), view = view, update = update, subscriptions = subscriptions }



-------------------------------------------------------------------------------------------------


subscriptions : Sudoku -> Sub Msg
subscriptions model =
    Keyboard.presses (\code -> Presses (fromCode code))



-----------------------------------------parse string to model-----------------------------------


string2ListList : String -> List (List Int)
string2ListList str =
    if String.length str < 10 then
        [ List.map str2int (String.split "" str) ]
    else
        List.map str2int (String.split "" (String.slice 0 9 str)) :: string2ListList (String.slice 8 -1 str)


str2int : String -> Int
str2int str =
    case String.toInt str of
        Ok n ->
            n

        _ ->
            0


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
    "010000000060000820002018000030080002000052080000401500000140000070000000120300006"
