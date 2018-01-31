module Main exposing (..)

import Char exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (field, string)
import Keyboard exposing (..)
import Random
import Time exposing (Time, second)


--import Timer


css : Html msg
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

    --extra dodano za zaklepanje gumbov in lazje preverjanje
    , hint_Possible : Bool
    , reset_Possible : Bool
    , solution_Possilble : Bool
    , sudokuTime : Int
    , score : Int
    , startSudoku : List Row
    , resultSudoku : List Row
    , correctSudoku : Bool
    , difficulty : Int
    }



-------------------------------------------------------------------------------------------------


type Msg
    = Click Int Int
    | Presses Char
    | Check
    | Reset
    | GetSudoku
    | PrikaziResitev
    | Namig
    | GenStNamiga Int
    | Tick Time
    | NewSudoku (Result.Result Http.Error String)



-------------------------------------------------------------------------------------------------


view : Sudoku -> Html Msg
view sudoku =
    Html.div []
        [ css
        , List.indexedMap viewRow sudoku.sudoku |> Html.div [ style [ ( "width", "1600px" ) ] ]
        , Html.button [ style [ ( "background-color", "#f2f2f2" ), ( "padding", "15px 27px" ), ( "font-size", "16px" ), ( "border-radius", "15px" ) ], disabled (not sudoku.solution_Possilble || sudoku.correct), onClick Check ] [ text "CHECK SOLUTION" ]
        , Html.button [ style [ ( "background-color", "#f2f2f2" ), ( "padding", "15px 27px" ), ( "font-size", "16px" ), ( "border-radius", "15px" ) ], disabled (not sudoku.reset_Possible), onClick Reset ] [ text "RESET" ]
        , Html.button [ style [ ( "background-color", "#f2f2f2" ), ( "padding", "15px 27px" ), ( "font-size", "16px" ), ( "border-radius", "15px" ) ], onClick GetSudoku ] [ text "GENERATE" ]
        , Html.button [ style [ ( "background-color", "#f2f2f2" ), ( "padding", "15px 27px" ), ( "font-size", "16px" ), ( "border-radius", "15px" ) ], onClick PrikaziResitev ] [ text "PRIKAZI RESITEV" ]
        , Html.button [ style [ ( "background-color", "#f2f2f2" ), ( "padding", "15px 27px" ), ( "font-size", "16px" ), ( "border-radius", "15px" ) ], disabled (not sudoku.hint_Possible), onClick Namig ]
            [ text "NAMIG" ]
        , Html.div [ style [ ( "padding", "15px 27px" ), ( "font-size", "24px" ) ] ] [ Html.text ("Current time: " ++ toString sudoku.sudokuTime) ]
        , Html.div [ style [ ( "padding", "15px 27px" ), ( "font-size", "24px" ) ] ] [ Html.text ("Score: " ++ toString sudoku.score) ]
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
                Html.img [ style [ ( "padding", "2px 259px" ), ( "border-radius", "15px" ) ], width 200, height 100, src "./assets/correct.png" ] []
              else
                Html.img [ style [ ( "padding", "2px 259px" ), ( "border-radius", "15px" ) ], width 200, height 100, src "./assets/incorrect.png" ] []
            ]
        ]


viewRow : Int -> List { a | clicked : Bool, disabled : Bool, number : number } -> Html Msg
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
            ( checkIfCorrect sudoku, Cmd.none )

        Reset ->
            ( returnSudokuToOrigin sudoku, Cmd.none )

        PrikaziResitev ->
            ( prikaziResitev sudoku, Cmd.none )

        Namig ->
            ( sudoku, generirajNakljucnoVrednostZaNamig sudoku )

        GenStNamiga i ->
            ( generirajNamig sudoku sudoku.resultSudoku (generirajMozneNamige sudoku) i, Cmd.none )

        Tick time ->
            ( { sudoku | sudokuTime = sudoku.sudokuTime + 1 }, Cmd.none )

        GetSudoku ->
            ( sudoku, loadSudoku sudoku )

        NewSudoku (Ok s) ->
            ( updateSudoku s sudoku, Cmd.none )

        NewSudoku (Err _) ->
            ( sudoku, Cmd.none )



------------------------------------------API KLIC--------------------------------------------------


updateSudoku : String -> { l | checked : b, correct : c, correctSudoku : d, hint_Possible : e, reset_Possible : f, resultSudoku : g, solution_Possilble : h, startSudoku : i, sudoku : j, sudokuTime : k, score : a, difficulty : Int } -> { l | checked : Bool, correct : Bool, correctSudoku : Bool, hint_Possible : Bool, reset_Possible : Bool, resultSudoku : List Row, score : a, solution_Possilble : Bool, startSudoku : List Row, sudoku : List Row, sudokuTime : number, difficulty : Int }
updateSudoku s sudoku =
    let
        generateSudoku sudokuZac sudokuRes sudoku =
            { sudoku
                | sudoku = List.map list2row (string2ListList sudokuZac)
                , checked = False
                , correct = False
                , hint_Possible = True
                , reset_Possible = True
                , solution_Possilble = True
                , sudokuTime = 0
                , score = sudoku.score
                , startSudoku = List.map list2row (string2ListList sudokuZac)
                , resultSudoku = List.map list2row (string2ListList sudokuRes)
                , correctSudoku = False
                , difficulty = sudoku.difficulty % 3 + 1
            }
    in
    generateSudoku (String.left 81 s) (String.right 81 s) sudoku


loadSudoku : Sudoku -> Cmd Msg
loadSudoku sudoku =
    field "puzzle" string |> Http.get (apiUrl sudoku.difficulty) |> Http.send NewSudoku


apiUrl : Int -> String
apiUrl diff =
    "https://sudoku-andrej834.c9users.io/" ++ toString (randomDifficulty (diff % 3 + 1) sudokuDifficulty)


sudokuDifficulty : List String
sudokuDifficulty =
    [ "easy", "medium", "hard" ]


randomDifficulty : Int -> List String -> String
randomDifficulty diff list =
    case list of
        h :: t ->
            if diff == 0 then
                h
            else
                randomDifficulty (diff - 1) t

        _ ->
            "default"



--------------------------------------------ADD SCORE----------------------------------------------


checkIfCorrect : { c | checked : a, correct : b, difficulty : number, sudoku : List Row, sudokuTime : number, score : number } -> { c | checked : Bool, correct : Bool, difficulty : number, score : number, sudoku : List Row, sudokuTime : number }
checkIfCorrect sudoku =
    if checkSudoku sudoku.sudoku then
        { sudoku | checked = True, correct = True, score = sudoku.score + 500 * sudoku.difficulty - sudoku.sudokuTime }
    else
        { sudoku | checked = True, correct = False, score = sudoku.score }



------------------------------------------Generating Namig------------------------------------------


generirajNakljucnoVrednostZaNamig : Sudoku -> Cmd Msg
generirajNakljucnoVrednostZaNamig sudoku =
    Random.generate GenStNamiga (Random.int 0 (List.length (generirajMozneNamige sudoku)))


generirajNamig : Sudoku -> List Row -> List ( number, number ) -> Int -> Sudoku
generirajNamig sudoku resitev tuple i =
    let
        extractTuple : List ( number, number ) -> ( number, number )
        extractTuple list =
            case list of
                h :: t ->
                    h

                [] ->
                    ( -1, -1 )

        findNamig resitev y tuple =
            case resitev of
                h :: t ->
                    if y == Tuple.second tuple then
                        findRow h 0 y tuple
                    else
                        findNamig t (y + 1) tuple

                [] ->
                    sudoku

        findRow list x y tuple =
            case list of
                h :: t ->
                    if x == Tuple.first tuple then
                        updateNamig sudoku.sudoku 0 x y [] h.number
                    else
                        findRow t (x + 1) y tuple

                [] ->
                    sudoku

        updateNamig splitSudoku y xR yR newSudoku value =
            case splitSudoku of
                h :: t ->
                    if y == yR then
                        { sudoku | sudoku = newSudoku ++ updateRow h 0 xR [] value :: t, hint_Possible = False }
                    else
                        updateNamig t (y + 1) xR yR (newSudoku ++ [ h ]) value

                [] ->
                    sudoku

        updateRow row x xS newList value =
            case row of
                h :: t ->
                    if x == xS then
                        newList ++ [ Square value True False ] ++ t
                    else
                        updateRow t (x + 1) xS (newList ++ [ h ]) value

                [] ->
                    row
    in
    List.drop i tuple |> extractTuple |> findNamig resitev 0



----Vzame sudoku in na podlagi vseh praznih polj sestavi array (x,y) koordinat, kamor lahko vpisemo stevilko----


generirajMozneNamige : Sudoku -> List ( number, number )
generirajMozneNamige sudoku =
    let
        lookForSquares row x y tuples =
            case row of
                h :: t ->
                    if h.number == 0 then
                        lookForSquares t (x + 1) y (( x, y ) :: tuples)
                    else
                        lookForSquares t (x + 1) y tuples

                [] ->
                    tuples

        createTuples sudoku y tuples =
            case sudoku of
                h :: t ->
                    lookForSquares h 0 y tuples |> createTuples t (y + 1)

                [] ->
                    tuples
    in
    createTuples sudoku.sudoku 0 []



------------------------------------------HELPER-------------------------------------------------


returnSudokuToOrigin : { e | checked : a, correct : b, startSudoku : c, sudoku : d } -> { e | startSudoku : c, checked : Bool, correct : Bool, sudoku : c }
returnSudokuToOrigin sudoku =
    { sudoku
        | sudoku = sudoku.startSudoku
        , checked = False
        , correct = False
    }


prikaziResitev : { g | checked : a, correct : b, reset_Possible : c, resultSudoku : d, solution_Possilble : e, sudoku : f } -> { g | resultSudoku : d, checked : Bool, correct : Bool, reset_Possible : Bool, solution_Possilble : Bool, sudoku : d }
prikaziResitev sudoku =
    { sudoku
        | sudoku = sudoku.resultSudoku
        , checked = False
        , correct = False
        , reset_Possible = False
        , solution_Possilble = False
    }



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


sudokuRowInput : Char -> List { a | clicked : Bool, number : number } -> List { a | clicked : Bool, number : number }
sudokuRowInput code row =
    List.map (sudokuSquareInput code) row


sudokuSquareInput : Char -> { a | clicked : Bool, number : number } -> { a | clicked : Bool, number : number }
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


main : Program Never Sudoku Msg
main =
    Html.program { init = ( sudoku2Model (string2ListList sudoku), Cmd.none ), view = view, update = update, subscriptions = subscriptions }



-------------------------------------------------------------------------------------------------


subscriptions : Sudoku -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.presses (\code -> Presses (fromCode code))
        , Time.every second Tick
        ]



-----------------------------------------parse string to model-----------------------------------


string2ListList : String -> List (List Int)
string2ListList str =
    if String.length str < 10 then
        [ List.map str2int (String.split "" str) ]
    else
        List.map str2int (String.split "" (String.slice 0 9 str)) :: string2ListList (String.slice 9 (String.length str) str)


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
    , hint_Possible = True
    , reset_Possible = True
    , solution_Possilble = True
    , sudokuTime = 0
    , score = 0
    , startSudoku = List.map list2row sudoku
    , resultSudoku = List.map list2row (string2ListList resitevSudoka)
    , correctSudoku = False
    , difficulty = 1
    }


list2row : List Int -> Row
list2row list =
    List.map int2Square list


int2Square : Int -> Square
int2Square int =
    { number = int, disabled = int /= 0, clicked = False }


sudoku : String
sudoku =
    "000216007600479000000005000005002090090003270000000500400000000006300100970080050"


resitevSudoka : String
resitevSudoka =
    "358216947612479835749835621135762498894153276267948513483521769526397184971684352"
