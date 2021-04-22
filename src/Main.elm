module Main exposing (..)

--import Html.Attributes exposing (..)
--import File exposing (File)

import Browser
import Class exposing (..)
import FontAwesome.Icon exposing (viewIcon)
import FontAwesome.Solid exposing (sortDown, sortUp)
import FontAwesome.Styles
import Html exposing (Html, a, br, button, div, h1, h2, input, p, span, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes exposing (autofocus, checked, class, hidden, href, id, type_)
import Html.Events exposing (..)
import Http exposing (expectString)
import List exposing (map, sortBy, sortWith)
import List.Extra exposing (dropWhile, elemIndex, remove, uncons, updateIf)
import Maybe exposing (andThen)
import Time exposing (Time)
import Utilities exposing (..)



---- MODEL ----


type alias Model =
    { gottenLinks : List String
    , newLinks : List String
    , classes : List Class
    , inputText : String
    , showingHidden : Bool
    , sortingReversed : Bool
    , sortingBy : SortBy
    , dayFilters : List String
    , levelFilters : List Int
    , creditFilters : List Int
    , remoteStatusFilters : List String
    , termLengthFilters : List String
    }


days =
    [ "M", "T", "W", "Th", "F", "TBA" ]


levels =
    [ 2000, 4000 ]


creditses =
    [ 1, 2, 4 ]


remoteStatuses =
    [ "Hybrid", "In Person", "Remotely Accessible" ]


termLengths =
    [ "Full Term", "1st 7 Weeks", "2nd 7 Weeks", "1st Module Block", "2nd Module Block", "3rd Module Block", "4th Module Block" ]


init : ( Model, Cmd Msg )
init =
    ( Model [] [] [] "" False False Name days levels creditses remoteStatuses termLengths, Cmd.none )



---- UPDATE ----


type SortBy
    = Name
    | Credits
    | Level
    | StartTime
    | EndTime
    | Day
    | RemoteStatus
    | Frequency
    | TermLength


type Msg
    = SetText String
    | GotPage String (Result Http.Error String)
    | ToggleClassHidden String
    | ToggleShowingHidden
    | SetDayFilter String Bool
    | SetLevelFilter Int Bool
    | SetCreditFilter Int Bool
    | SetTermLengthFilter String Bool
    | SetRemoteStatusFilter String Bool
    | SortClasses SortBy


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        getClass : String -> Cmd Msg
        getClass link =
            Http.get { url = "https://vriskas-cors-anywhere.herokuapp.com/" ++ link, expect = expectString <| GotPage link }

        nextClass : List String -> ( Cmd Msg, List String )
        nextClass =
            uncons >> unwrapMap ( Cmd.none, [] ) (Tuple.mapFirst getClass)
    in
    case msg of
        SetText text ->
            let
                links =
                    text
                        |> String.replace "," " "
                        |> String.words
                        |> dropWhile (\a -> List.member a model.gottenLinks)

                ( cmd, newLinks ) =
                    nextClass links
            in
            ( { model | newLinks = newLinks }, cmd )

        GotPage link result ->
            let
                class =
                    result
                        |> Result.toMaybe
                        |> andThen (generateClass link >> log >> Result.toMaybe)
                        |> unwrapMap [] List.singleton

                ( cmd, newLinks ) =
                    nextClass model.newLinks
            in
            ( { model | classes = List.Extra.uniqueBy .name (model.classes ++ class), gottenLinks = model.gottenLinks ++ ternary (class == []) [] [ link ], newLinks = newLinks }, cmd )

        ToggleClassHidden className ->
            ( { model
                | classes = updateIf (.name >> (==) className) (\x -> { x | hidden = not x.hidden }) model.classes
              }
            , Cmd.none
            )

        ToggleShowingHidden ->
            ( { model | showingHidden = not model.showingHidden }, Cmd.none )

        SetDayFilter filter value ->
            let
                dayFilters =
                    ternary value (model.dayFilters ++ [ filter ]) (remove filter model.dayFilters)
            in
            ( { model | dayFilters = dayFilters }, Cmd.none )

        SetLevelFilter filter value ->
            let
                levelFilters =
                    ternary value (model.levelFilters ++ [ filter ]) (remove filter model.levelFilters)
            in
            ( { model | levelFilters = levelFilters }, Cmd.none )

        SetCreditFilter filter value ->
            let
                creditFilters =
                    ternary value (model.creditFilters ++ [ filter ]) (remove filter model.creditFilters)
            in
            ( { model | creditFilters = creditFilters }, Cmd.none )

        SetRemoteStatusFilter filter value ->
            let
                remoteStatusFilters =
                    ternary value (model.remoteStatusFilters ++ [ filter ]) (remove filter model.remoteStatusFilters)
            in
            ( { model | remoteStatusFilters = remoteStatusFilters }, Cmd.none )

        SetTermLengthFilter filter value ->
            let
                termLengthFilters =
                    ternary value (model.termLengthFilters ++ [ filter ]) (remove filter model.termLengthFilters)
            in
            ( { model | termLengthFilters = termLengthFilters }, Cmd.none )

        SortClasses by ->
            let
                sorter =
                    case by of
                        Name ->
                            sortBy .name

                        Credits ->
                            sortBy .credits

                        Level ->
                            sortBy .level

                        StartTime ->
                            sortWith (\a b -> Time.compareTime a.startTime b.startTime)

                        EndTime ->
                            sortWith (\a b -> Time.compareTime a.endTime b.endTime)

                        Day ->
                            let
                                dayOrder a =
                                    elemIndex a [ "M", "T", "W", "Th", "F", "TBA" ] |> unwrap 6

                                firstDay =
                                    .days >> List.head >> unwrap ""
                            in
                            sortBy (firstDay >> dayOrder)

                        RemoteStatus ->
                            let
                                remoteOrder a =
                                    elemIndex a [ "Remote", "Hybrid", "In Person" ] |> unwrap 3
                            in
                            sortBy (.remoteStatus >> remoteOrder)

                        Frequency ->
                            sortBy .frequency

                        TermLength ->
                            sortBy .termLength
            in
            ( if by == model.sortingBy then
                { model | classes = model.classes |> List.reverse, sortingReversed = not model.sortingReversed }

              else
                { model | classes = model.classes |> sorter, sortingBy = by, sortingReversed = False }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        caret a =
            ternary (model.sortingBy == a && model.sortingReversed) sortUp sortDown

        dayCheckbox : String -> Html Msg
        dayCheckbox filter =
            div []
                [ input [ type_ "checkbox", checked (List.member filter model.dayFilters), onCheck (\value -> SetDayFilter filter value) ] []
                , text filter
                ]

        creditCheckbox : Int -> Html Msg
        creditCheckbox filter =
            div []
                [ input [ type_ "checkbox", checked (List.member filter model.creditFilters), onCheck (\value -> SetCreditFilter filter value) ] []
                , text <| String.fromInt filter
                ]

        levelCheckbox : Int -> Html Msg
        levelCheckbox filter =
            div []
                [ input [ type_ "checkbox", checked (List.member filter model.levelFilters), onCheck (\value -> SetLevelFilter filter value) ] []
                , text <| String.fromInt filter
                ]

        remoteStatusCheckbox : String -> Html Msg
        remoteStatusCheckbox filter =
            div []
                [ input [ type_ "checkbox", checked (List.member filter model.remoteStatusFilters), onCheck (\value -> SetRemoteStatusFilter filter value) ] []
                , text <| filter
                ]

        termLengthCheckbox : String -> Html Msg
        termLengthCheckbox filter =
            div []
                [ input [ type_ "checkbox", checked (List.member filter model.termLengthFilters), onCheck (\value -> SetTermLengthFilter filter value) ] []
                , text <| filter
                ]

        classTh : String -> SortBy -> Html Msg
        classTh name sortBy_ =
            th [ class "clickable", onClick <| SortClasses sortBy_ ]
                [ span [] [ text name ]
                , br [] []
                , viewIcon <| caret sortBy_
                ]

        classTr : Class -> Html Msg
        classTr class =
            tr
                [ hidden <|
                    not <|
                        (model.showingHidden
                            || not class.hidden
                            && List.any (\d -> List.member d class.days) model.dayFilters
                            && List.member class.level model.levelFilters
                            && List.member class.credits model.creditFilters
                            && List.member class.remoteStatus model.remoteStatusFilters
                            && List.member class.termLength model.termLengthFilters
                        )
                ]
                [ td []
                    [ button [ onClick <| ToggleClassHidden class.name ]
                        [ text <| ternary class.hidden "unhide" "hide" ]
                    ]
                , td [] [ text class.name ]
                , td [] [ text <| String.fromInt class.credits ]
                , td [] [ text <| String.fromInt class.level ]
                , td [] [ text <| String.join "/" class.days ]
                , td [] [ text <| Time.toString class.startTime ]
                , td [] [ text <| Time.toString class.endTime ]
                , td [] [ text class.remoteStatus ]
                , td [] [ text class.frequency ]
                , td [] [ text class.termLength ]
                , td [] [ a [ href class.link ] [ text class.link ] ]
                ]

        headers =
            List.map2 classTh
                [ "Class Name", "Credits", "Level", "Days", "Start Time", "End Time", "Remote Status", "Frequency", "Length" ]
                [ Name, Credits, Level, Day, StartTime, EndTime, RemoteStatus, Frequency, TermLength ]
    in
    div [ id "bigContainer" ]
        [ FontAwesome.Styles.css
        , h1 [] [ text "Bennington College Schedule Creator" ]
        , p [] [ text "If you have any issues or questions, email me at vriskaweaver@bennington.edu" ]
        , h2 [] [ text "Put links to class pages here ↓" ]
        , textarea [ autofocus True, onInput SetText ] []
        , br [] []
        , br [] []
        , br [] []
        , div [ id "filters" ]
            [ h2 [] [ text "Filters " ]
            , div [ class "filterDiv" ]
                ([ span [] [ text "Days" ]
                 , br [] []
                 ]
                    ++ List.map dayCheckbox days
                )
            , div [ class "filterDiv" ]
                ([ span [] [ text "Credits" ]
                 , br [] []
                 ]
                    ++ List.map creditCheckbox creditses
                )
            , div [ class "filterDiv" ]
                ([ span [] [ text "Level" ]
                 , br [] []
                 ]
                    ++ List.map levelCheckbox levels
                )
            , div [ class "filterDiv" ]
                ([ span [] [ text "Remote Status" ]
                 , br [] []
                 ]
                    ++ List.map remoteStatusCheckbox remoteStatuses
                )
            , div [ class "filterDiv" ]
                ([ span [] [ text "Length" ]
                 , br [] []
                 ]
                    ++ List.map termLengthCheckbox termLengths
                )
            ]
        , table []
            [ thead [] <|
                [ th []
                    [ button [ onClick ToggleShowingHidden ] [ text <| ternary model.showingHidden "Hide hidden" "Show hidden" ]
                    ]
                ]
                    ++ headers
                    ++ [ th [] [ text "Link to curriculum page" ] ]
            , tbody [] <| map classTr model.classes
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
