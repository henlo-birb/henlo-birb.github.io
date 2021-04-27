module Main exposing (..)

--import Html.Attributes exposing (..)
--import File exposing (File)

import AllDict
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
import Maybe exposing (andThen, withDefault)
import Time exposing (Time)
import Utilities exposing (..)



---- MODEL ----


type Category
    = Name
    | Credits
    | Level
    | StartTime
    | EndTime
    | Day
    | RemoteStatus
    | MaxEnrollment
    | Frequency
    | TermLength


categoryStrings : List ( Category, String )
categoryStrings =
    [ ( Name, "Class Name" )
    , ( Credits, "Credits" )
    , ( Level, "Level" )
    , ( Day, "Days" )
    , ( StartTime, "Start Time" )
    , ( EndTime, "End Time" )
    , ( RemoteStatus, "Remote Status" )
    , ( MaxEnrollment, "Maximum Enrollment" )
    , ( Frequency, "Frequency" )
    , ( TermLength, "Length" )
    ]


type alias Model =
    { gottenLinks : List String
    , newLinks : List String
    , classes : List Class
    , inputText : String
    , showingHidden : Bool
    , sortingReversed : Bool
    , sortingBy : Category
    , filters : AllDict.Dict Category (List String)
    }


filterKeys : List Category
filterKeys =
    [ Day, Level, Credits, RemoteStatus, TermLength ]


filterValues : AllDict.Dict Category (List String)
filterValues =
    AllDict.fromList
        [ ( Day, [ "M", "T", "W", "Th", "F", "TBA" ] )
        , ( Level, [ "2000", "4000" ] )
        , ( Credits, [ "1", "2", "4" ] )
        , ( RemoteStatus, [ "Hybrid", "In Person", "Remotely Accessible" ] )
        , ( TermLength, [ "Full Term", "1st 7 Weeks", "2nd 7 Weeks", "1st Module Block", "2nd Module Block", "3rd Module Block", "4th Module Block" ] )
        ]


init : ( Model, Cmd Msg )
init =
    ( Model [] [] [] "" False False Name filterValues, Cmd.none )



---- UPDATE ----


type Msg
    = SetText String
    | GotPage String (Result Http.Error String)
    | ToggleClassHidden String
    | ToggleShowingHidden
    | SetFilter Category String Bool
    | ToggleFilters Category
    | SortClasses Category


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

        SetFilter category filter value ->
            let
                oldFilters =
                    get category model.filters

                newFilters =
                    ternary value (oldFilters ++ [ filter ]) (remove filter oldFilters)
            in
            ( { model | filters = AllDict.insert category newFilters model.filters }, Cmd.none )

        ToggleFilters category ->
            let
                oldFilters =
                    get category model.filters

                newFilters =
                    ternary (List.isEmpty oldFilters) (get category filterValues) []
            in
            ( { model | filters = AllDict.insert category newFilters model.filters }, Cmd.none )

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

                                lastDay =
                                    .days >> List.reverse >> List.head >> unwrap ""
                            in
                            sortBy (lastDay >> dayOrder) >> sortBy (firstDay >> dayOrder)

                        RemoteStatus ->
                            let
                                remoteOrder a =
                                    elemIndex a [ "Remote", "Hybrid", "In Person" ] |> unwrap 3
                            in
                            sortBy (.remoteStatus >> remoteOrder)

                        MaxEnrollment ->
                            sortBy .maxEnrollment

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

        checkbox : Category -> String -> Html Msg
        checkbox category filter =
            div []
                [ input [ type_ "checkbox", checked (List.member filter (get category model.filters)), onCheck (\value -> SetFilter category filter value) ] []
                , text filter
                ]

        classTh : ( Category, String ) -> Html Msg
        classTh ( category, name ) =
            th [ class "clickable", onClick <| SortClasses category ]
                [ span [] [ text name ]
                , br [] []
                , viewIcon <| caret category
                ]

        classTr : Class -> Html Msg
        classTr class =
            tr
                [ hidden <|
                    not <|
                        ((model.showingHidden || not class.hidden)
                            && List.any (\d -> List.member d class.days) (get Day model.filters)
                            && List.member class.level (get Level model.filters)
                            && List.member class.credits (get Credits model.filters)
                            && List.member class.remoteStatus (get RemoteStatus model.filters)
                            && List.member class.termLength (get TermLength model.filters)
                        )
                ]
                [ td []
                    [ button [ onClick <| ToggleClassHidden class.name ]
                        [ text <| ternary class.hidden "unhide" "hide" ]
                    ]
                , td [] [ text class.name ]
                , td [] [ text class.credits ]
                , td [] [ text class.level ]
                , td [] [ text <| String.join "/" class.days ]
                , td [] [ text <| Time.toString class.startTime ]
                , td [] [ text <| Time.toString class.endTime ]
                , td [] [ text class.remoteStatus ]
                , td [] [ text class.maxEnrollment ]
                , td [] [ text class.frequency ]
                , td [] [ text class.termLength ]
                , td [] [ a [ href class.link ] [ text "Link" ] ]
                ]

        filterDiv : ( Category, String ) -> Html Msg
        filterDiv ( category, name ) =
            div [ class "filterDiv" ]
                ([ span [] [ text name ]
                 , br [] []
                 ]
                    ++ List.map (checkbox category) (get category filterValues)
                    ++ [ button [ class "toggleFiltersButton", onClick <| ToggleFilters category ] [ text "Toggle all" ] ]
                )

        filterDivs =
            let
                kvs : List ( Category, String )
                kvs =
                    List.map (\k -> withDefault ( Name, "Class Name" ) <| List.head <| List.filter (\( key, _ ) -> key == k) categoryStrings) filterKeys
            in
            List.map filterDiv kvs

        headers =
            List.map classTh categoryStrings
    in
    div [ id "bigContainer" ]
        [ FontAwesome.Styles.css
        , h1 [] [ text "Bennington College Schedule Creator" ]
        , p [] [ text "If you have any issues or questions, email me at vriskaweaver@bennington.edu" ]
        , h2 []
            [ text "Put links to class "
            , a [ href "https://curriculum.bennington.edu/" ] [ text "curriculum" ]
            , text " pages here ↓"
            ]
        , textarea [ autofocus True, onInput SetText ] []
        , br [] []
        , br [] []
        , br [] []
        , div [ id "filters" ]
            ([ h2 [] [ text "Filters " ] ]
                ++ filterDivs
            )
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
