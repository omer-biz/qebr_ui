module Main exposing (main)

import Browser
import Element exposing (alignBottom, alignRight, centerX, fill, mouseDown, mouseOver, padding, rgb255, spacing)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode exposing (Decoder, fail, field, float, int, list, nullable, string, succeed)
import Json.Decode.Pipeline exposing (required)
import SearchBox


debugMode : Bool
debugMode =
    True


qebrUrl : String
qebrUrl =
    if debugMode then
        "http://localhost:8080/"

    else
        Debug.todo "when will you be hosting?"


apiVersion : String
apiVersion =
    "api_v1/"


logoPath : String
logoPath =
    qebrUrl ++ "static/qebr_logo.png"


autocompletePath : String
autocompletePath =
    qebrUrl ++ apiVersion ++ "autocomplete/"


type alias Model =
    { page : Page
    , autocomplete : Autocomplete
    }


type alias Autocomplete =
    { suggestions : Maybe (List String)
    , selected : Maybe String
    , boxState : SearchBox.State
    , searchQuery : String
    }


type Page
    = SearchResult (List Deceased)
    | DetailView Deceased
    | Start


type Gender
    = Male
    | Female


type alias Location =
    { lon : Float
    , lat : Float
    }


type alias Deceased =
    { fullName : String
    , roleNumber : Int
    , gender : Gender
    , graveNumber : Int
    , afocha : String
    , qebele : Int
    , location : Location
    }


type alias QebrResponse =
    { count : Int
    , next : Maybe String
    , previous : Maybe String
    , results : List Deceased
    }



-- sampleDeceased : Deceased
-- sampleDeceased =
--     Deceased "Ali Ahmed Abdi" 134 Male 23453 "Haji Ali" 2 <| Location 12.1 221


type Msg
    = Search
    | ViewDetailDeceased Deceased
    | GotQebrResponse (Result Http.Error QebrResponse)
    | GotAfochaResponse (Result Http.Error String)
    | GotAutocomplete (Result Http.Error (List String))
    | ChangedSearchField (SearchBox.ChangeEvent String)


view : Model -> Browser.Document Msg
view model =
    { title = "Qebr"
    , body =
        [ Element.layout [{- Background.color <| rgb255 23 23 23 -}] <|
            Element.column
                [ Element.width fill, Element.height fill ]
                [ viewHeader
                , viewContent model
                , viewFooter
                ]
        ]
    }


viewHeader : Element.Element msg
viewHeader =
    let
        boxShadow =
            Border.shadow
                { blur = 5
                , color = rgb255 0x00 0x00 0x00
                , offset = ( 1, 1 )
                , size = 0.5
                }
    in
    Element.el
        [ Element.width fill
        , boxShadow
        ]
    <|
        Element.row
            [ spacing 10
            , padding 10
            , alignRight
            ]
            [ headerLink "About Us"
            , headerLink "Contact Us"
            ]


headerLink : String -> Element.Element msg
headerLink content =
    let
        headerStyle =
            [ Font.color <| rgb255 0x00 0x00 0x1F
            ]
    in
    Element.link headerStyle { url = "https://www.google.com", label = Element.text content }


viewFooter : Element.Element msg
viewFooter =
    Element.row [ centerX, alignBottom, padding 20 ] [ Element.text "Copyright" ]


viewContent : Model -> Element.Element Msg
viewContent model =
    Element.column [ centerX, spacing 10 ]
        [ viewSearchField model.autocomplete
        , viewResults model.page

        -- , viewDeceased sampleDeceased
        ]


viewResults : Page -> Element.Element Msg
viewResults page =
    case page of
        Start ->
            Element.none

        SearchResult results ->
            viewListDeceased results

        DetailView deceased ->
            viewDetailDeceased deceased


viewListDeceased : List Deceased -> Element.Element Msg
viewListDeceased results =
    Element.row [ centerX, spacing 10 ] <| List.map viewDeceased results


viewSearchField : Autocomplete -> Element.Element Msg
viewSearchField autocomplete =
    Element.column [ centerX ]
        [ Element.image [ centerX, Element.width (Element.px 400) ] { src = logoPath, description = "Qebr" }
        , Element.row [ spacing 20 ]
            [ searchInputField autocomplete
            , Input.button
                simpleButtonStyle
                { onPress = Just Search, label = Element.text "Search" }
            ]
        ]


searchInputField : Autocomplete -> Element.Element Msg
searchInputField autocomplete =
    SearchBox.input [ padding 5, Border.width 2, Element.width <| Element.px 400 ]
        { onChange = ChangedSearchField
        , text = autocomplete.searchQuery
        , selected = autocomplete.selected
        , options = autocomplete.suggestions
        , label = Input.labelHidden ""
        , placeholder = Nothing
        , toLabel = \opt -> opt
        , filter = \_ _ -> True
        , state = autocomplete.boxState
        }


simpleButtonStyle : List (Element.Attribute msg)
simpleButtonStyle =
    [ mouseDown [ Background.color <| rgb255 182 86 139, Font.color <| rgb255 0xFF 0x00 0xFF ]
    , mouseOver [ Background.color <| rgb255 86 182 139, Font.color <| rgb255 0xFF 0xFF 0xFF ]
    , Border.rounded 5
    , Border.width 1
    , padding 5
    ]


viewDeceased : Deceased -> Element.Element Msg
viewDeceased deceased =
    Element.column
        [ Element.padding 10
        , Border.color <| rgb255 0xFF 0x00 0x00
        , Border.width 1
        , spacing 5
        , Border.rounded 2
        ]
        [ Element.text deceased.fullName
        , Element.text <| String.fromInt deceased.qebele
        , Input.button simpleButtonStyle { onPress = Just (ViewDetailDeceased deceased), label = Element.text "More" }
        ]


viewDetailDeceased : Deceased -> Element.Element msg
viewDetailDeceased deceased =
    Element.column [ Element.padding 10 ]
        [ Element.text deceased.fullName
        , Element.text deceased.afocha
        ]


modSelection : Autocomplete -> String -> Autocomplete
modSelection ac selected =
    { ac | selected = Just selected }


modQuery : Autocomplete -> String -> Autocomplete
modQuery ac query =
    { ac
        | searchQuery = query
        , selected = Nothing
        , boxState = SearchBox.reset ac.boxState
    }


modBoxState : Autocomplete -> SearchBox.Msg -> Autocomplete
modBoxState ac subMsg =
    { ac | boxState = SearchBox.update subMsg ac.boxState }


modSuggestions : Autocomplete -> List String -> Autocomplete
modSuggestions ac suggestions =
    { ac | suggestions = Just suggestions, boxState = SearchBox.reset ac.boxState }


init : () -> ( Model, Cmd msg )
init _ =
    ( Model Start <| Autocomplete Nothing Nothing SearchBox.init "", Cmd.none )


updateSearchBox : Model -> SearchBox.ChangeEvent String -> ( Model, Cmd Msg )
updateSearchBox model ev =
    case ev of
        SearchBox.SelectionChanged selected ->
            ( { model | autocomplete = modSelection model.autocomplete selected }, Cmd.none )

        SearchBox.TextChanged text ->
            ( { model | autocomplete = modQuery model.autocomplete text }, fetchAutocomplete text )

        SearchBox.SearchBoxChanged subMsg ->
            ( { model | autocomplete = modBoxState model.autocomplete subMsg }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedSearchField ev ->
            updateSearchBox model ev

        Search ->
            let
                query =
                    case model.autocomplete.selected of
                        Nothing ->
                            model.autocomplete.searchQuery

                        Just text ->
                            text
            in
            ( model, searchQebr query )

        ViewDetailDeceased deceased ->
            ( { model | page = DetailView deceased }, fetchAfocha deceased.afocha )

        GotAfochaResponse (Ok response) ->
            ( updateAfochPage model response, Cmd.none )

        GotAfochaResponse (Err err) ->
            let
                _ =
                    Debug.log "err " err
            in
            ( model, Cmd.none )

        GotQebrResponse (Ok response) ->
            ( { model | page = SearchResult response.results }, Cmd.none )

        GotQebrResponse (Err err) ->
            let
                _ =
                    Debug.log "err " err
            in
            ( model, Cmd.none )

        GotAutocomplete (Ok response) ->
            ( { model | autocomplete = modSuggestions model.autocomplete response }, Cmd.none )

        GotAutocomplete (Err err) ->
            let
                _ =
                    Debug.log "err " err
            in
            ( model, Cmd.none )


updateAfochPage : Model -> String -> Model
updateAfochPage model afocha =
    case model.page of
        DetailView deceased ->
            { model | page = DetailView { deceased | afocha = afocha } }

        _ ->
            model


fetchAfocha : String -> Cmd Msg
fetchAfocha afochaUrl =
    Http.get
        { url = afochaUrl
        , expect = Http.expectJson GotAfochaResponse afochaDecoder
        }


searchQebr : String -> Cmd Msg
searchQebr query =
    Http.get
        { url = qebrUrl ++ apiVersion ++ "deceased/?search=" ++ query
        , expect = Http.expectJson GotQebrResponse qebrDecoder
        }


fetchAutocomplete : String -> Cmd Msg
fetchAutocomplete query =
    if String.length query >= 3 then
        Http.get
            { url = autocompletePath ++ query
            , expect = Http.expectJson GotAutocomplete (list string)
            }

    else
        Cmd.none


afochaDecoder : Decoder String
afochaDecoder =
    field "name" string


qebrDecoder : Json.Decode.Decoder QebrResponse
qebrDecoder =
    succeed QebrResponse
        |> required "count" int
        |> required "next" (nullable string)
        |> required "previous" (nullable string)
        |> required "results" (list deceasedDecoder)


deceasedDecoder : Decoder Deceased
deceasedDecoder =
    succeed Deceased
        |> required "full_name" string
        |> required "role_num" int
        |> required "gender" decodeGender
        |> required "grave_number" int
        |> required "afocha_name" string
        |> required "kebele" int
        |> required "location" locationDecoder


locationDecoder : Decoder Location
locationDecoder =
    Json.Decode.map2 Location
        (field "lon" float)
        (field "lat" float)


decodeGender : Decoder Gender
decodeGender =
    string
        |> Json.Decode.andThen
            (\abcd ->
                case abcd of
                    "male" ->
                        succeed Male

                    "female" ->
                        succeed Female

                    _ ->
                        fail "invalid gender type"
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
