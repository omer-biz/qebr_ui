module Main exposing (main)

import Browser
import Date exposing (Date)
import DatePicker exposing (ChangeEvent(..))
import Element exposing (alignLeft, alignRight, centerX, fill, mouseDown, mouseOver, padding, paddingXY, rgb255, spacing, spacingXY)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode exposing (Decoder, fail, field, float, int, list, maybe, nullable, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Maybe.Extra
import SearchBox
import Task


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


autocompletePath : String
autocompletePath =
    qebrUrl ++ apiVersion ++ "autocomplete/"


maleAvatar : String
maleAvatar =
    qebrUrl ++ "static/male_avatar.png"


femaleAvatar : String
femaleAvatar =
    qebrUrl ++ "static/female_avatar.png"


googleMap : Location -> String
googleMap location =
    "https://www.google.com/maps/search/?api=1&query=" ++ String.fromFloat location.lat ++ "," ++ String.fromFloat location.lat


type alias Model =
    { page : Page
    , searchField : SearchField
    }


type alias Autocomplete =
    { suggestions : Maybe (List String)
    , selected : Maybe String
    , boxState : SearchBox.State
    , searchQuery : String
    }


type SearchField
    = Simple Autocomplete
    | Advanced FilterFields


type alias DeathDate =
    { date : Maybe Date
    , dateText : String
    , pickerModel : DatePicker.Model
    }


type alias FilterFields =
    { fullName : String
    , qebele : String
    , dod : DeathDate
    , graveNumber : String
    , gender : Gender
    , afochaName : String
    }


type AdvancedMsg
    = FullName String
    | Qebele String
    | Dod ChangeEvent
    | GraveNumber String
    | Gender Gender
    | AfochaName String
    | SetToday Date


type Page
    = SearchResult QebrResponse
    | Start
    | Error Http.Error


type Gender
    = Male
    | Female
    | Unknown


type alias Location =
    { lon : Float
    , lat : Float
    }


type alias Afocha =
    { name : String }


type alias Deceased =
    { fullName : String
    , roleNumber : Int
    , gender : Gender
    , graveNumber : Int
    , afocha : Afocha
    , qebele : Int
    , location : Location
    , portraitPhoto : Maybe String
    }


type alias QebrResponse =
    { count : Int
    , next : Maybe String
    , previous : Maybe String
    , results : List Deceased
    }



-- sampleDeceased : Deceased
-- sampleDeceased =
--     Deceased "Ali Ahmed Abdi" 134 Male 23453 (Afocha "Haji Ali") 2 (Location 12.1 221) (Just maleAvatar)


type Msg
    = Search
    | GotQebrResponse (Result Http.Error QebrResponse)
    | GotAutocomplete (Result Http.Error (List String))
    | ChangedSearchField (SearchBox.ChangeEvent String)
    | FetchQebr String
    | ChangeToAdvanced
    | ChangeToSimple
    | ChangeFilterFields AdvancedMsg


view : Model -> Browser.Document Msg
view model =
    { title = "Qebr"
    , body =
        [ Element.layout [] <|
            Element.column
                [ Element.width fill, Element.height fill ]
                [ viewHeader
                , viewContent model
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


viewContent : Model -> Element.Element Msg
viewContent model =
    let
        field =
            case model.searchField of
                Simple autocomplete ->
                    viewSearchField autocomplete

                Advanced fields ->
                    viewAdvancedSearchFields fields
    in
    Element.column [ centerX, spacing 10 ]
        [ qebrLogo
        , field
        , viewResults model.page
        ]


viewAdvancedSearchFields : FilterFields -> Element.Element Msg
viewAdvancedSearchFields fields =
    let
        simpleInput text toFilterMsg label =
            Input.text [ padding 5, Border.width 2, Element.width <| Element.px 300 ]
                { onChange = ChangeFilterFields << toFilterMsg
                , placeholder = Nothing
                , label = Input.labelLeft [ Element.width fill, paddingXY 5 0 ] <| Element.text label
                , text = text
                }

        datePicker =
            DatePicker.input [ padding 5, Element.width <| Element.px 300 ]
                { onChange = ChangeFilterFields << Dod
                , selected = fields.dod.date
                , text = fields.dod.dateText
                , label = Input.labelLeft [ Element.width fill ] <| Element.text "Death Date"
                , placeholder = Just <| Input.placeholder [] <| Element.text "YYYY-MM-DD"
                , settings = DatePicker.defaultSettings
                , model = fields.dod.pickerModel
                }
    in
    Element.column [ spacing 10 ]
        [ simpleInput fields.fullName FullName "Full Name"
        , simpleInput fields.qebele Qebele "Qebele"
        , simpleInput fields.graveNumber GraveNumber "Grave Number"
        , simpleInput fields.afochaName AfochaName "Afocha"
        , Input.radio [ Element.width fill ]
            { label = Input.labelLeft [ Element.width fill ] <| Element.text "Gender"
            , onChange = ChangeFilterFields << Gender
            , options =
                [ Input.option Male (Element.text "Male")
                , Input.option Female (Element.text "Female")
                ]
            , selected = Just fields.gender
            }
        , datePicker
        , Element.row [ spacingXY 15 0, paddingXY 0 10 ]
            [ Input.button
                simpleButtonStyle
                { onPress = Just ChangeToSimple, label = Element.text "Simple Search" }
            , Input.button
                simpleButtonStyle
                { onPress = Just Search, label = Element.text "Search" }
            ]
        ]


viewResults : Page -> Element.Element Msg
viewResults page =
    case page of
        Start ->
            Element.none

        SearchResult results ->
            viewListDeceased results

        Error _ ->
            Element.el [] <| Element.text "Error: Can't connect to the server"


viewListDeceased : QebrResponse -> Element.Element Msg
viewListDeceased results =
    Element.column [ spacing 10, Element.width fill ] <|
        List.map viewDeceased results.results
            ++ [ Element.row
                    [ Element.width fill, paddingXY 0 20 ]
                 <|
                    resultNav { next = results.next, prev = results.previous }
               ]


resultNav : { next : Maybe String, prev : Maybe String } -> List (Element.Element Msg)
resultNav { next, prev } =
    [ navButton next "Next" alignRight
    , navButton prev "Previous" alignLeft
    ]


navButton : Maybe String -> String -> Element.Attribute Msg -> Element.Element Msg
navButton link label alignment =
    case link of
        Just url ->
            Input.button
                (alignment :: simpleButtonStyle)
                { onPress = Just (FetchQebr url), label = Element.text label }

        Nothing ->
            Element.none


qebrLogo : Element.Element msg
qebrLogo =
    Element.el [ centerX, Font.underline, Font.size 150, padding 20 ] <| Element.text "Qebr"


viewSearchField : Autocomplete -> Element.Element Msg
viewSearchField autocomplete =
    Element.column [ centerX, spacingXY 0 10 ]
        [ Element.row [ spacing 20 ]
            [ searchInputField autocomplete
            , Input.button
                simpleButtonStyle
                { onPress = Just Search, label = Element.text "Search" }
            , Input.button
                simpleButtonStyle
                { onPress = Just ChangeToAdvanced, label = Element.text "Advanced" }
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
    Element.row [ Element.width fill, paddingXY 0 10 ]
        [ Element.column
            [ Element.padding 10
            , spacing 5
            , Border.rounded 2
            , spacing 10
            ]
            [ Element.text <| "Full Name: " ++ deceased.fullName
            , Element.text <| "Kebele:  " ++ String.fromInt deceased.qebele
            , Element.text <| "Afocha: " ++ deceased.afocha.name
            , Element.link simpleButtonStyle
                { url = googleMap deceased.location
                , label = Element.text "Location on map"
                }
            ]
        , Element.image [ Element.width <| Element.px 100, alignRight ]
            { src = portraitFrom deceased.gender deceased.portraitPhoto, description = "" }
        ]


portraitFrom : Gender -> Maybe String -> String
portraitFrom gender portrait =
    case portrait of
        Just path ->
            path

        Nothing ->
            genderToAvatar gender


genderToAvatar : Gender -> String
genderToAvatar gender =
    case gender of
        Male ->
            maleAvatar

        Female ->
            femaleAvatar

        _ ->
            maleAvatar


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


initDeathDate : DeathDate
initDeathDate =
    DeathDate Nothing "" (DatePicker.init |> DatePicker.close)


init : () -> ( Model, Cmd msg )
init _ =
    -- ( Model Start <| Simple (Autocomplete Nothing Nothing SearchBox.init ""), Cmd.none )
    ( Model Start <| Advanced (FilterFields "" "" initDeathDate "" Unknown ""), Cmd.none )


updateSearchBox : Model -> SearchBox.ChangeEvent String -> Autocomplete -> ( Model, Cmd Msg )
updateSearchBox model ev ac =
    case ev of
        SearchBox.SelectionChanged selected ->
            ( { model | searchField = Simple (modSelection ac selected) }, Cmd.none )

        SearchBox.TextChanged text ->
            ( { model | searchField = Simple (modQuery ac text) }, fetchAutocomplete text )

        SearchBox.SearchBoxChanged subMsg ->
            ( { model | searchField = Simple (modBoxState ac subMsg) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeToAdvanced ->
            ( { model | searchField = Advanced (FilterFields "" "" initDeathDate "" Unknown "") }
            , Task.perform (ChangeFilterFields << SetToday) Date.today
            )

        ChangeToSimple ->
            ( { model | searchField = Simple (Autocomplete Nothing Nothing SearchBox.init "") }, Cmd.none )

        ChangedSearchField ev ->
            case model.searchField of
                Simple ac ->
                    updateSearchBox model ev ac

                _ ->
                    ( model, Cmd.none )

        Search ->
            let
                query =
                    case model.searchField of
                        Simple ac ->
                            case ac.selected of
                                Nothing ->
                                    ac.searchQuery

                                Just text ->
                                    text

                        _ ->
                            ""
            in
            ( model, searchQebr query )

        FetchQebr link ->
            ( model, fetchQebr link )

        GotQebrResponse (Ok response) ->
            ( { model | page = SearchResult response }, Cmd.none )

        GotQebrResponse (Err err) ->
            ( { model | page = serverError err }, Cmd.none )

        GotAutocomplete (Ok response) ->
            let
                m =
                    case model.searchField of
                        Simple ac ->
                            { model | searchField = Simple (modSuggestions ac response) }

                        _ ->
                            model
            in
            ( m, Cmd.none )

        GotAutocomplete (Err err) ->
            ( { model | page = serverError err }, Cmd.none )

        ChangeFilterFields advMsg ->
            case model.searchField of
                Advanced fields ->
                    ( { model | searchField = Advanced (updateAdvancedMsg advMsg fields) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


updateAdvancedMsg : AdvancedMsg -> FilterFields -> FilterFields
updateAdvancedMsg advMsg fields =
    case advMsg of
        FullName name ->
            { fields | fullName = name }

        Qebele qebele ->
            { fields | qebele = qebele }

        Gender gender ->
            { fields | gender = gender }

        AfochaName name ->
            { fields | afochaName = name }

        Dod dodEvent ->
            { fields | dod = updateDeathDate dodEvent fields.dod }

        GraveNumber gn ->
            { fields | graveNumber = gn }

        SetToday date ->
            { fields | dod = updateDDToday date fields.dod }


updateDDToday : Date -> DeathDate -> DeathDate
updateDDToday date deathDate =
    { deathDate | pickerModel = deathDate.pickerModel |> DatePicker.setToday date }


updateDeathDate : ChangeEvent -> DeathDate -> DeathDate
updateDeathDate event deathDate =
    case event of
        DateChanged date ->
            { deathDate
                | date = Just date
                , dateText = Date.toIsoString date
                , pickerModel = deathDate.pickerModel |> DatePicker.close
            }

        TextChanged text ->
            { deathDate
                | date =
                    Debug.log "date"
                        (Date.fromIsoString text
                            |> Result.toMaybe
                            |> Maybe.Extra.orElse deathDate.date
                        )
                , dateText = text
            }

        PickerChanged subMsg ->
            { deathDate
                | pickerModel =
                    deathDate.pickerModel
                        |> DatePicker.update subMsg
            }


serverError : Http.Error -> Page
serverError err =
    Error <| Debug.log "Error: " err


searchQebr : String -> Cmd Msg
searchQebr query =
    if String.length query >= 3 then
        Http.get
            { url = qebrUrl ++ apiVersion ++ "deceased/?search=" ++ query
            , expect = Http.expectJson GotQebrResponse qebrDecoder
            }

    else
        Cmd.none


fetchQebr : String -> Cmd Msg
fetchQebr link =
    Http.get
        { url = link
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


afochaDecoder : Decoder Afocha
afochaDecoder =
    succeed Afocha
        |> required "name" string


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
        |> required "afocha_name" afochaDecoder
        |> required "kebele" int
        |> required "location" locationDecoder
        |> required "portrait_photo" (maybe string)


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
