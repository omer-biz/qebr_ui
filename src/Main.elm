module Main exposing (main)

import Browser
import Element exposing (centerX)
import Element.Input as Input
import Http
import Json.Decode exposing (Decoder, fail, field, float, int, list, nullable, string, succeed)
import Json.Decode.Pipeline exposing (required)


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
    "static/qebr_logo.png"


type alias Model =
    { serchQuery : String
    , page : Page
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


type Msg
    = SearchField String
    | Search
    | ViewDetailDeceased Deceased
    | GotQebrResponse (Result Http.Error QebrResponse)
    | GotAfochaResponse (Result Http.Error String)


view : Model -> Browser.Document Msg
view model =
    { title = "Qebr"
    , body =
        [ Element.layout [{- Background.color <| rgb255 23 23 23 -}] <|
            Element.column
                [ centerX
                ]
                [ viewSearchField model.serchQuery
                , viewResults model.page
                ]
        ]
    }


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
    Element.row [] <| List.map viewDeceased results


viewSearchField : String -> Element.Element Msg
viewSearchField searchQuery =
    Element.column []
        [ Element.image [ Element.width (Element.px 400) ] { src = qebrUrl ++ logoPath, description = "Qebr" }
        , Element.row []
            [ Input.search [ Element.width Element.fill ]
                { onChange = SearchField
                , text = searchQuery
                , placeholder = Nothing
                , label = Input.labelHidden ""
                }
            , Input.button [] { onPress = Just Search, label = Element.text "Search" }
            ]
        ]


viewDeceased : Deceased -> Element.Element Msg
viewDeceased deceased =
    Element.column [ Element.padding 10 ]
        [ Element.text deceased.fullName
        , Element.text <| String.fromInt deceased.qebele
        , Input.button [] { onPress = Just (ViewDetailDeceased deceased), label = Element.text "More" }
        ]


viewDetailDeceased : Deceased -> Element.Element msg
viewDetailDeceased deceased =
    Element.column [ Element.padding 10 ]
        [ Element.text deceased.fullName
        , Element.text deceased.afocha
        ]


init : () -> ( Model, Cmd msg )
init _ =
    ( Model "" Start, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchField query ->
            ( { model | serchQuery = query }, Cmd.none )

        Search ->
            ( model, searchQebr model.serchQuery )

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
