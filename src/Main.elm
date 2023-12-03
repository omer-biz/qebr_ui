module Main exposing (main)

import Browser
import Html exposing (Html, button, div, img, input, span, text)
import Html.Attributes exposing (placeholder, src, value, width)
import Html.Events exposing (onClick, onInput)
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


logoPath : String
logoPath =
    "static/qebr_logo.png"


type alias Model =
    { serchQuery : String
    , page : Page
    }


type Page
    = NotFound
    | SearchResult (List Deceased)
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
        [ div [] <|
            viewSearchField model.serchQuery
                :: (case model.page of
                        Start ->
                            []

                        SearchResult result ->
                            List.map viewDeceased result

                        DetailView deceased ->
                            [ viewDetailDeceased deceased ]

                        NotFound ->
                            []
                   )
        ]
    }


viewSearchField : String -> Html Msg
viewSearchField searchQuery =
    span []
        [ img [ src <| qebrUrl ++ logoPath, width 300 ] []
        , span [] []
        , input [ onInput SearchField, placeholder "Search", value searchQuery ] []
        , button [ onClick Search ]
            [ text "Search" ]
        ]


viewDeceased : Deceased -> Html Msg
viewDeceased deceased =
    div []
        [ text deceased.fullName
        , text " \n "
        , text deceased.afocha
        , button [ onClick (ViewDetailDeceased deceased) ] [ text "More" ]
        ]


viewDetailDeceased : Deceased -> Html Msg
viewDetailDeceased deceased =
    div []
        [ text deceased.fullName
        , text deceased.afocha
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
        { url = qebrUrl ++ "deceased/?search=" ++ query
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
