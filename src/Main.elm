module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder)
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


type alias Model =
    { serchQuery : String
    , page : Page
    }


type Page
    = NotFound
    | SearchResult (List Deceased)
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
    | GotQebrResponse (Result Http.Error QebrResponse)


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput SearchField, placeholder "Search" ] [ text model.serchQuery ]
        , button [ onClick Search ]
            [ text "Search" ]
        , viewPage model.page
        ]


viewPage : Page -> Html Msg
viewPage page =
    case page of
        Start ->
            div []
                []

        SearchResult result ->
            div [] (List.map viewDeceased result)

        NotFound ->
            div [] []


viewDeceased : Deceased -> Html Msg
viewDeceased deceased =
    div []
        [ text deceased.fullName
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

        GotQebrResponse (Ok response) ->
            ( { model | page = SearchResult response.results }, Cmd.none )

        GotQebrResponse (Err err) ->
            let
                _ =
                    Debug.log "err " err
            in
            ( model, Cmd.none )


searchQebr : String -> Cmd Msg
searchQebr query =
    Http.get
        { url = qebrUrl ++ "deceased/?search=" ++ query
        , expect = Http.expectJson GotQebrResponse qebrDecoder
        }


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
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
