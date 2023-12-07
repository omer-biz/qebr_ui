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



-- sampleDeceased : Deceased
-- sampleDeceased =
--     Deceased "Ali Ahmed Abdi" 134 Male 23453 "Haji Ali" 2 <| Location 12.1 221


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
        [ viewSearchField model.serchQuery
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


viewSearchField : String -> Element.Element Msg
viewSearchField searchQuery =
    Element.column [ centerX ]
        [ Element.image [ Element.width (Element.px 400) ] { src = qebrUrl ++ logoPath, description = "Qebr" }
        , Element.row [ spacing 20 ]
            [ Input.search [ padding 5, Border.width 2 ]
                { onChange = SearchField
                , text = searchQuery
                , placeholder = Nothing
                , label = Input.labelHidden "search label"
                }
            , Input.button
                simpleButtonStyle
                { onPress = Just Search, label = Element.text "Search" }
            ]
        ]


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
