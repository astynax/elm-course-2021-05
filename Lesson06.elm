module Lesson06 exposing (main)

import Browser
import Html exposing (Html, Attribute)
import Html.Attributes exposing (style)
import Html.Events as HE
import Http
import Platform exposing (Program)
import Task
import Time exposing (Posix)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JP

type Color = RGB Int Int Int

type alias Data =
    { fg : Maybe Color
    , bg : Maybe Color
    , text : String
    }

type alias Model =
    { time : Maybe Posix
    , zone : Maybe Time.Zone
    , text : Maybe Data
    }

type Msg
    = SetTime Posix
    | SetZone Time.Zone
    | GetResponse (Maybe Data)
    | Refresh

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

init : () -> (Model, Cmd Msg)
init () =
    let model =
            { time = Nothing
            , zone = Nothing
            , text = Nothing
            }
    in
        ( model
        , Cmd.batch
              [ Task.perform SetZone Time.here
              , Task.perform SetTime Time.now
              , fetch model
              ]
        )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 SetTime
        ]

view : Model -> Html Msg
view model =
    let
        (h, m, s) =
            Maybe.withDefault (0, 0, 0)
                <| Maybe.map2 fromPosix model.zone model.time
        fg =
            fromColor "color"
                <| Maybe.andThen .fg model.text
        bg =
            fromColor "background-color"
                <| Maybe.andThen .bg model.text
    in Html.div []
        [ Html.text <| Debug.toString (h, m, s)
        , Html.div
              (List.append fg bg)
              [ Html.text
                    <| Maybe.withDefault ""
                    <| Maybe.map .text model.text
              ]
        , Html.button [HE.onClick Refresh]
            [Html.text "Refresh"]
        ]

fromColor : String -> Maybe Color -> List (Attribute a)
fromColor key =
    Maybe.withDefault []
        << Maybe.map
            (\(RGB r g b) ->
                 [style key
                      <| String.concat
                      ["rgb("
                      , String.fromInt r
                      , ","
                      , String.fromInt g
                      , ","
                      , String.fromInt b
                      ,")"
                      ]]
            )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetTime time ->
            ({ model | time = Just time }, Cmd.none)
        SetZone zone ->
            ({ model | zone = Just zone }, Cmd.none)
        GetResponse text ->
            ({ model | text = text }, Cmd.none)
        Refresh ->
            ( model, fetch model )

fromPosix : Time.Zone -> Posix -> (Int, Int, Int)
fromPosix z p =
    ( Time.toHour z p
    , Time.toMinute z p
    , Time.toSecond z p
    )

handleGet : Http.Expect Msg
handleGet =
    Http.expectJson
        (GetResponse << Result.toMaybe)
        data

fetch : Model -> Cmd Msg
fetch model =
    let
        key = String.fromInt
              <| Maybe.withDefault 0
              <| Maybe.map Time.posixToMillis model.time
    in Http.get
        { url = String.append "./data.json?q=" key
        , expect = handleGet
        }

data : Decoder Data
data =
    JD.succeed Data
        |> JP.custom (JD.maybe (JD.field "fg" color))
        |> JP.custom (JD.maybe (JD.field "bg" color))
        |> JP.required "text" JD.string

color : Decoder Color
color =
    JD.succeed RGB
        |> JP.custom (JD.index 0 JD.int)
        |> JP.custom (JD.index 1 JD.int)
        |> JP.custom (JD.index 2 JD.int)
