module App exposing (..)

import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (svg, polygon)
import Svg.Attributes exposing (version, x, y, viewBox, fill, points, stroke, strokeWidth)
import Http
import Colors


type alias Model =
    { dress : Dress
    , colorList : List String
    }


type alias Dress =
    { color : String
    , strokeColor : String
    , points : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model initDressConfig initColors
    , Cmd.none
    )


initDressConfig : Dress
initDressConfig =
    let
        initPoints =
            [ ( 50, 50 )
            , ( 100, 50 )
            , ( 110, 40 )
            , ( 100, 100 )
            , ( 150, 150 )
            , ( 0, 150 )
            , ( 50, 100 )
            , ( 40, 40 )
            , ( 50, 50 )
            , ( 90, 50 )
            ]

        points =
            List.map (adjustPoints ( 0, (-30) )) initPoints
    in
        { color =
            Colors.magenta
        , strokeColor = Colors.black
        , points =
            (points
                |> List.map polyPoint
                |> String.join " "
            )
        }


initColors : List String
initColors =
    [ Colors.blue
    , Colors.magenta
    , Colors.pink
    , Colors.red
    , Colors.yellow
    , Colors.purple
    , Colors.green
    , Colors.black
    ]



-- UPDATE


type Msg
    = ChangeDressColor String
    | ChangeStrokeColor String
    | FetchSucceed Model
    | FetchFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeDressColor color ->
            ( { model | dress = { strokeColor = model.dress.strokeColor, color = color, points = model.dress.points } }, Cmd.none )

        ChangeStrokeColor strokeColor ->
            ( { model | dress = { strokeColor = strokeColor, color = model.dress.color, points = model.dress.points } }, Cmd.none )

        FetchSucceed model ->
            ( model, Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ div [ class "container-fluid" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-12 col-lg-12" ]
                        [ text "Dress Designer 1.0"
                        ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-6 col-lg-6" ]
                    [ (dressView model)
                    ]
                , div [ class "col-md-6 col-lg-6" ]
                    [ div [ class "feature-box" ]
                        [ h2 [] [ text "Dress Color" ]
                        , span [] (List.map dressColorSwatchBtn model.colorList)
                        ]
                    ]
                , div [ class "col-md-6 col-lg-6" ]
                    [ div [ class "feature-box" ]
                        [ h2 [] [ text "Stroke Color" ]
                        , span [] (List.map strokeColorSwatchBtn model.colorList)
                        ]
                    ]
                ]
            ]
        ]


dressView : Model -> Html Msg
dressView model =
    div [ class "dress-container" ]
        [ svg
            [ version "1.1"
            , x "0"
            , y "0"
            , viewBox "0 0 150 150"
            ]
            [ polygon
                [ fill model.dress.color
                , points model.dress.points
                , stroke model.dress.strokeColor
                , strokeWidth "0.5"
                ]
                []
            ]
        ]


dressColorSwatchBtn : String -> Html Msg
dressColorSwatchBtn color =
    div
        [ style [ ( "backgroundColor", color ) ]
        , class "swatch "
        , onClick (ChangeDressColor color)
        ]
        []


strokeColorSwatchBtn : String -> Html Msg
strokeColorSwatchBtn color =
    div
        [ style [ ( "border", "6px solid " ++ color ) ]
        , class "swatch "
        , onClick (ChangeStrokeColor color)
        ]
        []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--HELPERS


polyPoint : ( a, b ) -> String
polyPoint ( x, y ) =
    toString x ++ "," ++ toString y ++ " "


adjustPoints : ( number, number1 ) -> ( number, number1 ) -> ( number, number1 )
adjustPoints ( adjX, adjY ) ( x, y ) =
    ( x + adjX, y + adjY )
