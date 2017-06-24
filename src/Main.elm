port module Dojo exposing (..)

import Html exposing (text, Html, div, button, pre, input)
import Html.Attributes exposing (style, value, type_)
import Html.Events exposing (onClick, onInput)


type Side
    = Top
    | LeftTop
    | LeftBottom
    | RightTop
    | RightBottom
    | Bottom
    | Middle


type alias NumberDisplay =
    List Side


type alias Model =
    { currentInput : String
    , size : Int
    }


type Msg
    = NoOp
    | ChangeNum String
    | ChangeSize String


init =
    ( { currentInput = "0123456789"
      , size = 2
      }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


numToDisplay : Int -> NumberDisplay
numToDisplay num =
    case num of
        0 ->
            [ Top, LeftTop, LeftBottom, Bottom, RightBottom, RightTop ]

        1 ->
            [ RightTop, RightBottom ]

        2 ->
            [ Top, RightTop, Middle, LeftBottom, Bottom ]

        3 ->
            [ Top, RightTop, Middle, RightBottom, Bottom ]

        4 ->
            [ LeftTop, Middle, RightTop, RightBottom ]

        5 ->
            [ Top, LeftTop, Middle, RightBottom, Bottom ]

        6 ->
            [ Top, LeftTop, Middle, LeftBottom, Bottom, RightBottom ]

        7 ->
            [ Top, RightTop, RightBottom ]

        8 ->
            [ Top, LeftTop, RightTop, Middle, LeftBottom, RightBottom, Bottom ]

        9 ->
            [ Top, LeftTop, RightTop, Middle, RightBottom, Bottom ]

        _ ->
            []


renderTopLine : Int -> Bool -> NumberDisplay -> String
renderTopLine size _ numDisp =
    let
        padStr =
            if List.member Top numDisp then
                "_"
            else
                " "

        padding =
            String.repeat size padStr
    in
        " " ++ padding ++ " "


renderTopBody : Int -> Bool -> NumberDisplay -> String
renderTopBody size last numDisp =
    let
        contentLeft =
            if List.member LeftTop numDisp then
                "|"
            else
                " "

        contentMiddle =
            if (List.member Middle numDisp) && last then
                String.repeat size "_"
            else
                String.repeat size " "

        contentRight =
            if List.member RightTop numDisp then
                "|"
            else
                " "
    in
        contentLeft ++ contentMiddle ++ contentRight


renderBottomBody : Int -> Bool -> NumberDisplay -> String
renderBottomBody size last numDisp =
    let
        contentLeft =
            if List.member LeftBottom numDisp then
                "|"
            else
                " "

        contentMiddle =
            if (List.member Bottom numDisp) && last then
                String.repeat size "_"
            else
                String.repeat size " "

        contentRight =
            if List.member RightBottom numDisp then
                "|"
            else
                " "
    in
        contentLeft ++ contentMiddle ++ contentRight


renderPart : (Int -> Bool -> NumberDisplay -> String) -> Int -> Int -> List Int -> Html Msg
renderPart partRenderer index size numbers =
    let
        renderPartNum : Int -> String -> String
        renderPartNum num output =
            let
                numDisp =
                    numToDisplay num

                content =
                    partRenderer size (size == index) numDisp
            in
                output ++ content

        content =
            List.foldl renderPartNum "" numbers
    in
        div
            []
            [ text content ]


charToInt : Char -> Int
charToInt c =
    case String.toInt <| String.fromChar c of
        Err msg ->
            0

        Ok val ->
            val


renderNumber : Int -> String -> Html Msg
renderNumber size str =
    let
        numDisp =
            numToDisplay 0

        nums =
            String.toList str
                |> List.map charToInt

        r =
            List.range 1 size

        renderTopBodies =
            List.map (\index -> renderPart renderTopBody index size nums) r

        renderBottomBodies =
            List.map (\index -> renderPart renderBottomBody index size nums) r
    in
        pre
            [ style
                [ ( "font-family", "monospace" )
                , ( "font-size", "24px" )
                ]
            ]
        <|
            List.concat
                [ [ renderPart renderTopLine 0 size nums ]
                , renderTopBodies
                , renderBottomBodies
                ]


renderHeader : Model -> Html Msg
renderHeader model =
    div
        []
        [ input
            [ value <| model.currentInput
            , onInput (ChangeNum)
            , type_ "number"
            ]
            []
        , input
            [ value <| toString model.size
            , onInput (ChangeSize)
            , type_ "range"
            , Html.Attributes.min "1"
            , Html.Attributes.max "10"
            ]
            []
        ]


view : Model -> Html Msg
view model =
    div
        []
        [ renderHeader model
        , renderNumber model.size model.currentInput
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeNum str ->
            { model | currentInput = str } ! []

        ChangeSize str ->
            { model
                | size =
                    case String.toInt <| str of
                        Err msg ->
                            0

                        Ok val ->
                            val
            }
                ! []

        NoOp ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []
