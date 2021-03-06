module Main exposing (..)

import Html exposing (program, Html, div, img, text, textarea, button)
import Html.Attributes exposing (src, value)
import Html.Events exposing (onClick, onInput)
import Date exposing (..)
import Task
import Styles as Styles


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { current : Timeline, timelines: List Timeline }


type alias Timeline =
    { index: Index, text : String, time: Maybe Date, isEdit: Bool }

type alias Index = Int

type Msg
    = EditMsg String
    | Post
    | PostWithTime Date
    | Delete Index
    | Edit Index
    | EditComp Index
    | EditChange Index String


init : ( Model, Cmd Msg )
init =
    ( { current = Timeline 0 "" Nothing False, timelines = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        timeline = model.current
    in
        case msg of
            EditMsg msg ->
                ( { model | current = { timeline | text = msg } }, Cmd.none )
            Post -> 
                ( model, Task.perform PostWithTime Date.now )
            PostWithTime now ->
                let
                    timelineWithTime = { timeline | time = Just now }
                    nextTimeline = { timeline | index = timeline.index + 1, text = "" }
                in
                    ( { model | current = nextTimeline, timelines = timelineWithTime :: model.timelines }, Cmd.none )
            Delete index ->
                ( { model | timelines = List.filter ( \tl -> index /= tl.index ) model.timelines }, Cmd.none )
            Edit index ->
                ( { model | timelines = List.map ( \tl -> if index == tl.index then { tl | isEdit = True } else tl ) model.timelines }, Cmd.none )
            EditComp index ->
                ( { model | timelines = List.map ( \tl -> if index == tl.index then { tl | isEdit = False } else tl ) model.timelines }, Cmd.none )
            EditChange index msg ->
                ( { model | timelines = List.map ( \tl -> if index == tl.index then { tl | text = msg } else tl ) model.timelines }, Cmd.none )


monthNumber: Date -> Int
monthNumber date =
    let
        month = Date.month date
    in
        case month of
            Jan -> 1
            Feb -> 2
            Mar -> 3
            Apr -> 4
            May -> 5
            Jun -> 6
            Jul -> 7
            Aug -> 8
            Sep -> 9
            Oct -> 10
            Nov -> 11
            Dec -> 12
            


formatDate: Date -> String
formatDate date =
    let
        ymd = List.map ( \func -> func date |> toString )
            [ Date.year
            , monthNumber
            , Date.day
            ]
        hms = List.map ( \func -> func date |> toString )
            [ Date.hour
            , Date.minute
            , Date.second
            ]
    in
        String.concat 
            [ String.join "/" ymd
            , " "
            , String.join ":" hms
            ]



view : Model -> Html Msg
view { current, timelines } =
    div [ Styles.mainWrap ]
        ( [ div [ Styles.postForm ]
            [ div [ Styles.formLeft ]
              [ img [ Styles.selfImg, src "http://www.hochi.co.jp/photo/20170718/20170718-OHT1I50084-T.jpg" ] []
              ]
            , div [ Styles.formRight ]
              [ textarea [ Styles.formArea, onInput EditMsg, value current.text ] []
              , button [ Styles.postButton, onClick Post ] [ text "投稿！" ]
              ]
            ]
        ] ++ ( List.map (\tl -> if tl.isEdit then viewEditingTalk tl else createTimeline tl) timelines ) )


-- cf. 編集中はメッセージがtextarea表示になり、変更できるようになります
viewEditingTalk : Timeline -> Html Msg
viewEditingTalk tl =
        div [ Styles.talk ]
            [ div [ Styles.talkLeft ]
                [ img [ Styles.posterImg, src "http://www.hochi.co.jp/photo/20170718/20170718-OHT1I50084-T.jpg" ] [] ]
            , div [ Styles.talkRight ]
                [ div [ Styles.posterName ] [ text "とみざわ" ]
                , textarea [ Styles.editingMessage, onInput ( EditChange tl.index ), value tl.text ] []
                , div [ Styles.talkFooter ]
                    [ text <| Maybe.withDefault "fail" <| Maybe.map formatDate tl.time
                    , div [ Styles.buttons ]
                        [ button [ Styles.editButton, onClick ( EditComp tl.index )  ] [ text "完了" ]
                        , button [ Styles.deleteButton, onClick ( Delete tl.index ) ] [ text "削除" ]
                        ]
                    ]
                ]
            ]


createTimeline: Timeline -> Html Msg
createTimeline tl =
        div [ Styles.talk ]
            [ div [ Styles.talkLeft ]
                [ img [ Styles.posterImg, src "http://www.hochi.co.jp/photo/20170718/20170718-OHT1I50084-T.jpg" ] [] ]
            , div [ Styles.talkRight ]
                [ div [ Styles.posterName ] [ text "とみざわ" ]
                , div [ Styles.message ] [ text tl.text ]
                , div [ Styles.talkFooter ]
                    [ text <| Maybe.withDefault "fail" <| Maybe.map formatDate tl.time
                    , div [ Styles.buttons ]
                        [ button [ Styles.editButton, onClick ( Edit tl.index )  ] [ text "編集" ]
                        , button [ Styles.deleteButton, onClick ( Delete tl.index ) ] [ text "削除" ]
                        ]
                    ]
                ]
            ]
