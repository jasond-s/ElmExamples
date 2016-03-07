module CounterApp where

import Effects exposing (Effects, map, batch, Never)
import Html exposing (Html, div, p, text, table, tr, td)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Signal exposing (forwardTo)
import RouteHash exposing (HashUpdate)


import Counter as Example1
import CounterList as Example2


-- Cool little infix for building tuples
(=>) = (,)


-- MODEL

type Example
    = Example1
    | Example2

type alias Model =
    { example1 : Example1.Model
    , example2 : Example2.Model
    , currentExample : Example
    }


init : (Model, Effects Action)
init =
    let
        model =
            { example1 = Example1.init 0
            , example2 = Example2.init 
            , currentExample = Example1
            }                

        effects =
            batch
                [ ]
    in
        (model, effects)


-- UPDATE

type Action
    = Example1Action Example1.Action
    | Example2Action Example2.Action
    | ShowExample Example
    | NoOp


update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        NoOp ->
            (model, Effects.none)

        ShowExample example ->
            ({ model | currentExample = example }, Effects.none)

        Example1Action subaction ->
            ({ model | example1 = Example1.update subaction model.example1 }, Effects.none)

        Example2Action subaction ->
            ({ model | example2 = Example2.update subaction model.example2 }, Effects.none)


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    let
        viewExample =
            case model.currentExample of
                Example1 ->
                    Example1.view (forwardTo address Example1Action) model.example1

                Example2 ->
                    Example2.view (forwardTo address Example2Action) model.example2

        makeTitle (index, example, title) =
            let
                styleList =
                    if example == model.currentExample
                        then
                            [ "font-weight" => "bold"
                            ]
                        else
                            [ "font-weight" => "normal"
                            , "color" => "blue"
                            , "cursor" => "pointer"
                            ]

                fullTitle =
                    text <|
                        "Example " ++ (toString index) ++ ": " ++ title

                clickAction =
                    if example == model.currentExample
                        then []
                        else [ onClick address (ShowExample example) ] 

            in
                p   ( style styleList :: clickAction ) 
                    [ fullTitle ]

        toc =
            div [] <|
                List.map makeTitle
                    [ ( 1, Example1, Example1.title )
                    , ( 2, Example2, Example2.title )
                    ]

    in
        table []
            [ tr []
                [ td 
                    [ style
                        [ "vertical-align" => "top"
                        , "width" => "25%"
                        , "padding" => "8px"
                        , "margin" => "8px"
                        ]
                    ]
                    [ toc ]
                , td
                    [ style
                        [ "vertical-align" => "top"
                        , "width" => "75%"
                        , "padding" => "8px"
                        , "margin" => "8px"
                        , "border" => "1px dotted black"
                        ]
                    ]
                    [ viewExample ]
                ]
            ]


delta2update : Model -> Model -> Maybe HashUpdate
delta2update previous current =
    case current.currentExample of
        Example1 ->
            RouteHash.map ((::) "example-1") <|
                Example1.delta2update previous.example1 current.example1

        Example2 ->
            RouteHash.map ((::) "example-2") <|
                Example2.delta2update previous.example2 current.example2


location2action : List String -> List Action
location2action list =
    case list of
        "example-1" :: rest ->
            ( ShowExample Example1 ) :: List.map Example1Action ( Example1.location2action rest )
        
        "example-2" :: rest ->
            ( ShowExample Example2 ) :: List.map Example2Action ( Example2.location2action rest )

        _ ->
            [ ShowExample Example1 ]
