module Counter where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import RouteHash exposing (HashUpdate)
import String exposing (toInt)


type alias Model = Int

type Action 
    = Increment 
    | Decrement
    | Set Int

-- This will become an important principle.
type alias Context =
    { actions : Signal.Address Action
    , remove : Signal.Address ()
    }


init: Int -> Model
init count = count

update : Action -> Model -> Model
update action model = 
    case action of 
        Increment -> model + 1
        Decrement -> model - 1
        Set value -> value

view : Signal.Address Action -> Model -> Html
view address model = 
    div [ countContainerStyle ]
        [ button [ onClick address Decrement ] [ text "-" ]
        , div [ countStyle ] [ text (toString model) ]
        , button [ onClick address Increment ] [ text "+" ]
        ]

viewWithRemoveButton : Context -> Model -> Html
viewWithRemoveButton context model =
    div [ countContainerStyle ]
        [ view context.actions model
        , button [ onClick context.remove () ] [ text "Remove" ]
        ]

countContainerStyle : Attribute
countContainerStyle = 
    style 
        [ ("border", "1px solid grey")
        , ("margin", "5px")
        , ("padding", "5px")
        , ("display", "inline-block")
        ]

countStyle : Attribute
countStyle =
    style
        [ ("font-size", "20px")
        , ("font-family", "monospace")
        , ("display", "inline-block")
        , ("width", "50px")
        , ("text-align", "center")
        ]

title : String
title = "Simple Counter"

-- Routing

-- For delta2update, we provide our state as the value for the URL
delta2update : Model -> Model -> Maybe HashUpdate
delta2update previous current =
    Just <|
        RouteHash.set [toString current]


-- For location2action, we generate an action that will restore our state
location2action : List String -> List Action
location2action list =
    case list of
        first :: rest ->
            case toInt first of
                Ok value ->
                    [ Set value ]

                Err _ ->
                    -- If it wasn't an integer, then no action ... we could
                    -- show an error instead, of course.
                    []

        _ ->
            -- If nothing provided for this part of the URL, return empty list 
            []