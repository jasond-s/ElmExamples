module CounterList where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RouteHash exposing (HashUpdate)
import Result.Extra
import String

import Counter exposing (..)


type alias Model =
    { counters : List (ID, Counter.Model) 
    , nextID : ID
    }

type alias ID = Int

type Action 
    = Insert
    | Remove ID
    | Modify ID Counter.Action
    | Set (List Int)



init : Model
init =
    { counters = []
    , nextID = 0
    }               


update : Action -> Model -> Model
update action model =
  case action of
    Insert ->
        let newCounter = ( model.nextID, Counter.init 0 )
            newCounters = model.counters ++ [ newCounter ]
        in
            { model |
                counters = newCounters,
                nextID = model.nextID + 1
            }

    Remove id ->
        { model |
            -- This is a an anonymous function prefixed with '\'.
            counters = List.filter (\(counterID, _) -> counterID /= id) model.counters
        }

    Modify id counterAction ->
        let updateCounter (counterID, counterModel) =
            -- This is out predicate for the map filter.
            if counterID == id
                then (counterID, Counter.update counterAction counterModel)
                else (counterID, counterModel)
        in
            { model | counters = List.map updateCounter model.counters }

    Set list ->
        let
            counters =
                List.indexedMap (\index item ->
                    (index, Counter.init item)
                ) list

        in
            { counters = counters
            , nextID = List.length counters
            }


view : Signal.Address Action -> Model -> Html
view address model =
    let insert = button [ onClick address Insert ] [ text "Add" ]
    in
        -- This is equivolent to ("head" :: ["tail"] )
        div [] (insert :: List.map (viewCounter address) model.counters)


viewCounter : Signal.Address Action -> (ID, Counter.Model) -> Html
viewCounter address (id, model) =
    let context =
        Counter.Context
            (Signal.forwardTo address (Modify id))
            (Signal.forwardTo address (always (Remove id)))
    in
        div [] 
            [ Counter.viewWithRemoveButton context model ]

title : String
title = "List of Counters"

delta2update : Model -> Model -> Maybe HashUpdate
delta2update previous current =
    List.map (toString << snd) current.counters
        |> RouteHash.set
        |> Just


location2action : List String -> List Action
location2action list =
    let
        result =
            List.map String.toInt list
                |> Result.Extra.combine

    in
        case result of
            Ok ints ->
                [ Set ints ]

            Err _ ->
                []