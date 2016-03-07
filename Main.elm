import Effects exposing (Never)
import StartApp exposing (App)
import Task exposing (Task)
import Html exposing (Html) 
import RouteHash

import CounterApp exposing (Model, Action(NoOp), init, update, view)


app : App Model
app =
    StartApp.start
        { init = init
        , update = update
        , view = view
        , inputs = [ messages.signal ]
        }


messages : Signal.Mailbox Action
messages =
    Signal.mailbox NoOp


main : Signal Html
main =
    app.html


port tasks : Signal (Task Never ())
port tasks =
    app.tasks


port routeTasks : Signal (Task () ())
port routeTasks =
    RouteHash.start
        { prefix = RouteHash.defaultPrefix
        , address = messages.address
        , models = app.model
        , delta2update = CounterApp.delta2update
        , location2action = CounterApp.location2action
        }