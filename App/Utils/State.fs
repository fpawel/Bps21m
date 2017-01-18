[<AutoOpen>]
module State

open System

type State<'state, 'result> =
    State of ('state -> 'result * 'state)

let runState (State f) initialState = f initialState

type StateBuilder() =
    
    member __.Bind( result, restOfComputation  ) =
         State <| fun initialState ->
             let result, updatedState = runState result initialState
             runState (restOfComputation result) updatedState 

    member __.Combine( partOne, partTwo ) =
        State <| fun initialState ->
            let (), updatedState = runState partOne initialState
            runState partTwo updatedState 

    member __.Delay( restOfComputation ) =
         State <| fun initialState ->
            runState ( restOfComputation() ) initialState

    member __.For( elements, forBody  ) =
        State <| fun initialState ->
            let state = ref initialState
            for e in elements do
                let (), updatedState = runState (forBody e) (!state)
                state := updatedState
            (), !state

    member __.Return(x) = State(fun initialState -> x, initialState)
    member __.ReturnFrom(result) = 
        State (runState result)

    member __.Using<'a, 'state, 'b when 'a :> IDisposable>( x : 'a, restOfComputation : 'a -> State<'state, 'b> ) =
        State <| fun initialState ->
            try
                runState (restOfComputation x) initialState
            finally
                x.Dispose()

    member __.TryFinally( tryBlock, finallyBlock ) =
        State <| fun initialState ->
            try
                runState tryBlock initialState
            finally
                finallyBlock()

    member __.TryWith( tryBlock, exnHandler ) = 
        State <| fun initialState -> 
            try
                runState tryBlock initialState 
            with e -> 
                runState (exnHandler e) initialState 

    member __.While( predicate, body ) =
        State <| fun initialState ->
            let state = ref initialState
            while predicate() do 
                let (), updatedState = runState body (!state)
                state := updatedState
            (), !state
    member __.Zero() =
        State(fun initialState -> (), initialState)

let state = StateBuilder()

let getState = State (fun state -> state, state)
let setState newState = State (fun prevState -> (), newState)