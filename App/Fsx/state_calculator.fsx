#load "Utils/State.fs"

let add x = state {
    let! currentTotal, history = getState
    do! setState (currentTotal + x, (sprintf "Added %d" x) :: history) }

let subtract x = state {
    let! currentTotal, history = getState
    do! setState (currentTotal - x, (sprintf "Subtracted %d" x) :: history) }

let multiply x = state {
    let! currentTotal, history = getState
    do! setState (currentTotal * x, (sprintf "Multiplied %d" x) :: history) }

let divide x = state {
    let! currentTotal, history = getState
    do! setState (currentTotal / x, (sprintf "Divided %d" x) :: history) }

let calculatorActions = state {
    do! add 2
    do! multiply 10
    do! divide 5
    do! subtract 8
    return "Finished"
    }
// Теперь выполняем объект StatefulFunc, передав ему начальное состояние
let sfResult, finalState = runState calculatorActions (0, [])