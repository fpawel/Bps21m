module Ref

open System

type Observable<'a>(value:'a) =
    let mutable value = value
    let mutable changed'handlers = []
    
    member __.AddChangedHandler f =        
        let f = Action<'a * 'a>(f)
        changed'handlers <-  f::changed'handlers
        fun () -> 
            changed'handlers <- List.filter ((<>) f) changed'handlers

    member x.AddChanged f =
        x.AddChangedHandler f |> ignore

    member __.Set v = 
        changed'handlers |> List.iter( fun f -> f.Invoke(value,v) )
        value <- v
    member __.Get () = value 

    member x.Value 
        with get() = x.Get()
        and set v = x.Set v

[<AutoOpen>]
module private Helpers =
    let mutable ``not initialized referencies`` = Map.empty
    
type Initializable<'a>(what:string) =
    let what = sprintf "%s:%s" what (typeof<'a>.Name)
    let k = String.getUniqueKey 50
    let mutable (value' : 'a option) = None
    do
        ``not initialized referencies`` <- Map.add k what ``not initialized referencies``

    member __.Value 
        with get() =             
            match value' with
            | Some (value : 'a) -> value
            | _ -> failwithf "InitializableRef %A is not initialized!" what
        and set (value : 'a)  = 
            match value' with
            | Some _ -> failwithf "InitializableRef %A is alredy initialized!" what
            | _ -> 
                value' <- Some value
                ``not initialized referencies`` <- Map.remove k ``not initialized referencies``

let ``check referencies was initialized``() = 
    let xs = ``not initialized referencies`` 
    if Map.isEmpty xs then () else
    ``not initialized referencies`` 
    |> Map.toList 
    |> List.map snd
    |> Seq.toStr "\n" id
    |> failwithf "not initilized refs found: \n%s"