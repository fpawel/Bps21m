module Json.Config

open System
open System.IO

open Json.Serialization


    


let private error<'a> (filename, createNewFunc  : unit -> 'a) errorText =    
    createNewFunc(), Some (sprintf "ошибка файла конфигурации %A: %s" filename errorText ) 

let read filename createNewFunc =    
    let path =  IO.Path.Combine( IO.Path.ofExe, filename)
    let error = error (filename, createNewFunc)
    if IO.File.Exists path then 
        try
            match parse (IO.File.ReadAllText(path)) with
            | Ok x -> x, None
            | Err x -> error x
        with e -> error <| sprintf "%A" e
    else
        createNewFunc(), None

let write filename x' = 
    let path =  IO.Path.Combine( IO.Path.ofExe, filename)
    let x = stringify x'
    let r =
        try
            File.WriteAllText(filename, x)
            Ok ()
        with e ->
            Err <| sprintf "oшибка записи файла конфигурации json %A: %A" filename e 

    match r with
    | Err e -> Logging.error "%s" e
    | _ -> ()

let create filename dummy =         
    let config,error = read filename dummy
    let save() = write filename config
    config, error, save