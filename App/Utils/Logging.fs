module Logging

open System
open System.IO
open System.Drawing
open System.Diagnostics

type Level = 
    | Error
    | Warn
    | Info
    | Debug
    static member toString (x:Level) = 
        (sprintf "%A" x).ToUpper()

//let resultToLevel<'a,'e> = function
//    | Ok (_ : 'a) -> Info 
//    | Err (_ : 'e) -> Error


let addLogger, getLoggers = 

    let mutable loggers = []
    let addLogger logger = 
        let k = String.getUniqueKey 10
        loggers <- (k,logger) :: loggers
        fun () -> 
            loggers <- List.filter( fst >> ((<>) k)  ) loggers
   
    let flevel = Level.toString
    let path,_,_,_ = IO.Path.ofDateTime "Log" 
    let stream = 
        let x = new StreamWriter(Path.Combine(path true DateTime.Now, "log.log"), true)
        x.AutoFlush <- true
        x

    let now() = DateTime.toString DateTime.Now

    addLogger (fun l -> 
        fprintfn stream "%s|%s|%s" (now()) (flevel l) ) |> ignore
    addLogger (fun l -> 
        printfn "%s|%s|%s" (now()) (flevel l) ) |> ignore


    addLogger, (fun () -> loggers)
 
let private write1 level (message : string) =
    getLoggers() |> List.map snd |> List.iter( fun f -> f level message)     
        
let write level format =
    Printf.kprintf (write1 level ) format 

let info f = write Info f
let warn f = write Warn f
let error f = write Error f
let debug f = write Debug f

let foreColor = function
    | Error -> Color.Red 
    | Info -> Color.Navy
    | Debug -> Color.Gray
    | Warn -> Color.Maroon

let backColor = function
    | Error -> Color.LightGray
    | Info -> Color.Azure
    | Debug -> Color.White
    | Warn -> Color.White


type Line = DateTime * Level * string

type Lines = Line list