module LoggingHtml

open System
open System.Windows.Forms

open Tree
open Html

open Logging

[<AutoOpen>]
module private Helpers = 
    let css = IO.File.ReadAllText("Content/logging.css")
    let js = IO.File.ReadAllText("Content/logging.js")

    let logging'class = function
            | Info -> "info"
            | Error -> "error"
            | Warn -> "warn"
            | Debug -> "debug"
    let date'class = "date"
    let time'class = "time"


    let record (date,level,text) = 
        [   span[ %% (DateTime.format "dd/MM" date) ; class' date'class ]
            span[ %% (DateTime.format "HH:mm:ss" date) ; class' time'class ]
            span[ %% text; level |> logging'class |> class' ]
            br ]

    let flip f a b = f b a
    

    let create title = 
        List.map record
        >> List.concat
        >> (flip List.append [ script js ] )
        >> html5 css title    
        >> Seq.toStr "\n" stringify

let set (ie:WebBrowser) title logging = 
    //IO.File.WriteAllText("report.html", create title logging)
    ie.AllowNavigation <- true
    ie.DocumentText <- create title logging

let addRecord (ie:WebBrowser) level (text:string) = 
    let d = ie.Document
    d.InvokeScript
        ("addRecord", 
            [|  DateTime.format "dd/MM" DateTime.Now 
                DateTime.format "HH:mm:ss" DateTime.Now
                logging'class level
                text |] )
    |> ignore
    if  d <> null && d.Body <> null then 
        d.Body.ScrollIntoView(false)