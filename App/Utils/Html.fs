module Html

open Tree

type H =
    | Attr of string * string
    | Tag of string
    | Text of string


let (<!) attr value = { RootLabel = Attr (attr,value); SubForest = [] }  

let (<<<) x items = { RootLabel = x; SubForest = items }
let (<==) (tag) items = Tag tag <<< items

let (~%%) s = { RootLabel = Text s; SubForest = [] }


[<AutoOpen>]
module private Helpers = 
    open System
    let tabs n = String(' ',n)
    let ln n x = sprintf "%s%s" (tabs n) x
    
let tag tag items ={   RootLabel = Tag (tag); SubForest = items }

let id' x = "id" <! x
let class' x = "class" <! x
let colspan n = "colspan" <! sprintf "%d" n
let rowspan n = "rowspan" <! sprintf "%d" n
let valign s = "valign" <! s


let script x = tag "script" [ "type" <! "text/javascript"; %% x]
let h1 = tag "h1"
let h2 = tag "h2"
let h3 = tag "h3"
let h4 = tag "h4"
let p = tag "p"
let div = tag "div"
let span = tag "span"
let br = %% "<br />"


let href = id' "href"
let table = tag "table"
let th = tag "th"
let tr = tag "tr"
let td = tag "td"
let tbody = tag "tbody"
let thead = tag "thead"
let caption = tag "caption"
let sub = tag "sub"


let header = tag "header"
let nav = tag "nav"
let section = tag "section"

let ul = tag "ul"
let li = tag "li"

let stringify =
    let rec loop n = function
        | { RootLabel = Attr (attr,value) } -> sprintf "%s=\"%s\"" attr value
        | { RootLabel = Text value } -> value
        | { RootLabel = Tag tag; SubForest = [] } ->
            sprintf "<%s></%s>" tag tag
        | { RootLabel = Tag tag; SubForest = items } ->
            let attrs, inner =
                items
                |> List.partition( function { RootLabel = Attr _ } -> true | _ -> false )
            let part1 =
                attrs
                |> Seq.toStr ( " ") (loop (n+1))
                |> sprintf "<%s %s>" tag
            let part2 = inner |> Seq.toStr "\n" ( fun x -> sprintf "%s%s" (tabs n) (loop (n+1) x))
            [   yield part1
                yield part2 ]
            |> Seq.toStr "\n" id
            |> fun x -> sprintf "%s\n%s</%s>" x (tabs n) tag
    loop 0

let html5 css title content =
        [   %% "<!DOCTYPE html>"
            tag "html" [
                tag "head"[
                    tag "meta" [ 
                        "charset" <! "UTF-8" 
                        "http-equiv" <! "X-UA-Compatible" 
                        "content"  <! "IE=9" 
                        ]
                    tag "title" [ %% title]
                    tag "style" [ %% css ] ]
                tag "body" content]]

