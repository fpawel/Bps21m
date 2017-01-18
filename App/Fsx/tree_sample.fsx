
type 'a Tree = { RootLabel:'a
                 SubForest:'a Forest }
and 'a Forest = 'a Tree list


type 'a TreeLoc = { Tree : 'a Tree
                    Lefts : 'a Forest
                    Rights : 'a Forest
                    Parents : ('a Forest * 'a * 'a Forest) list }

// combChildren : 'a list -> 'a -> 'a list -> 'a list
let private combChildren ls t rs = List.fold ( fun xs x -> x::xs ) (t::rs) ls

// downParents : 'a TreeLoc -> ('a Forest * 'a * 'a Forest) list
let private downParents loc = (loc.Lefts, (loc.Tree).RootLabel, loc.Rights) :: loc.Parents

let split<'a> : 'a list -> int -> ('a list * 'a list) option =
    let rec loop acc xs n =
        match (acc, xs, n) with
        | (acc, xs, 0) -> Some (acc, xs)
        | (acc, x::xs, n) -> loop (x::acc) xs (n - 1)
        | _ -> None
    loop []

// top' : 'a TreeLoc -> 'a TreeLoc option
let topM = function
    | { Parents = (pls, v, prs) :: ps } as loc ->
        {   Tree =
                {   RootLabel = v
                    SubForest = combChildren (loc.Lefts) (loc.Tree) (loc.Rights) }
            Lefts = pls
            Rights = prs
            Parents = ps } |> Some
    | _ -> None


// left' : 'a TreeLoc -> 'a TreeLoc option
let leftM = function
    | { Lefts = t::ts } as loc ->
        { loc with
            Tree = t
            Lefts = ts
            Rights = loc.Tree :: loc.Rights } |> Some
    | _ -> None

// right' : 'a TreeLoc -> 'a TreeLoc option
let rightM = function
    | { Rights = t::ts } as loc ->
        { loc with
            Tree = t
            Lefts = loc.Tree :: loc.Lefts
            Rights = ts } |> Some
    | _ -> None

// down'first' : 'a TreeLoc -> 'a TreeLoc option
let downFirstM = function
    | { Tree = { SubForest = t::ts } } as loc ->
        {   Tree = t
            Lefts = []
            Rights = ts
            Parents = downParents loc }
        |> Some
    | _ -> None

let private (|Rev|) = List.rev

// down'first' : 'a TreeLoc -> 'a TreeLoc option
let downLastM = function
    | { Tree = { SubForest = Rev( t::ts ) } } as loc ->
        {   Tree = t
            Lefts = ts
            Rights = []
            Parents = downParents loc }
        |> Some
    | _ -> None

// down'nth' : int -> 'a TreeLoc -> 'a TreeLoc option
let downAtM n loc =
    split ((loc.Tree).SubForest) n
    |> Option.map (fun (t::ls, rs) ->
         {  Tree = t
            Lefts = ls
            Rights = rs
            Parents = downParents loc })

let (|Top|_|) = topM
let (|Left|_|) = leftM
let (|Right|_|) = rightM
let (|DownFirst|_|) = downFirstM
let (|DownLast|_|) = downLastM
let (|DownAt|_|) = downAtM

// 'a TreeLoc -> 'a TreeLoc
let top x = topM x |> Option.get
let left x = leftM x |> Option.get
let right x = rightM x |> Option.get
let downFirst x = downFirstM x |> Option.get
let downLast x = downLastM x |> Option.get
let downAt n x = downAtM n x |> Option.get

// root : 'a TreeLoc -> 'a TreeLoc
let rec root = function
    | Top x -> root x
    | x -> x

// find : ('a Tree -> bool) -> 'a TreeLoc -> 'a TreeLoc option
let find p loc =
    let rec split acc xs =
        match xs with
        | x::xs when p x -> Some(acc, x, xs)
        | x::xs -> split (x::acc) xs
        | []-> None
    split [] ((loc.Tree).SubForest)
    |> Option.map (fun (ls, t, rs) -> { Tree = t
                                        Lefts = ls
                                        Rights = rs
                                        Parents = downParents loc })

// from'tree : 'a Tree -> 'a TreeLoc
let fromTree t = { Tree = t
                   Lefts = []
                   Rights = []
                   Parents = [] }

// fromForest : 'a Forest -> 'a TreeLoc option
let fromForest ( (t::ts) : 'a Forest) =
    {   Tree = t
        Lefts = []
        Rights = ts
        Parents = [] }

// toTree : 'a TreeLoc -> 'a Tree
let toTree loc = (root loc).Tree

// toForest : 'a TreeLoc -> 'a Forest
let toForest loc : 'a Forest =
    let r = root loc in combChildren (r.Lefts) (r.Tree) (r.Rights)

// isRoot : 'a TreeLoc -> bool
let isRoot loc = List.isEmpty (loc.Parents)

// isFirst : 'a TreeLoc -> bool
let isFirst loc = List.isEmpty (loc.Lefts)

// isLast : 'a TreeLoc -> bool
let isLast loc = List.isEmpty (loc.Rights)

// isLeaf : 'a TreeLoc -> bool
let isLeaf loc = List.isEmpty ((loc.Tree).SubForest)

// isChild : 'a TreeLoc -> bool
let isChild loc = not (isRoot loc)

// hasChildren : 'a TreeLoc -> bool
let hasChildren loc = not (isLeaf loc)

// setTree : 'a Tree -> 'a TreeLoc -> 'a TreeLoc
let setTree t loc = { loc with Tree = t }

// modifyTree : ('a Tree -> 'a Tree) -> 'a TreeLoc -> 'a TreeLoc
let modifyTree f loc = setTree (f (loc.Tree)) loc

// setLabel : 'a -> 'a TreeLoc -> 'a TreeLoc
let setLabel v loc = modifyTree (fun t -> { t with RootLabel = v }) loc

// getLabel : 'a TreeLoc -> 'a
let getLabel loc = (loc.Tree).RootLabel

// modifyLabel : ('a -> 'a) -> 'a TreeLoc -> 'a TreeLoc
let modifyLabel f loc = setLabel (f (getLabel loc)) loc

module Insert = 

    // insertLeft : 'a Tree -> 'a TreeLoc -> 'a TreeLoc
    let left t loc = 
        { loc with Tree = t
                   Rights = loc.Tree :: loc.Rights }

    // insertRight : 'a Tree -> 'a TreeLoc -> 'a TreeLoc
    let right t loc = 
        { loc with Tree = t
                   Lefts = loc.Tree :: loc.Lefts }

    // insertDownFirst : 'a Tree -> 'a TreeLoc -> 'a TreeLoc
    let downFirst t loc = 
        { loc with Tree = t
                   Lefts = []
                   Rights = (loc.Tree).SubForest
                   Parents = downParents loc }

    // insertDownLast : 'a Tree -> 'a TreeLoc -> 'a TreeLoc
    let downLast t loc = 
        { loc with Tree = t
                   Lefts = List.rev ((loc.Tree).SubForest)
                   Rights = []
                   Parents = downParents loc }

    // insertDownAt : int -> 'a Tree -> 'a TreeLoc -> 'a TreeLoc option
    let downAt n t loc =
        split ((loc.Tree).SubForest) n
        |> Option.map (fun (ls, rs) -> 
            { loc with Tree = t
                       Lefts = ls
                       Rights = rs
                       Parents = downParents loc })

type Html =
    | HAttr of string * string
    | HTag of string
    | HText of string


let (<!) attr value = { RootLabel = HAttr (attr,value); SubForest = [] }  

let (<<<) x items = { RootLabel = x; SubForest = items }
let (<==) (tag) items = HTag tag <<< items

let (~%%) s = { RootLabel = HText s; SubForest = [] }


let body = HTag "body"
let div = HTag "div"
let p = HTag "p"
let span = HTag "span"

let Id x = "id" <! x

let sample3 =
    body <<< [
        div <<< [
            %% "some text 1"
            p <<< [ "width" <! "1" ; "height" <! "12"; "height" <! "15" ]
            p <<< [ "height" <! "1";  "height" <! "12"; "height" <! "15" ]
            %% "some text 2" ] ]

open System
let tabs n = String('\t',n)
//let ln n x = sprintf "%s%s" (tabs n) x

let Seq.toStr<'T> delimString conv (collection : 'T seq )  =
    collection |> Seq.fold( fun acc x ->
        acc + (if acc |> String.IsNullOrEmpty then acc else delimString) + (conv x) ) ""

let stringify =
    let rec loop n = function
        | { RootLabel = HAttr (attr,value) } -> sprintf "%s=\"%s\"" attr value
        | { RootLabel = HText value } -> value
        | { RootLabel = HTag tag; SubForest = items } ->
            let attrs, inner =
                items
                |> List.partition( function { RootLabel = HAttr _ } -> true | _ -> false )
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
open System.IO


let tag tag items =
    {   RootLabel = HTag (tag)
        SubForest = items }

let html5doc content =
    [   %% "<!DOCTYPE html>"
        tag "html"[
            tag "head"[
                tag "meta" [ "charset" <! "UTF-8" ]
                tag "title" [ %% "Title of the document"]]
            tag "body" content]]

let samp4 =
    [   div <<< [ Id "div1"; %% "text1" ]
        div <<< [ Id "div2"; %% "text2" ]
        div <<< [ Id "div3"; %% "text3" ]
        %% "text content of the doc" ]
    |>  html5doc

let samp5 = 
    samp4 
    |> fromForest 
    |> right
    |> downAt 2
    |> downAt 3
    |> Insert.downFirst (p <<< [ "id" <! "p1"; span <<< [%% "span content"] ] )
    |> root
    |> toForest


File.WriteAllText("c:\\1.html", samp5 |> Seq.toStr "\n" stringify )
