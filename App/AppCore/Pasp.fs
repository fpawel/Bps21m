module Bps21.Pasp

open System

open Tree
open Html

[<AutoOpen>]
module private Helpers = 
    let pasportBlock xs = 
        td ( (class' "pasport-block")::xs  )

    let value' = class' "value"

let private product ((h,d):Party.Content) (p:Product) =
    let t = h.ProductType
 

    let trs = [   
        tr[
            td [%% "Заводской номер:"]
            td [value'; %% p.Serial ]
        ]
        


    ]

    [   div [ class' "header1"; %% "Паспорт"]
        div [ 
            class' "product-type"
            span [ 
                class' "product-type-1"
                %% t.What
            ] 
        ]
        table [
            tbody trs 
        ]
        div[
            %% "Подпись: _____________________" 
        ]
    ]
    

let private css = 
        IO.File.ReadAllText("content\\report.css")

let party ((h,d):Party.Content as party) = 
    let product = product party
    let trs = 
        List.window 2 d.Products 
        |> List.map( function
            | [p1;p2] -> [p1;p2] |> List.map (product >> pasportBlock)
            | [p1] -> [ pasportBlock <| (colspan 2)::(product p1)  ]
            | _ -> [] )
        |> List.map tr
    [ table [ tbody trs ] ]
    |> html5 css "Индивидуальные паспорта МИЛ-82"
    |> Seq.toStr "\n" stringify