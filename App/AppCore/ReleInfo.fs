
module ReleInfo  
open Html
open Bps21.Hard.Stend

let whrele : (string * (Rele -> bool) )list = 
    [   "Статус",   (fun x -> x.Status)
        "Отказ",    (fun x -> x.Failure)
        "Сп.режим", (fun x -> x.Failure)
        "Порог 1",    (fun x -> x.Porog1)
        "Порог 2",    (fun x -> x.Porog2)
        "Порог 3",    (fun x -> x.Porog3)
    ]

let diffHtml x y =
    
    let tableBody =            
        whrele 
        |> List.map( fun (what,f) -> 
            let (~&&) a =
                text (if f a then "замкнуто" else "разомкнуто" )
            let ok = f x = f y
            
            let styleColor = 
                (if not ok then "red" else "navy")
                |> sprintf "color:%s;"
                |> style
                 
            [   styleColor
                td [text what]
                td [ && x ]
                td [ && y] 
            ] |> tr  )
        |> tbody 
    let tableHead =
        [   th [ %% "Пин" ]
            th [ %% "Состояние" ]
            th [ %% "Длжно быть" ]
        ] |> thead

    table
        [   class' "table-bordered"
            tableHead
            tableBody
        ]    
    |> stringify