namespace  Bps21.ViewModel

open System
open System.Text.RegularExpressions

open Bps21

[<AutoOpen>]
module private ViewModelProductHelpers =
    type P = Bps21.Product
    let appCfg = AppConfig.config

    let formatPorogSt = function
        | Some true -> "вкл." 
        | Some false -> "выкл." 
        | _ -> ""

    let fmtDecOpt = Option.map Decimal.toStr6 >> Option.withDefault ""
    

type Product(p : P, getProductType : unit -> ProductType) =
    inherit ViewModelBase()    
    let mutable p = p

    let mutable connection : Result<string,string> option = None
    let mutable conc : decimal option = None
    let mutable curr : decimal option = None
    let mutable tens : decimal option = None
    
    let mutable status : Device.Status option = None

    override x.RaisePropertyChanged propertyName = 
        ViewModelBase.raisePropertyChanged x propertyName

    member x.Write cmd =
        x.Connection <- 
            Device.Cmd.Send x.Addr cmd 
            |> Result.map( fun () -> cmd.What )
            |> Some

    member x.ReadConc() = 
        let r = Device.readConc x.Addr
        r |> Result.iter (fun v -> 
            conc <- Some v.Value
            x.RaisePropertyChanged "Conc" )
        x.Connection <- 
            r |> Result.map( fun v -> sprintf "C=%M" v.Value )
            |> Some
        r

    member x.ReadCurrent() = 
        let r = Device.readCurrent x.Addr
        r |> Result.iter (fun value -> 
            curr <- Some value
            x.RaisePropertyChanged "Curr" )
        x.Connection <- 
            r |> Result.map( sprintf "I=%M" )
            |> Some
        r

    member x.ReadTension() = 
        let r = Device.readCurrent x.Addr
        r |> Result.iter (fun value -> 
            curr <- Some value
            x.RaisePropertyChanged "Tens" )
        x.Connection <- 
            r |> Result.map( sprintf "U=%M" )
            |> Some
        r

    member x.ReadPorogs() = 
        let r = Device.readVPorogs x.Addr
        x.Connection <- 
            r 
            |> Result.map( sprintf "Уст.пороги %A"  )
            |> Some
        r

    member x.ReadStatus() = 
        let r = Device.readStatus x.Addr
        r |> Result.iter (fun value -> 
            status <- Some value        
            ["Mode"; "Failure"; "Porog1"; "Porog2"; "Porog3"]
            |> List.iter x.RaisePropertyChanged )
        x.Connection <- 
            r |> Result.map( sprintf "Статус %A" )
            |> Some
        r

    member x.Connection
        with get () = connection
        and set v = 
            if v <> connection then
                connection <- v
                x.RaisePropertyChanged "Connection"
                
    member x.IsChecked 
        with get () = p.IsChecked          
        and set v = 
            if v <> p.IsChecked then
                p <- { p with IsChecked = v}
                x.RaisePropertyChanged "IsChecked"
                
    member x.Addr
        with get () = p.Addr          
        and set v = 
            if v <> p.Addr then
                p <- { p with Addr = v}
                x.RaisePropertyChanged "Addr"
                x.RaisePropertyChanged "What"
    
    member x.Serial
        with get () = p.Serial
        and set v = 
            if v <> p.Serial then
                x.Product <- { p with Serial = v }

    member x.Conc = fmtDecOpt conc
    member x.Tens = fmtDecOpt tens
    member x.Curr = fmtDecOpt curr

    member x.Mode     = status |> Option.map( fun {Mode = a} -> a ) 
    member x.Failure  = status |> Option.map( fun {Failure = a} -> a ) 
    member x.Porog1   = status |> Option.map( fun {Porog1 = a} -> a )
    member x.Porog2   = status |> Option.map( fun {Porog2 = a} -> a )
    member x.Porog3   = status |> Option.map( fun {Porog3 = a} -> a )
    

    member x.Product 
        with get () = p
        and set other =
            if p = other then () else
            p <- other
            x.RaisePropertyChanged "Product"
            x.RaisePropertyChanged "What"
            x.RaisePropertyChanged "Serial"

    member x.What = P.what p

    