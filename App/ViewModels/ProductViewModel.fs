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
    let mutable porog1 : bool option = None
    let mutable porog2 : bool option = None
    let mutable porog3 : bool option = None

    let mutable conc : decimal option = None
    let mutable curr : decimal option = None
    let mutable tens : decimal option = None

    let mutable vporog1 : decimal option = None
    let mutable vporog2 : decimal option = None
    let mutable vporog3 : decimal option = None

    let mutable status : Device.Status option = None

    override x.RaisePropertyChanged propertyName = 
        ViewModelBase.raisePropertyChanged x propertyName

    member x.Write cmd =
        x.Connection <- 
            Device.Cmd.Send x.Addr cmd 
            |> Result.map( fun () -> cmd.What )
            |> Some

    member x.ReadIndication() = 
        let r = Device.readIndication x.Addr
        r |> Result.iter (fun ind -> 
            conc <- Some ind.Conc
            porog1 <- Some ind.P1 
            porog2 <- Some ind.P2
            porog3 <- Some ind.P3        
            x.RaisePropertyChanged "Conc"
            x.RaisePropertyChanged "Porog1"
            x.RaisePropertyChanged "Porog2"
            x.RaisePropertyChanged "Porog3" )
        x.Connection <- 
            r |> Result.map( fun v -> sprintf "C=%M" v.Conc )
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
        r |> Result.iter (fun (p1,p2,p3) -> 
            vporog1 <- Some p1 
            x.RaisePropertyChanged  "VPorog1"
            vporog2 <- Some p2 
            x.RaisePropertyChanged  "VPorog2"
            vporog3 <- Some p3 
            x.RaisePropertyChanged  "VPorog3" )
        x.Connection <- 
            r 
            |> Result.map( sprintf "Уст.пороги %A"  )
            |> Some
        r

    member x.ReadStatus() = 
        let r = Device.readStatus x.Addr
        r |> Result.iter (fun value -> 
            status <- Some value        
            ["StatusMode"; "StatusFailure"; "StatusPorog1"; "StatusPorog2"; "StatusPorog3"]
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

    member x.Porog1 = porog1
    member x.Porog2 = porog2
    member x.Porog3 = porog3

    member x.Conc = fmtDecOpt conc
    member x.Tens = fmtDecOpt tens
    member x.Curr = fmtDecOpt curr

    member x.VPorog1 = fmtDecOpt vporog1
    member x.VPorog2 = fmtDecOpt vporog2
    member x.VPorog3 = fmtDecOpt vporog3
    
    member x.StatusMode     = status |> Option.map( fun {Mode = a} -> a ) 
    member x.StatusFailure  = status |> Option.map( fun {Failure = a} -> a ) 
    member x.StatusPorog1   = status |> Option.map( fun {Porog1 = a} -> a )
    member x.StatusPorog2   = status |> Option.map( fun {Porog2 = a} -> a )
    member x.StatusPorog3   = status |> Option.map( fun {Porog3 = a} -> a )
    

    member x.Product 
        with get () = p
        and set other =
            if p = other then () else
            p <- other
            x.RaisePropertyChanged "Product"
            x.RaisePropertyChanged "What"
            x.RaisePropertyChanged "Serial"

    member x.What = P.what p

    member x.InrerrogateVar = function
        | DevConc -> x.ReadIndication() |> Result.someErr
        | DevCurr -> x.ReadCurrent() |> Result.someErr
        | DevTens -> x.ReadTension() |> Result.someErr
        | DevStatus -> x.ReadStatus() |> Result.someErr