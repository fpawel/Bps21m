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
    
    override x.RaisePropertyChanged propertyName = 
        ViewModelBase.raisePropertyChanged x propertyName

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
    member x.Porog3 = porog2

    member x.Conc = conc |> Option.map Decimal.toStr6 |> Option.withDefault ""
    member x.Tens = tens |> Option.map Decimal.toStr6 |> Option.withDefault ""
    member x.Curr = curr |> Option.map Decimal.toStr6 |> Option.withDefault ""

    member x.SetIndication (ind:Device.Indication) =  
        conc <- Some ind.Conc
        porog1 <- Some ind.P1 
        porog2 <- Some ind.P2
        porog3 <- Some ind.P3        
        x.RaisePropertyChanged "Conc"
        x.RaisePropertyChanged "Porog1"
        x.RaisePropertyChanged "Porog2"
        x.RaisePropertyChanged "Porog3"

    member x.SetCurr value = 
        curr <- Some value
        x.RaisePropertyChanged "Curr"

    member x.SetTens value = 
        tens <- Some value
        x.RaisePropertyChanged "Tens"

    member x.Product 
        with get () = p
        and set other =
            if p = other then () else
            p <- other
            x.RaisePropertyChanged "Product"
            x.RaisePropertyChanged "What"
            x.RaisePropertyChanged "Serial"

    member x.What = P.what p

    
    

   