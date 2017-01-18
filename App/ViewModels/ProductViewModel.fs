namespace  Bps21.ViewModel

open System
open System.Text.RegularExpressions

open Bps21

[<AutoOpen>]
module private ViewModelProductHelpers =
    type P = Bps21.Product
    let appCfg = AppConfig.config
    

type Product(p : P, getProductType : unit -> ProductType) =
    inherit ViewModelBase()    
    let mutable p = p

    let mutable connection : Result<string,string> option = None
    
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

    member x.Product 
        with get () = p
        and set other =
            if p = other then () else
            p <- other
            x.RaisePropertyChanged "Product"
            x.RaisePropertyChanged "What"
            x.RaisePropertyChanged "Serial"

    member x.What = P.what p

    
    member x.GetModbusResponse request formatResult parseResponse = 
        let r = 
            Mdbs.getResponse
                appCfg.Hardware.ComportProducts request formatResult parseResponse
        x.Connection <- 
            r 
            |> Result.map(fun v -> sprintf "%s : %s" request.what (formatResult v))
            |> Some 
        r

   