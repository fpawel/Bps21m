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

    member x.Porog1 = formatPorogSt porog1
    member x.Porog2 = formatPorogSt porog2
    member x.Porog3 = formatPorogSt porog2

    member x.setPorog porog value =         
        let f,s  = 
            match porog with
            | Bps21.Th1 -> (fun () -> porog1 <- value), "Porog1"
            | Bps21.Th2 -> (fun () -> porog2 <- value), "Porog2"
            | Bps21.Th3 -> (fun () -> porog3 <- value), "Porog3"
        f()
        x.RaisePropertyChanged s        

    member x.Product 
        with get () = p
        and set other =
            if p = other then () else
            p <- other
            x.RaisePropertyChanged "Product"
            x.RaisePropertyChanged "What"
            x.RaisePropertyChanged "Serial"

    member x.What = P.what p

    
    

   