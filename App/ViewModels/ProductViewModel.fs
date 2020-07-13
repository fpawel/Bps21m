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

    let parseSerial serial = 
        let pat = @"^[^№]*№\s*(\d+)[^\d]+(\d+)[^\d]+(\d+)[^$]*$"
        let re = System.Text.RegularExpressions.Regex(pat)
        let xs = re.Match serial
        if xs.Groups.Count = 4 then 
            let pr (n:int) = Int32.Parse xs.Groups.[n].Value
            Ok(pr 1, pr 2, pr 3)
        else 
            Err <| sprintf "%A не соответсвует шаблону %A" serial pat

    let formatSerial serial quarter year = 
        sprintf "Зав.№%d %d г. %d кв." serial year quarter 
    
    

type Product(p : P, getProductType : unit -> ProductType) =
    inherit ViewModelBase()    
    let mutable p = p

    let mutable connection : Result<string,string> option = None
    let mutable productCurrent : decimal option = None
    let mutable stendCurrent : decimal option = None
    
    let mutable productStatus : Hard.Product.Status option = None
    let mutable rele : Hard.Stend.Rele option = None
    
    
    let getProd pt = Map.tryFind pt p.Production

    override x.RaisePropertyChanged propertyName = 
        ViewModelBase.raisePropertyChanged x propertyName

    member x.SwitchOffRS485() =
        let r = Mdbs.switchOffRS485 (appCfg.Comport) p.Addr 
        x.Connection <- 
            r
            |> Result.map( fun () -> "отключение RS-485" )
            |> Some
        r

    member x.WriteProduct (cmd:Hard.Product.Cmd, value) =
        let r = cmd.Perform x.Addr Mdbs.AnswerRequired value
        x.Connection <- 
            r
            |> Result.map( fun () -> cmd.What )
            |> Some
        r

    member x.WriteStend (cmd:Hard.Stend.Cmd) =
        let r = cmd.Perform x.Addr 
        x.Connection <- 
            r
            |> Result.map( fun () -> cmd.What )
            |> Some
        r

    member x.ReadProductCurrent() = 
        let r = Hard.Product.readCurrent x.Addr
        r |> Result.iter (fun v -> 
            productCurrent <- Some v 
            x.RaisePropertyChanged "ProductCurrent" )
        x.Connection <- 
            r |> Result.map( sprintf "I:%M" )
            |> Some
        r

    member x.ReadStendCurrent() = 
        let r = Hard.Stend.readCurrent (int(x.Addr))
        r |> Result.iter (fun value -> 
            stendCurrent <- Some value
            x.RaisePropertyChanged "StendCurrent" )
        x.Connection <- 
            r |> Result.map( sprintf "I:%M:стенд" )
            |> Some
        r

    member x.ReadTensionLoad() = 
        let r = Hard.Stend.readTensionLoad x.Addr (getProductType().ExplosionProtection.R)
        x.Connection <- 
            r |> Result.map( sprintf "U_load: %M: стенд" )
            |> Some
        r

    member x.ReadTensionOpen() = 
        let r = Hard.Stend.readTensionOpenCircuit x.Addr
        x.Connection <- 
            r |> Result.map( sprintf "U_open: %M: стенд" )
            |> Some
        r

    member x.ReadProductStatus() = 
        let r = Hard.Product.readStatus x.Addr
        x.Connection <- 
            r |> Result.map( fun (st, p1, p2, p3) -> 
                productStatus <- Some st
                x.RaisePropertyChanged "ProductStatus"
                sprintf "%s, %b, %b, %b" st.What p1 p2 p3)
            |> Some
        r

    member x.WriteID() = 
        p <- { p with Kind = (getProductType()).What }
        let r = Hard.Product.setID p.Addr p.Kind x.SerialQuarterYear
        x.Connection <- 
            r |> Result.map( fun () ->  
                Logging.warn "%s: %s %s" x.What p.Kind x.SerialQuarterYear
                sprintf "%s %s -->" p.Kind x.SerialQuarterYear)
            |> Some
        r



    member x.ReadID() = 
        let r = Hard.Product.readID p.Addr
        x.Connection <- 
            r |> Result.bind( fun (kind,serial) -> 
                Logging.warn "%s: %s %s" x.What kind serial
                p <- { p with Kind = kind }
                match parseSerial serial with
                | Ok (s,y,q) -> 
                    p <- { p with Serial = s; Year = y; Quarter = q; }
                    x.RaisePropertyChanged "SerialQuarterYear"
                    x.RaisePropertyChanged "Serial"
                    x.RaisePropertyChanged "Quarter"
                    x.RaisePropertyChanged "Year"
                | Err err -> 
                    Logging.warn "%s: %s" x.What err
                Ok "--> ID")
            |> Some
        r

    member x.ReadStendRele() = 
        let r = Hard.Stend.readRele x.Addr
        x.Connection <- 
            r |> Result.map( fun a -> 
                rele <- Some a
                [   "Status"
                    "Failure"
                    "Porog1"
                    "Porog2"
                    "Porog3"
                    "SpMode"
                ] |> List.iter x.RaisePropertyChanged 
                sprintf "%A" a)
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
                
    member x.Addr
        with get () = p.Addr          
        and set v = 
            if v <> p.Addr then
                p <- { p with Addr = v}
                x.RaisePropertyChanged "Addr"

    member x.SerialQuarterYear =
        formatSerial p.Serial p.Quarter p.Year 

    member x.Nameplate =
        x.SerialQuarterYear + " " + x.Kind

    member x.Kind = p.Kind

    member x.Serial
        with get () = p.Serial
        and set v = 
            if v <> p.Serial then
                x.Product <- { p with Serial = v }
                x.RaisePropertyChanged "Serial"
                x.RaisePropertyChanged "SerialQuarterYear"
                
    member x.Year
        with get () = p.Year
        and set v = 
            if v <> p.Year then
                x.Product <- { p with Year = v }
                x.RaisePropertyChanged "Year"
                x.RaisePropertyChanged "SerialQuarterYear"

    member x.Quarter
        with get () = p.Quarter
        and set v = 
            if v <> p.Quarter then
                x.Product <- { p with Quarter = v }
                x.RaisePropertyChanged "Quarter"
                x.RaisePropertyChanged "SerialQuarterYear"

    member x.ProductCurrent = fmtDecOpt productCurrent
    member x.StendCurrent = fmtDecOpt stendCurrent

    member x.ProductStatus = 
        productStatus 
        |> Option.map( fun a -> a.What ) 
        |> Option.withDefault ""
    member x.Porog1 = rele |> Option.map( fun a -> a.Porog1 )  
    member x.Porog2 = rele |> Option.map( fun a -> a.Porog2 )
    member x.Porog3 = rele |> Option.map( fun a -> a.Porog3 ) 
    member x.Status = rele |> Option.map( fun a -> a.Status ) 
    member x.SpMode = rele |> Option.map( fun a -> a.SpMode ) 
    member x.Failure = rele |> Option.map( fun a -> a.Failure )     
    member x.Product 
        with get () = p
        and set n =
            if p = n then () else
            p <- n
            x.RaisePropertyChanged "Product"
            x.RaisePropertyChanged "Serial"
            x.RaisePropertyChanged "Year"
            x.RaisePropertyChanged "Quarter"
            x.RaisePropertyChanged "What"
            x.RaisePropertyChanged "Nameplate"
            
            for pt in Bps21.ProductionPoint.values do
                if p.Production.TryFind pt <> n.Production.TryFind pt then
                    x.RaisePropertyChanged pt.Property 

    member x.What = P.what p

    member x.FailProductionIfConnectionError pt =
        match connection with
        | Some (Err err) -> x.SetProduction pt (false,err)
        | _ -> ()

    member x.SetProduction pt (ok,text) =
        let level = if ok then Logging.Info else Logging.Error 
        let prod = x.Product.Production
        let logLine = DateTime.Now,level,text
        if Map.tryFind pt prod <> Some logLine then
            p <- { p with Production = Map.add pt logLine prod }
            Logging.write level "%s: %s, %s" x.What pt.What text
            x.RaisePropertyChanged pt.Property 

    static member New productType p  = 
        Product(p, productType )     
        

    member x.ProdPtAdjust = getProd Adjust 
    member x.ProdPtLoadCapacity = getProd LoadCapacity 
    member x.ProdPtTestAlarmFailure = getProd TestAlarmFailure 
    member x.ProdPtTestPorog1 = getProd TestPorog1 
    member x.ProdPtTestPorog2 = getProd TestPorog2 
    member x.ProdPtTestPorog3 = getProd TestPorog3 
    member x.ProdPtReservedPower = getProd ReservedPower

    member x.ProdPtTune4M = getProd (Tune ScaleBeg)
    member x.ProdPtTune20M = getProd (Tune ScaleEnd)
    member x.GetProd = getProd 
