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
    type Col = System.Windows.Forms.DataGridViewTextBoxColumn
    

type Product(p : P, getProductType : unit -> ProductType) =
    inherit ViewModelBase()    
    let mutable p = p

    let mutable connection : Result<string,string> option = None
    let mutable productCurrent : decimal option = None
    let mutable stendCurrent : decimal option = None
    
    let mutable productStatus : Hard.Product.Status option = None
    let mutable rele : Hard.Stend.Rele option = None
    
    let productionChangedEvent = Event<Product * ProductionPoint * Logging.Line >()

    let prodPointsColumn = 
        let col = new Col(HeaderText = Bps21.Product.what p)
        MainWindow.gridData.Columns.Add(col) |> ignore
        col

    override x.RaisePropertyChanged propertyName = 
        ViewModelBase.raisePropertyChanged x propertyName

    [<CLIEvent>]
    member private x.OnProductionChanged = productionChangedEvent.Publish

    member private x.ForceUpdateProdPoint pt = 
        let row = MainWindow.ProdPointRow.getRowOfProdPoint pt
        let colIndex = prodPointsColumn.Index
        let cell = row.Cells.[colIndex]
        row.Cells.[colIndex].Value <- Map.tryFind pt p.Production 

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
        let r = Hard.Stend.readCurrent x.Addr
        r |> Result.iter (fun value -> 
            stendCurrent <- Some value
            x.RaisePropertyChanged "StendCurrent" )
        x.Connection <- 
            r |> Result.map( sprintf "I:%M:стенд" )
            |> Some
        r

    member x.ReadTensionLoad() = 
        let r = Hard.Stend.readTensionLoad x.Addr
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
            r |> Result.map( fun (st, p1, p2, p3,_) -> 
                productStatus <- Some st
                x.RaisePropertyChanged "ProductStatus"
                sprintf "%s, %b, %b, %b" st.What p1 p2 p3)
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
                x.RaisePropertyChanged "IsChecked"
                prodPointsColumn.Visible <- v
                
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
        and set other =
            if p = other then () else
            p <- other
            x.RaisePropertyChanged "Product"            
            x.RaisePropertyChanged "Serial"
            x.RaisePropertyChanged "What"
            prodPointsColumn.HeaderText <- x.What

    member x.What = P.what p

    member x.FailProductionIfConnError pt =
        match connection with
        | Some (Err err) -> x.SetProduction pt false err
        | _ -> ()

    member x.SetProduction pt ok text =
        let level = if ok then Logging.Info else Logging.Error 
        let prod = x.Product.Production
        let logLine = DateTime.Now,level,text
        if Map.tryFind pt prod <> Some logLine then
            p <- { p with Production = Map.add pt logLine prod }
            productionChangedEvent.Trigger(x,pt,logLine)
            Logging.write level "%s: %s, %s" x.What pt.What text

    member private x.ProdPointColumn = prodPointsColumn

    member x.Remove() = 
        MainWindow.gridData.Columns.Remove x.ProdPointColumn

    static member New productType p  = 
        let x = Product(p, productType )     
        x.ProdPointColumn.Tag <- x
        for pt in ProductionPoint.values do
            x.ForceUpdateProdPoint pt
        x.OnProductionChanged.Add(fun (_, pt, _) -> 
            x.ForceUpdateProdPoint pt
            )
        x

