namespace Bps21.ViewModel

open System
open System.ComponentModel
open System.Collections.Generic

open Bps21

[<AutoOpen>]
module private ViewModelPartyHelpers =

    type PartyPath = Repository.PartyPath

    type P = Bps21.ViewModel.Product
    type Col = System.Windows.Forms.DataGridViewTextBoxColumn
    type Cols = System.Windows.Forms.DataGridViewColumnCollection

    let createProductViewModel productType p  = 

        let col = new Col(HeaderText = Bps21.Product.what p, Visible = p.IsChecked)
        let x = ViewModel.Product(p, productType )     
        Runtime.PropertyChanged.add x <| fun e ->
            match e.PropertyName with
            | "IsChecked" -> col.Visible <- x.IsChecked
            | "What" -> col.HeaderText <- x.What
            | _ -> ()
        col.Tag <- x
        x


type ProductTypesConverter() = 
    inherit StringConverter()
    override this.GetStandardValuesSupported _ = true
    override this.GetStandardValuesExclusive _ = true
    override this.GetStandardValues _ =       
        ProductType.values
        |> Seq.toArray
        |> Array.map( fun x -> x.What)
        |> TypeConverter.StandardValuesCollection
        

type Party
        (   partyHeader:Bps21.Party.Head, 
            partyData : Bps21.Party.Data ) =
    inherit ViewModelBase() 

    let mutable partyHeader = partyHeader
    let mutable partyData = partyData
    let productType() = partyHeader.ProductType 
    
    let products, setProducts = 
        let x = BindingList<P>()
        let setProducts xs = 
            x.Clear()
            xs |> List.map (createProductViewModel productType )
            |> List.iter x.Add
        setProducts partyData.Products
        x, setProducts
    let getProducts() = products |> Seq.map(fun x -> x.Product) |> Seq.toList
    
    let getChecked() =
        let xs = getProducts()
        if xs |> List.forall( fun x -> x.IsChecked ) then Nullable<bool>(true) else
        if xs |> List.forall( fun x -> not x.IsChecked ) then Nullable<bool>(false) else
        Nullable<bool>()
    let mutable productsChecked = Nullable<bool>()

    let setMainWindowTitle() = 
        MainWindow.form.Text <- 
            sprintf "Партия %s %s %A" 
                (DateTime.format "dd/MM/yy" partyHeader.Date)
                partyHeader.ProductType.What  
                partyHeader.Name
  
    do
        setMainWindowTitle()

    let addLoggingEvent = new Event<_>()

    override x.RaisePropertyChanged propertyName = 
        ViewModelBase.raisePropertyChanged x propertyName

    member private __.AddLoggingEvent = addLoggingEvent

    member __.OnAddLogging = addLoggingEvent.Publish
            
    member __.Products = products

    member x.Party 
        with get() = 
            let partyData = { partyData with Products = getProducts() }
            let partyHeader = 
                {   partyHeader with 
                        Serials =  List.map Product.serial partyData.Products }
            partyHeader, partyData
        and set ( otherHeader, otherPartyData) = 
            partyHeader <- otherHeader
            partyData <- otherPartyData
            products
            |> Seq.toList
            |> List.iter x.DeleteProduct
            setProducts otherPartyData.Products
            x.RaisePropertyChanged "ProductType"
            x.RaisePropertyChanged "Name"
            setMainWindowTitle()
            AppConfig.config.View.PartyId <- partyHeader.Id

    member __.NewValidAddr() = products |> Seq.map(fun x -> x.Addr)  |> Party.getNewValidAddy
    
    member x.AddNewProduct() = 
        Product.New 0 (x.NewValidAddr())
        |> createProductViewModel productType 
        |> products.Add 
        
    member __.DeleteProduct(product) = 
        let r = products.Remove( product )
        if not r then            
            failwith "Bps21.ViewModel.Party.DeleteProduct : missing element"
        
    member x.HasOneCheckedProduct() =
        products
        |> Seq.exists( fun p -> p.IsChecked )
    
    member x.HasNotOneCheckedProduct() =
        products
        |> Seq.exists( fun p -> p.IsChecked )
        |> not

    member __.getProductType() = partyHeader.ProductType

    member x.ProductType 
        with get() = partyHeader.ProductType.What
        and set v = 
            if v <> x.ProductType then
                let t = 
                    ProductType.values 
                    |> List.tryFind( ProductType.what >> (=) v)
                    |> Option.withDefault ProductType.values.Head
                partyHeader <- { partyHeader with ProductType = t}
                x.RaisePropertyChanged "ProductType"
                setMainWindowTitle()

    member x.Name 
        with get() = partyHeader.Name
        and set v = 
            if v <> x.Name then
                partyHeader <- { partyHeader with Name = v}
                x.RaisePropertyChanged "Name"
                setMainWindowTitle()

    member x.ProdLog 
        with get() = partyData.ProdLog
        and set value =
            if value <> partyData.ProdLog then
                partyData <- { partyData with ProdLog =  value }
                x.RaisePropertyChanged "ProdLog"
    
[<AutoOpen>]
module private RunInfoHelpers =
    let private getHash (x:string) = x.GetHashCode()
    let now() = Some DateTime.Now
    let upd op y (x:Party) = 
        x.ProdLog <- Map.add (getHash op) y x.ProdLog
    let tryGetOp op (x:Party) = x.ProdLog.TryFind (getHash op)
    let getOp x party  = tryGetOp x party |> Option.getWithf ProdOpInfo.createNew

type Party with
    
    member x.TryGetLogOperation operation  = tryGetOp operation x
    member x.GetLogOperation operation  = getOp operation x

    member x.LogStartOperation operation  = 
        upd operation  { RunStart = now(); RunEnd = None; LogLines = []} x

    member x.LogStopOperation operation =
        upd operation { getOp operation x  with RunEnd = now() } x
    
    member x.WriteJournal operation level text = 
        let perfOp = getOp operation x
        let logLines = (DateTime.Now,level,text)::perfOp.LogLines
        upd operation { perfOp with LogLines = logLines } x
        x.AddLoggingEvent.Trigger (operation,level,text)