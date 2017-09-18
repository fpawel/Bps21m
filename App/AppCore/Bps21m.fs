namespace Bps21

open System

type ExplosionProtection = 
    | AIIB 
    | AIIC
    | BIIB 
    | BIIC    
    | None__
    member x.What = 
        match x with
        | AIIB -> "iaIIB"
        | AIIC -> "iaIIC"
        | BIIB -> "ibIIB"
        | BIIC -> "ibIIC"
        | None__ -> ""

    member x.R = 
        match x with
        | AIIC 
        | BIIC -> 91m
        | _ -> 68m

type SwitchLoadType = 
    | Rele
    | K
    | None_
    member x.What = 
        match x with
        | Rele -> "P"
        | K -> "K"
        | None_ -> ""


type NPorog= 
    | NPorog1
    | NPorog2
    | NPorog3
    static member values = [ NPorog1; NPorog2; NPorog3]
    
    member x.Order = NPorog.GetOrder x

    static member GetOrder = function
        | NPorog1 -> 0
        | NPorog2 -> 1
        | NPorog3 -> 2
    

type ProductType =
    {   mutable Number : int 
        mutable Ucc : decimal
        mutable DUcc1 : decimal 
        mutable DUcc2 : decimal 
        mutable Uout : decimal
        mutable SwitchLoadType : SwitchLoadType
        mutable ExplosionProtection : ExplosionProtection
        mutable Porog1 : bool
        mutable Porog2 : bool
        mutable Porog3 : bool
        mutable ReservedPower : bool
        mutable Tune : bool
        mutable LoadCapacity : bool
    }
    static member what (x : ProductType) =  x.What

    member x.Porog = function
        | NPorog1 -> x.Porog1
        | NPorog2 -> x.Porog2
        | NPorog3 -> x.Porog3
        
    member x.Doc = 
        sprintf "ИБЯЛ.411111.047%s" ( if x.Number > 0 then x.StrNumber else "" )

    member x.StrNumber = 
        if x.Number < 10 then sprintf "0%d" x.Number else sprintf "%d" x.Number

    member x.ExplosionProtectionStr  
        with get () = 
            x.ExplosionProtection.What
        and set v = 
            x.ExplosionProtection <- 
                match v with
                | "iaIIB" -> AIIB 
                | "iaIIC" -> AIIC 
                | "ibIIB" ->  BIIB
                | "ibIIC" ->  BIIC 
                | _ -> None__ 

    member x.SwitchLoadTypeStr  
        with get () = 
            x.SwitchLoadType.What
        and set v = 
            x.SwitchLoadType <- 
                match v with
                | "P" -> Rele
                | "K" -> K 
                | _ -> None_ 
            

    member x.What = 
        let strST = 
            match x.SwitchLoadType with
            | Rele -> "-Р" 
            | K -> "-K"
            | None_ -> ""
        let strExplosionProtection = 
            if x.ExplosionProtection = None__ then 
                "" 
            else 
                "-" + x.ExplosionProtection.What
        let strUout = 
            if x.Uout = 0m then 
                "-КСД"
            else
                sprintf "х%M" x.Uout
        sprintf "%s: %M%s%s%s" x.StrNumber x.Ucc strUout strExplosionProtection strST

    

module ProductTypes =    
    let private pt n ucc ducc1 ducc2 uout slt ep   = 
        
        {   Number = n
            Ucc = ucc
            DUcc1 = ducc1
            DUcc2 = ducc2
            Uout = uout
            SwitchLoadType = slt 
            ExplosionProtection = ep 
            Porog1 = slt <> K  
            Porog2 = slt <> K 
            Porog3 = slt <> K  && ucc <> 220m
            ReservedPower = ucc <> 220m
            Tune =          slt <> K  && uout <> 0m
            LoadCapacity =  slt <> K  && uout <> 0m  
        }

    let private pt24  n = pt n 24m 3.6m 2.4m 
    let private pt220 n = pt n 220m 43m 23m
        
    let defaultProductTypes = 
        [   pt24  0  24m Rele  None__ 
            pt24  1  16m None_ BIIB 
            pt24  2  16m Rele  BIIB 
            pt24  3  16m None_ BIIC 
            pt24  4  16m Rele  BIIC 
            pt220 5  24m None_ None__ 
            pt220 6  16m None_ BIIB 
            pt220 7  16m None_ BIIC 
            pt24  8  0m  None_ None__ 
            pt24  9  0m  Rele  None__ 
            pt24  10 24m None_ AIIC
            pt24  11 24m Rele  AIIC
            pt220 12 24m None_ AIIC
            pt24  13 16m K     BIIC 
            pt24  14 24m K     AIIC  ]  

    open System.ComponentModel

    let values, save =  
        let filename = "productTypes.json"
        let xs, err = Json.Config.read filename <| fun () -> defaultProductTypes 
        match err with 
        | Some err -> Logging.error "%s" err 
        | _ -> ()
        let values = new BindingList<ProductType>( ResizeArray<ProductType>(xs) ) 
        let save() = Json.Config.write filename [ for x in values -> x ]
        values,save

    let byName s = 
        values 
        |> Seq.tryFind( fun pt -> pt.What = s)
        |> Option.withDefault values.[0]

type Current = 
    | I4 | I12 | I20    
    member x.Value = 
        match x with
        | I4 -> 4m
        | I12 -> 12m
        | I20 -> 20m
    static member GetValue (x:Current) = x.Value

type ScalePoint = 
    | ScaleBeg
    | ScaleEnd
    member x.Current = 
        match x with
        | ScaleBeg -> I4
        | ScaleEnd -> I20

type ProductionPoint =
    | Adjust
    | Tune of ScalePoint
    | LoadCapacity
    | TestAlarmFailure 
    | TestPorog1
    | TestPorog2
    | TestPorog3
    | ReservedPower
    static member values = 
        [   Adjust
            Tune ScaleBeg
            Tune ScaleEnd
            LoadCapacity
            TestAlarmFailure 
            TestPorog1
            TestPorog2
            TestPorog3
            ReservedPower ]
    member x.What = 
        match x with
        | Adjust -> "Корректировка Iвх"
        | Tune i -> sprintf "Корректировка Iвых %M мА" i.Current.Value
        | LoadCapacity -> "Нагрузочная способность"
        | TestAlarmFailure  -> "Режим ОТКАЗ"
        | TestPorog1 -> "ПОРОГ 1"
        | TestPorog2 -> "ПОРОГ 2"
        | TestPorog3 -> "ПОРОГ 3"
        | ReservedPower -> "Резервное питание"

    member x.Property = 
        match x with
        | Tune i -> sprintf "ProdPtTune%A" i.Current.Value
        | x -> sprintf "ProdPt%A" x

type Id = string

type Product = 
    {   Id : Id
        IsChecked : bool        
        Addr : byte
        Kind : string
        Serial : int
        Year : int
        Quarter : int
        Production : Map<ProductionPoint, Logging.Line > }

    member x.What = Product.what x

    static member id x = x.Id
    static member serial x = x.Serial, x.Quarter, x.Year

    static member createNewId() = String.getUniqueKey 12
    static member what x = sprintf "№%d #%d" x.Serial x.Addr 

    static member New addy = 
        let now = DateTime.Now
        {   Id = Product.createNewId()
            Addr = addy
            Serial = 0
            Kind = "?"
            Year = DateTime.Now.Year
            Quarter = DateTime.Now.Month /4

            IsChecked = true
            Production = Map.empty }

type ProdOpInfo =
    {   RunStart : DateTime option 
        RunEnd : DateTime option
        LogLines : Logging.Lines }
    static member createNew() = 
        {   RunStart = None
            RunEnd = None
            LogLines = [] }
        

type ProdLog = Map<int, ProdOpInfo >



module Party =
    
    type Head = 
        {   Id : Id
            Date : DateTime
            ProductTypeNumber : int
            Name : string
            Serials : (int*int*int) list   }

        static member id x = x.Id 

        member x.ProductType = 
            let y = ProductTypes.values |> Seq.tryFind(fun prodType -> prodType.Number = x.ProductTypeNumber)
            match y with
            | Some y -> y
            | None -> ProductTypes.defaultProductTypes.[0]

    type Data = {
        Products : Product list
        ProdLog : ProdLog 
        }

    type Content = Head * Data

    let getNewValidAddy addrs = 
        let rec loop n = 
            if addrs |> Seq.exists( (=) n ) then
                if n=127uy then 1uy else loop (n+1uy)
            else n
        loop 1uy

    let New name productTypeNumber count : Content =         
        let products = 
            [1..count] |> List.map( fun n -> Product.New (byte n) ) 

        {   Id = Product.createNewId()
            Serials = List.map Product.serial products 
            Date=DateTime.Now 
            Name = name
            ProductTypeNumber = productTypeNumber }, 
            {   Products = products
                ProdLog = Map.empty }

type DelayType = 
    | DelayPowerOn 
    | DelayTune
    static member values = 
        [   DelayPowerOn 
            DelayTune  ]

    static member what = function
        | DelayPowerOn -> "включение питания"
        | DelayTune -> "таймаут подстройки тока"

    member x.What = DelayType.what x
    member x.Prop = DelayType.getPropertyName x

    static member getPropertyName (x:DelayType) =  FSharpValue.unionCaseName x


