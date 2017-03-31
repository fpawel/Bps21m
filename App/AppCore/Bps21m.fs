namespace Bps21

open System



type ProductType =
    {   Number : int option
        Name : string
        // контроль напряжения питания линии датчика 
        U1 : decimal * decimal
        U2 : decimal * decimal
        // нагрузка линии датчика
        In : decimal * decimal
    }
    static member what (x : ProductType) =  x.What

    member x.Doc = 
        let n = 
            match x.Number with
            | Some n -> sprintf "-%s%d" (if n < 10 then "0" else "") n
            | _ -> ""

        sprintf "ИБЯЛ.411111.047%s" n

    member x.What = 
        sprintf "БПС21М3-%s" x.Name
    
[<AutoOpen>]
module private Helpers =
    let productTypes = 
        [   yield { Number = Some 5
                    Name = "220х24"
                    U1 = 23.5m, 24.5m 
                    U2 = 23m, 24.5m 
                    In = 260m, 10m }

            for n, name in [ 3, "24х16-ibIIC"; 4,"24х16-ibIIC-P"; 7,"220х16-ibIIC"] do
                yield { Number = Some n
                        Name = name
                        U1 = 14.5m, 15.1m 
                        U2 = 14.2m, 15m 
                        In = 170m, 5m }

            for n,name in [1, "24х16-ibIIB"; 2, "24х16-ibIIB-P"; 6, "220х16-ibIIB"] do
                yield { Number = Some n
                        Name = name
                        U1 = 14.5m, 15.1m 
                        U2 = 14.2m, 15m 
                        In = 210m, 10m } ] |> List.sortBy(fun {Number = n} -> n)


type ProductType with 
    static member values = productTypes

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
            TestAlarmFailure 
            TestPorog1
            TestPorog2
            TestPorog3
            ReservedPower
        ]
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

    

type Id = string

type Product = 
    {   Id : Id
        IsChecked : bool        
        Addr : byte
        Kind : string
        Serial : string
        Production : Map<ProductionPoint, Logging.Line > }

    member x.What = Product.what x

    static member id x = x.Id
    static member serial x = x.Serial

    static member createNewId() = String.getUniqueKey 12
    static member what x = sprintf "%s.%s.%d" x.Kind x.Serial x.Addr 

    static member New addy = 
        let now = DateTime.Now
        {   Id = Product.createNewId()
            Addr = addy
            Serial = "?"
            Kind = "?"
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

type RLoadLine =
    | RLoadLine68
    | RLoadLine91
    member x.Value = 
        match x with
        | RLoadLine68 -> 68m
        | RLoadLine91 -> 91m

    member x.What = 
        match x with
        | RLoadLine68 -> "68 Ом"
        | RLoadLine91 -> "91 Ом"

    static member Parse str = 
        if RLoadLine68.What = str then RLoadLine68 else RLoadLine91

module Party =
    
    type Head = 
        {   Id : Id
            Date : DateTime
            ProductType : ProductType
            Name : string
            Serials : string list   }

        static member id x = x.Id 

    type Data = {
        Products : Product list
        ProdLog : ProdLog 
        RLoadLine : RLoadLine
        UloadMin : decimal
        UloadMax : decimal
        }

    type Content = Head * Data

    let getNewValidAddy addrs = 
        let rec loop n = 
            if addrs |> Seq.exists( (=) n ) then
                if n=127uy then 1uy else loop (n+1uy)
            else n
        loop 1uy

    let New name productType count : Content =         
        let products = 
            [1..count] |> List.map( fun n -> Product.New (byte n) ) 

        {   Id = Product.createNewId()
            Serials = List.map Product.serial products 
            Date=DateTime.Now 
            Name = name
            ProductType = productType }, 
            {   Products = products
                ProdLog = Map.empty 
                RLoadLine = RLoadLine68
                UloadMin = 14.2m
                UloadMax = 15m}

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


