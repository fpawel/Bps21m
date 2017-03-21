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
    member x.What = 
        let n = 
            match x.Number with
            | Some n -> sprintf "-%s%d" (if n < 10 then "0" else "") n
            | _ -> ""

        sprintf "ИБЯЛ.411111.047%s БПС21М3-%s" n x.Name
    
[<AutoOpen>]
module private Helpers =
    let productTypes = 
        [   yield { Number = Some 5
                    Name = "220×24"
                    U1 = 23.5m, 24.5m 
                    U2 = 23m, 24.5m 
                    In = 260m, 10m }

            for n, name in [ 3, "24×16-ibIIC"; 4,"24×16-ibIIC-P"; 7,"220×16-ibIIC"] do
                yield { Number = Some n
                        Name = name
                        U1 = 14.5m, 15.1m 
                        U2 = 14.2m, 15m 
                        In = 170m, 5m }

            for n,name in [1, "24×16-ibIIB"; 2, "24×16-ibIIB-P"; 6, "220×16-ibIIB"] do
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
    static member values = 
        [   Adjust
            Tune ScaleBeg
            Tune ScaleEnd
            TestAlarmFailure 
        ]
    member x.What = 
        match x with
        | Adjust -> "Корректировка 4-20 мА"
        | Tune i -> sprintf "Корректировка Iвых %M мА" i.Current.Value
        | LoadCapacity -> "Нагрузочная способность линии питания датчика"
        | TestAlarmFailure  -> "Режим ОТКАЗ"


    

type Id = string

type Product = 
    {   Id : Id
        IsChecked : bool        
        Addr : byte
        Serial : int
        Production : Map<ProductionPoint, Logging.Line > }

    member x.What = Product.what x

    static member id x = x.Id
    static member serial x = x.Serial

    static member createNewId() = String.getUniqueKey 12
    static member what x = sprintf "№%d.#%d" x.Serial x.Addr 

    static member New serial addy = 
        let now = DateTime.Now
        {   Id = Product.createNewId()
            Addr = addy
            Serial = serial
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
            ProductType : ProductType
            Name : string
            Serials : int list   }

        static member id x = x.Id 

    type Data = {
        Products : Product list
        ProdLog : ProdLog }

    type Content = Head * Data

    let getNewValidAddy addrs = 
        let rec loop n = 
            if addrs |> Seq.exists( (=) n ) then
                if n=127uy then 1uy else loop (n+1uy)
            else n
        loop 1uy

    let New name productType count : Content =         
        let products = 
            [1..count] |> List.map( fun n -> Product.New n (byte n) ) 

        {   Id = Product.createNewId()
            Serials = List.map Product.serial products 
            Date=DateTime.Now 
            Name = name
            ProductType = productType }, 
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


