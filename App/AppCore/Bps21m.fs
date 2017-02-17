namespace Bps21

open System

type ThresholdIndex = 
    | Th1
    | Th2
    | Th3

    static member values = [ Th1; Th2; Th3]
    
    member x.Order = ThresholdIndex.GetOrder x

    static member GetOrder = function
        | Th1 -> 0
        | Th2 -> 1
        | Th3 -> 2

type Rele =
    | ReleThreshold of ThresholdIndex
    | ReleFailure
    | ReleMode
    | ReleStatus

type ReleState = Rele * bool

type TestProduct = 
    | TestTens of decimal * decimal  
    | TestReles of ReleState list

type ProductType =
    {   Number : int
        // контроль напряжения питания линии датчика 
        U1 : decimal * decimal
        U2 : decimal * decimal
        // нагрузка линии датчика
        In : decimal * decimal
    }
    static member what (x : ProductType) =  x.What
    member x.What = 
        let n = x.Number
        sprintf "ИБЯЛ.411111.047-%s%d" (if n < 10 then "0" else "") n
    
[<AutoOpen>]
module private Helpers =
    let productTypes = 
        [   yield { Number = 5
                    U1 = 23.5m, 24.5m 
                    U2 = 23m, 24.5m 
                    In = 260m, 10m }

            for n in [3;4;7] do
                yield { Number = n
                        U1 = 14.5m, 15.1m 
                        U2 = 14.2m, 15m 
                        In = 170m, 5m }

            for n in [1;2;6] do
                yield { Number = n
                        U1 = 14.5m, 15.1m 
                        U2 = 14.2m, 15m 
                        In = 210m, 10m } ] |> List.sortBy(fun {Number = n} -> n)


type ProductType with 
    static member values = productTypes



type ProdPt =
    | Adjust
    | Tune of ScalePt
    | TestLoad
    | TestFailure
    | TestThreshold of ThresholdIndex
    | TestReservedPower

type Id = string

type Product = 
    {   Id : Id
        IsChecked : bool        
        Addr : byte
        Serial : int
        ProdLogLines : Map<ProdPt, Logging.Line > }

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
            ProdLogLines = Map.empty }

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
    | DelaySetCurrent
    | DelayAdjust
    static member values = 
        [   DelayPowerOn 
            DelaySetCurrent
            DelayAdjust ]

    static member what = function
        | DelayPowerOn -> "включение питания"
        | DelaySetCurrent -> "установка входного тока"
        | DelayAdjust -> "корректировка показаний"

    member x.What = DelayType.what x
    member x.Prop = DelayType.getPropertyName x

    static member getPropertyName (x:DelayType) =  FSharpValue.unionCaseName x

type DevVar =
    | DevConc
    | DevCurr
    | DevTens
