module Bps21.PartyWorks

open System

open Thread2

open ViewModel.Operations

[<AutoOpen>]
module private Helpers = 
    type P = Bps21.ViewModel.Product
    let party = AppContent.party
    let appCfg = AppConfig.config
    let viewCfg = appCfg.View
    
    let checkedProducts() = 
        party.Products
        |> Seq.filter( fun p -> p.IsChecked )
    let hasNotCheckedProduct() = checkedProducts() |> Seq.isEmpty 
    let hasCheckedProduct() = not <| hasNotCheckedProduct()
    
    let doWithProducts f = 
        checkedProducts() |> Seq.iter ( fun p ->       
            if isKeepRunning() && p.IsChecked then 
                f p ) 



type Bps21.ViewModel.Product with
    static member TestRead (x:Bps21.ViewModel.Product) = maybeErr {
        let! _ = x.ReadProductCurrent()
        let! _ = x.ReadStendCurrent()
        let! _ = x.ReadStendTension()
        let! {Porog1 = p1; Porog2 = p2; Porog3 = p3} as a = x.ReadStendRele()
        let! _,p1_,p2_,p3_,_  as b = x.ReadProductStatus()
        if (p1,p2,p3) <> (p1_,p2_,p3_) then
            Logging.error "несоответствие данных порогов, прибор: %A, стенд: %A" a b       
        return! None }

type Bps21.ViewModel.Party with
    member x.DoForEachProduct f = 
        let xs1 = x.Products  |> Seq.filter(fun p ->  p.IsChecked )
        if Seq.isEmpty xs1 then Err "приборы не отмечены" else
        let xs2 =  xs1 |> Seq.filter(fun p -> 
            match p.Connection with
            | Some (Err _ ) -> false
            | _ -> true )
        if Seq.isEmpty xs2 then Err "нет связи с приборами" else
            for p in xs2 do 
                if isKeepRunning() && p.IsChecked then 
                    f p
            Ok ()

    member x.Interrogate() = Option.toResult <| maybeErr {
        let xs = x.Products |> Seq.filter(fun p -> p.IsChecked)
        if Seq.isEmpty xs then
            return "приборы не отмечены"
        else
            do! Comport.testPort Hard.Stend.comportConfig
            do! x.DoForEachProduct ( P.TestRead >> ignore ) 
        }

    member x.WriteStend(cmd) = 
        x.DoForEachProduct (fun p -> p.WriteStend cmd  ) 
            |> Result.someErr

    member x.WriteProduct cmd value = 
        x.DoForEachProduct (fun p -> p.WriteProduct cmd value ) 
            |> Result.someErr

    member x.DoSetAddrs() = maybeErr{        
        do! x.DoForEachProduct (fun product -> 
            maybeErr{
                do! x.DoForEachProduct (fun p -> 
                    let st = 
                        obj.ReferenceEquals(p,product)
                        |> Hard.Stend.PowerState.fromBool 
                    let cmd = Hard.Stend.SetPower(Hard.Stend.PowerMain, st)
                    p.WriteStend cmd )
                do! Hard.Product.SetAddr.Perform 0uy (decimal product.Addr)
                let! _ = product.ReadProductCurrent()
                return ()
            } |> ignore         
        )   
    }

    
    
module Delay = 
    let onStart = Ref.Initializable<_>(sprintf "Delay.start %s:%s" __LINE__ __SOURCE_FILE__ )
    let onStop = Ref.Initializable<_>(sprintf "Delay.stop %s:%s" __LINE__ __SOURCE_FILE__ )
    let onUpdate = Ref.Initializable<_>(sprintf "Delay.stop %s:%s" __LINE__ __SOURCE_FILE__ )

    let mutable private keepRunning = false

    let cancel() = keepRunning <- false

    let perform what gettime interrogate = 
        onStart.Value what gettime
        keepRunning <- true
        let start'time = DateTime.Now
        let result = 
            maybeErr{
                while keepRunning && Thread2.isKeepRunning() && (DateTime.Now - start'time < gettime()) do
                    onUpdate.Value start'time gettime
                    if interrogate then
                        do! party.Interrogate()
                    else
                        Threading.Thread.Sleep 10 }
        keepRunning <- false
        onStop.Value() 
        result

module ModalMessage = 
    let onShow = Ref.Initializable<_>(sprintf "ModalMessage.onShow %s:%s" __LINE__ __SOURCE_FILE__ )
    let getIsVivisble = Ref.Initializable<_>(sprintf "ModalMessage.getIsVivisble %s:%s" __LINE__ __SOURCE_FILE__ )
    let onClose = Ref.Initializable<(unit -> unit)>(sprintf "ModalMessage.onClose %s:%s" __LINE__ __SOURCE_FILE__ )
    
    let show (level:Logging.Level) (title:string) (text:string) = 
        onShow.Value title level text
        while Thread2.isKeepRunning() && getIsVivisble.Value() do
            Threading.Thread.Sleep 50
        onClose.Value()    

[<AutoOpen>]
module private Helpers1 =     
    let none _ = None
    let (<|>) what f = 
        Operation.CreateSingle (what, none) f 
    let (<-|->) (what,time,whatDelay) f = 
        Operation.CreateTimed (what, none) (Delay.create time whatDelay) f
    let (<||>) what xs =  Operation.CreateScenary ( what, none)  xs
        
    type OpConfig = Config
    type Op = Operation

    let writeProducts (cmd,value) = 
        let f() = party.WriteProduct cmd value
        cmd.What <|> f

    let writeStend cmd = 
        let f() = party.WriteStend cmd 
        cmd.What <|> f

    let opDelay1 what time = 
        what <|> fun () -> maybeErr{    
            do! Delay.perform what (fun () -> time) true }

    let opDelay2 what time delayType = 
        (what, time, delayType) <-|-> fun gettime -> 
            Delay.perform what gettime true 

    let _2minute = TimeSpan.FromMinutes 2.
    let _10sec = TimeSpan.FromSeconds 10.

    let withСonfig (scenary:Operation)=
        let dummy msg = 
            Logging.debug "%s" msg
        
            Bps21.ViewModel.Operations.Config.CreateNew()
        let fileName = IO.Path.Combine(IO.Path.ofExe, "scenary.json")
        let config = 
            if IO.File.Exists fileName |> not then                
                dummy <| sprintf "не найден файл сценария %A" fileName 
            else                
                try
                    match Json.Serialization.parse<OpConfig> (IO.File.ReadAllText(fileName)) with
                    | Ok x -> x
                    | Err error ->
                        dummy <| sprintf "ошибла файла сценария %s\n%s" fileName error                    
                with e ->             
                    dummy <| sprintf "ошибла файла сценария %s\n%A" fileName e 
        Op.SetConfig (scenary,config)

        MainWindow.form.Closing.Add <| fun _ ->
            let config = Op.GetConfig scenary
            try
                IO.File.WriteAllText(fileName, Json.Serialization.stringify config ) 
            with e ->             
                Logging.error "ошибла сохранения файла сценария %s\n%A" fileName e 
        Thread2.scenary.Set scenary    

        scenary

let main = 
    "Настройка БПС21М3" <||> [
        "Корректировка 4-20 мА" <||> [
            writeStend Hard.Stend.Cmd.MainPowerOn
            opDelay2 "Пауза 2 мин." _2minute DelayPowerOn

            writeStend Hard.Stend.Cmd.Set4mA
            opDelay2 "Пауза 10 с" _10sec DelaySetCurrent
        
            writeProducts Hard.Product.Cmd.Adjust4mA
            opDelay2 "Пауза 10 с" _10sec DelayAdjust

            writeStend Hard.Stend.Cmd.Set20mA
            opDelay2 "Пауза 10 с" _10sec DelayAdjust

            writeProducts Hard.Product.Cmd.Adjust20mA
            opDelay2 "Пауза 10 с" _10sec DelaySetCurrent
        ]
    ] |> withСonfig

let all = Op.MapReduce Some main 

[<AutoOpen>]
module private Helpers3 =
    let ( -->> ) s f =
        s <|> f
        |> Thread2.run 
    
let runInterrogate() = "Опрос" -->> fun () -> maybeErr{ 
    do! Comport.testPort Hard.Stend.comportConfig
    do! party.WriteStend Hard.Stend.Cmd.MainPowerOn
    while Thread2.isKeepRunning() do
        do! party.Interrogate() 
   }


type Bps21.ViewModel.Party with
    member party.RunWriteStend (cmd:Hard.Stend.Cmd) = 
        cmd.What -->> fun () -> 
            party.WriteStend cmd

    member x.RunWriteProduct (cmd:Hard.Product.Cmd, value) = 
        sprintf "%s, %M" cmd.What value -->> fun () -> 
            party.WriteProduct cmd value

    member x.RunSetAddrs() = 
        "Установка сетевых адресов" -->> x.DoSetAddrs
