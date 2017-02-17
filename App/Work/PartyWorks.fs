module Bps21.PartyWorks

open System

open Thread2

open ViewModel.Operations

[<AutoOpen>]
module private Helpers = 
    type P = Product
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
    
    member x.Write cmd =
        x.Connection <- 
            Device.Cmd.Send x.Addr cmd 
            |> Result.map( fun () -> cmd.What )
            |> Some


    member x.ReadVar var = 
        match var with
        | DevConc -> 
            Device.readIndication x.Addr
            |> Result.map( fun v -> sprintf "C=%M" v.Conc )
        | DevCurr -> 
            Device.readCurrent x.Addr
            |> Result.map( sprintf "I=%M")
        | DevTens -> 
            Device.readTension x.Addr
            |> Result.map( sprintf "U=%M")
        |> fun v -> 
            x.Connection <- Some v
            v
            
    member x.Interrogate() = maybeErr {
        let xs = 
            let xs = AppConfig.config.View.DevVars
            if Set.isEmpty xs then Set.singleton Bps21.DevConc else xs
        for var in xs do
            if isKeepRunning() then
                let! _ = x.ReadVar var 
                () }

type Bps21.ViewModel.Party with
    member x.DoForEachProduct f = 
        let xs = x.Products |> Seq.filter(fun p -> p.IsChecked)
        if Seq.isEmpty xs then
            Err "приборы не отмечены"
        else
            for p in xs do 
                if isKeepRunning() && p.IsChecked then 
                    f p
            Ok ()

    member x.Interrogate() = Option.toResult <| maybeErr {
        let xs = x.Products |> Seq.filter(fun p -> p.IsChecked)
        if Seq.isEmpty xs then
            return "приборы не отмечены"
        else
            do! Comport.testPort Device.comportConfig
            for p in xs do 
                if isKeepRunning() && p.IsChecked then                         
                    do! p.Interrogate() }

    member x.Write(cmd) = maybeErr{
        do! Comport.testPort Device.comportConfig
        do! x.DoForEachProduct (fun p -> p.Write cmd   ) }

    
    
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
    let onClose = Ref.Initializable<_>(sprintf "ModalMessage.onClose %s:%s" __LINE__ __SOURCE_FILE__ )
    
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

    let opWriteParty cmd = 
        let f() = party.Write cmd
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
    "Корректировка 4-20 мА" <||> [           
        opWriteParty Device.Cmd.MainPowerOn
        opDelay2 "Пауза 2 мин." _2minute DelayPowerOn

        opWriteParty Device.Cmd.Set4mA
        opDelay2 "Пауза 10 с" _10sec DelaySetCurrent
        
        opWriteParty Device.Cmd.Adjust4mA
        opDelay2 "Пауза 10 с" _10sec DelayAdjust

        opWriteParty Device.Cmd.Set20mA
        opDelay2 "Пауза 10 с" _10sec DelaySetCurrent

        opWriteParty Device.Cmd.Adjust20mA
        opDelay2 "Пауза 10 с" _10sec DelayAdjust
    ]
    |> withСonfig




//    let blow = 
//        all |> List.choose ( function 
//            | (Op.Timed (_, ({DelayType = BlowDelay gas} as delay),_) as op) -> 
//                Some (op,delay,gas)
//            | _ -> None)


[<AutoOpen>]
module private Helpers3 =
    let ( -->> ) s f =
        s <|> f
        |> Thread2.run 

    
let runInterrogate() = "Опрос" -->> fun () -> maybeErr{ 
    do! Comport.testPort Device.comportConfig
    while Thread2.isKeepRunning() do
        do! party.Interrogate() }


let setAddr addr = sprintf "Установка адреса %A" addr -->> fun () -> maybeErr{ 
    do! Device.setAddr addr
    let! _ =  Mdbs.read3decimal Device.comportConfig (byte addr) 0 "проверка установки адреса"
    () }

//let sendCommand (cmd,value as x) = 
//    sprintf "%s <- %M" (Command.what cmd) value -->> fun () -> 
//        party.WriteModbus x

