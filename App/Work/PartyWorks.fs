module Bps21.PartyWorks

open System

open Thread2
open Device

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
    member x.TestRead1 () = result {
        let! conc = x.ReadConc()
        let! rele = x.ReadReleState()
        let porogsConc = conc.Porog1, conc.Porog2, conc.Porog3
        (*
        let! st35 = x.ReadStatus35()
        let porogsSt35 = st35.Porog1, st35.Porog2, st35.Porog3
        if porogsConc <> porogsSt35 then
            return!
                sprintf "несоответсвие состояний порогов регистра 0 %A и регистра 35 %A" porogsConc porogsSt35 
                |> Err else
        
        let rele_ : Device.Status35  =
            {   Failure = rele.Failure 
                SpMode = rele.SpMode
                Porog1 = rele.Porog1
                Porog2 = rele.Porog2
                Porog3 = rele.Porog3}
        if rele_ <> st35 then
            return!
                sprintf "несоответсвие состояний контактов реле, полученных от стенда %A, и считанных из регистра 35 %A" 
                    rele_ st35 
                |> Err else
        *)
        return conc.Value, rele }

    member x.TestRead () = 
        let r =
            maybeErr {
                let! _ = x.TestRead1()
                let! _ = x.ReadCurrent()
                let! _ = x.ReadTension()
                return! None }
        match r with
        | Some err ->
            Logging.error "прибор №%d: %s" x.Addr err
            x.Connection <- Some (Err err)
        | _ -> ()

    

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
            do! Comport.testPort Device.comportConfig
            do! x.DoForEachProduct (fun p -> 
                p.TestRead() )

                }

    member x.Write(cmd) = maybeErr{
        do! Comport.testPort Device.comportConfig
        do! x.DoForEachProduct (fun p -> p.Write cmd   ) }

    member x.DoSetAddrs() = maybeErr{
        do! Comport.testPort Device.comportConfig
        
        do! x.DoForEachProduct (fun product -> 
            maybeErr{
                do! x.DoForEachProduct (fun p -> 
                    (PowerMain, PowerState.fromBool <| obj.ReferenceEquals(p,product) )
                    |> SetPower 
                    |> CmdStend
                    |> p.Write )

                do! Device.setAddr (decimal product.Addr)
                Mdbs.read3decimal 
                        Device.comportConfig 
                        product.Addr 0 
                        (sprintf "проверка установки адреса %d" product.Addr)
                |> ignore
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
    "Настройка БПС21М3" <||> [
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
    ]
    |> withСonfig

let all = Op.MapReduce Some main 

[<AutoOpen>]
module private Helpers3 =
    let ( -->> ) s f =
        s <|> f
        |> Thread2.run 
    
let runInterrogate() = "Опрос" -->> fun () -> maybeErr{ 
    do! Comport.testPort Device.comportConfig
    do! party.Write Device.Cmd.MainPowerOn
//    do! party.DoForEachProduct(fun p ->
//        ignore( p.ReadPorogs() )
//        )
    while Thread2.isKeepRunning() do
        do! party.Interrogate() }


type Device.Cmd with
    member x.Send () = 
        x.What -->> fun () -> 
            party.Write x


type Device.CmdDevice with
    member x.Send (value) = 
        Device.CmdDevice( x, value ).Send()


type Bps21.ViewModel.Party with
    member x.SetAddrs() = 
        "Установка сетевых адресов" -->> x.DoSetAddrs
