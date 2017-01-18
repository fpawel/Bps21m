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
    
    
    
    member x.Interrogate() = maybeErr {
        let xs = 
            let xs = AppConfig.config.View.VisiblePhysVars
            if Set.isEmpty xs then [Conc] else
            Set.toList xs
        for var in xs do
            let _ = x.ReadModbus( ReadVar var)
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
            do! Comport.testPort appCfg.Hardware.ComportProducts
            for p in xs do 
                if isKeepRunning() && p.IsChecked then                         
                    do! p.Interrogate() }

    member x.WriteModbus(cmd,value) = maybeErr{
        do! Comport.testPort appCfg.Hardware.ComportProducts
        do! x.DoForEachProduct (fun p -> p.WriteModbus(SendCommand cmd,value) |> ignore  ) }

    member x.WriteKefs(kefs) = maybeErr{
        do! Comport.testPort appCfg.Hardware.ComportProducts
        do! x.DoForEachProduct (fun p -> 
            p.WriteKefs kefs |> ignore ) }

    member x.ReadKefs(kefs) = maybeErr{
        do! Comport.testPort appCfg.Hardware.ComportProducts
        do! x.DoForEachProduct (fun p -> 
            p.ReadKefs kefs |> ignore ) }

    member x.ComputeAndWriteKefGroup (kefGroup) = 
        x.DoForEachProduct(fun p -> 
            p.ComputeKefGroup kefGroup
            KefGroup.kefs kefGroup
            |> List.map (fun x -> x, None)
            |> p.WriteKefs  
            |> ignore )
   
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

    let computeGroup kefGroup = 
        sprintf "Расчёт %A" (KefGroup.what kefGroup) <|> fun () -> 
            party.ComputeKefGroup kefGroup
            None

    let writeGroup kefGroup = 
        sprintf "Запись к-тов группы %A" (KefGroup.what kefGroup) <|> fun () -> 
            KefGroup.kefs kefGroup
            |> List.map(fun x -> x, None)
            |> party.WriteKefs 

    let computeAndWriteGroup kefGroup = 
        [   "Расчёт" <|> fun () -> 
                party.ComputeKefGroup kefGroup
                None
            "Запись" <|> fun () ->  
                KefGroup.kefs kefGroup
                |> List.map(fun x -> x, None)
                |> party.WriteKefs ]

    type OpConfig = Config
    type Op = Operation

    let switchPneumo gas = maybeErr{
        let code, title, text = 
            match gas with
            | Some gas -> 
                let s = ScalePt.what gas
                ScalePt.code gas |> byte, "Продувка " + s, "Подайте " + s
            | _ -> 0uy, "Выключить пневмоблок", "Отключите газ"

        if appCfg.Hardware.Pneumoblock.Enabled then
            do! Hardware.Pneumo.switch code
        else            
            ModalMessage.show Logging.Info  title text 
            if isKeepRunning() then
                match gas with
                | Some gas -> Logging.info "газ %s подан вручную" gas.What
                | _ -> Logging.info "пневмоблок закрыт вручную" }

    let blow minutes gas what = 
        let s = ScalePt.what gas
        let title, text = "Продувка " + s, "Подайте " + s

        (what, TimeSpan.FromMinutes (float minutes), BlowDelay gas ) <-|-> fun gettime -> maybeErr{        
            do! switchPneumo <| Some gas
            do! Delay.perform title gettime true }


    let warm tempValue = maybeErr{    
        if appCfg.Hardware.Pneumoblock.Enabled && Hardware.Pneumo.isOpened()  then
            do! switchPneumo None
        Logging.info "Установка температуры %M\"C" tempValue
        if not appCfg.Hardware.Termochamber.Enabled then             
            ModalMessage.show Logging.Info
                "Уставка термокамеры" (sprintf "Установите в термокамере температуру %M\"C" tempValue) 
            if isKeepRunning() then
                Logging.info "температура %M\"C установлена вручную" tempValue
        else 
            do! Hardware.Warm.warm tempValue Thread2.isKeepRunning party.Interrogate  
    }

    type TermoPt with
        member x.Warm() = 
            TermoPt.warm x
        static member warm t =
            warm (party.GetTermoTemperature t)

    let adjust isScaleEnd = 
        let cmd, gas, wht = 
            if not isScaleEnd then 
                AdjustBeg, ScaleBeg, "начало" 
            else 
                AdjustEnd, ScaleEnd, "конец"
        let whatOperation = sprintf "Калибровка %s шкалы" wht
        let defaultDelayTime = TimeSpan.FromMinutes 3.
        (whatOperation, defaultDelayTime, AdjustDelay isScaleEnd)  <-|-> fun gettime -> maybeErr{
            let pgs = party.GetPgs gas
            Logging.info  "Калибровка %s шкалы, %M" wht  pgs
            do! switchPneumo <| Some gas
            do! Delay.perform (sprintf  "Продувка перед калибровкой %A" gas.What) gettime true
            do! party.WriteModbus( cmd, pgs ) 
            do! Delay.perform (sprintf  "Выдержка после калибровки %A" gas.What) (fun () -> TimeSpan.FromSeconds 10.) true
            }

    let goNku = "Установка НКУ" <|> TermoNorm.Warm

    let readVarsValues feat gas t wht = maybeErr{
        do! Comport.testPort appCfg.Hardware.ComportProducts
        do! party.DoForEachProduct(fun p -> 
            maybeErr{
                for var in PhysVar.values do
                    let! readedValue = p.ReadModbus( ReadVar var)
                    p.setVar (feat,var,gas,t) (Some readedValue)
                    Logging.info "%s : %s = %s" p.What (PhysVar.what var) (Decimal.toStr6 readedValue) } |> function 
            | Some error -> Logging.error "%s" error
            | _ -> () ) }

let blowAir() = 
    "Продувка воздухом" <||> [   
        blow 1 ScaleBeg "Продуть воздух"
        "Закрыть пневмоблок" <|> fun () -> switchPneumo None
    ]
let blowAndRead feat t =
    [   for gas in ScalePt.values ->
            gas.What <||> [
                yield blow 3 gas "Продувка"
                yield "Считывание" <|> readVarsValues feat gas t  ] 
        yield blowAir() ]

let warmAndRead feat t =
    sprintf "Cнятие %A %A" (Feature.what1 feat) (TermoPt.what t) <||> 
        [   yield sprintf "Температура %A" (TermoPt.what t) <||> [
                yield "Установка"  <|> t.Warm
                yield ("Выдержка", TimeSpan.FromHours 1., WarmDelay t) <-|-> fun gettime -> maybeErr{    
                    do! switchPneumo None    
                    do! Delay.perform ( sprintf "Выдержка термокамеры %A" (TermoPt.what t) ) gettime true } ]
        
            yield! blowAndRead feat t  ]

let test = 
    
    [   adjust false
        adjust true 
        blowAir()
        "Cнятие НКУ" <||> ( blowAndRead Test TermoNorm  )
        warmAndRead Test TermoLow 
        warmAndRead Test TermoHigh 
        warmAndRead Test Termo90 
        warmAndRead RetNku TermoNorm  ]
    |> (<||>) "Проверка"


let texprogon = 
    "Техпрогон" <||> [   
        adjust false
        adjust true 
        blowAir()
        "Снятие перед техпрогоном" <||> blowAndRead Tex1 TermoNorm 
        ("Выдержка на техпрогоне", TimeSpan.FromHours 16., TexprogonDelay) <-|-> fun gettime ->
            Delay.perform "Техпрогон" gettime true
        "Снятие после техпрогона" <||> blowAndRead Tex2 TermoNorm ]

let reworkTermo = 
        
    [   goNku
        adjust false
        adjust true
        blowAir()
        "Снятие НКУ для перевода климатики" <||> blowAndRead Termo TermoNorm 
        "Переcчёт" <|> fun () ->
            party.DoForEachProduct(fun p -> 
                p.Product <- snd <| runState Alchemy.translateTermo p.Product )
            |> Result.someErr
            
        "Термокомпенсация" <||> [
            "Начало шкалы" <||> computeAndWriteGroup (KefTermo ScaleBeg)
            "Конец шкалы" <||> computeAndWriteGroup (KefTermo ScaleEnd) ]
        test ]
    |> (<||>) "Перевод климатики" 

    

let productionWork = 
    [   "Установка к-тов исп." <|> fun () -> maybeErr{
            do! party.DoForEachProduct (fun p -> 
                p.WriteKefsInitValues()
                |> ignore ) }
        goNku
        blowAir()
        "Ноормировка" <|> fun () -> party.WriteModbus(Nommalize, 1000m)        
        adjust false
        adjust true 
        "Линеаризация" <||> [
            yield "Снятие" <||> blowAndRead Lin TermoNorm 
            yield!  computeAndWriteGroup KefLin ]
        warmAndRead Termo TermoLow 
        warmAndRead Termo TermoHigh 
        warmAndRead Termo TermoNorm 
        "Термокомпенсация"  <||> [
            for gas in ScalePt.values ->
                 gas.What <||> computeAndWriteGroup  (KefTermo gas) ]
        test
        "Сигналы каналов"  <|> fun () -> 
            party.DoForEachProduct (fun p -> 
                p.ReadKefs [Coef21; Coef22; CoefKw; CoefKr]
                |> ignore )
            |> Result.someErr

        texprogon ]
    |> (<||>) "Осн."

let testo = 
    let m = System.Collections.Generic.Dictionary<string,decimal>()
    [   "draw sin" <|> fun () -> 
            while isKeepRunning() do
                for p in party.Products do
                    if notKeepRunning() then () else
                    let b,v = m.TryGetValue(p.Product.Id)
                    let v = if b then v else 0m
                    let y = System.Math.Sin( float v ) |> decimal 
                    p.setPhysVarValue Conc y
                    m.[p.Product.Id] <- v + 0.1m
                    sleep 100 |> ignore                
            None ]
    |> (<||>) "testo"

let bps21 =
    let bps21 =  "МИЛ-82" <||> [productionWork; reworkTermo]
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
    Op.SetConfig (bps21,config)

    MainWindow.form.Closing.Add <| fun _ ->
        let config = Op.GetConfig bps21
        try
            IO.File.WriteAllText(fileName, Json.Serialization.stringify config ) 
        with e ->             
            Logging.error "ошибла сохранения файла сценария %s\n%A" fileName e 
    Thread2.scenary.Set bps21    

    bps21


module Works =
    let all = Op.MapReduce Some bps21 

    let blow = 
        all |> List.choose ( function 
            | (Op.Timed (_, ({DelayContext = BlowDelay gas} as delay),_) as op) -> 
                Some (op,delay,gas)
            | _ -> None)

    let warm = 
        all |> List.choose ( function 
            | (Op.Timed (_, ({DelayContext = WarmDelay t} as delay),_) as op) -> 
                Some (op,delay,t)
            | _ -> None)



[<AutoOpen>]
module private Helpers3 =
    let ( -->> ) s f =
        s <|> f
        |> Thread2.run false

    
let runInterrogate() = "Опрос" -->> fun () -> maybeErr{ 
    do! Comport.testPort appCfg.Hardware.ComportProducts
    while Thread2.isKeepRunning() do
        do! party.Interrogate() }


let setAddr addr = sprintf "Установка адреса %A" addr -->> fun () -> maybeErr{ 
    
    do! Mdbs.write appCfg.Hardware.ComportProducts 0uy ResetAddy.Code "установка адреса" addr
    let! _ =  Mdbs.read3decimal appCfg.Hardware.ComportProducts (byte addr) 0 "проверка установки адреса"
    () }

let sendCommand (cmd,value as x) = 
    sprintf "%s <- %M" (Command.what cmd) value -->> fun () -> 
        party.WriteModbus x


module Pneumoblock =
    let switch gas = 
        ScalePt.what gas -->> fun () -> 
            Hardware.Pneumo.switch gas.Code |> Result.someErr
    let close() = 
        "Выкл." -->> fun () ->
            Hardware.Pneumo.switch 0uy |> Result.someErr

module Kefs = 
    
    let private run s f = 
        s -->> fun () ->
            let x = appCfg.View
            let kefs = 
                Set.intersect 
                    (IntRanges.parseSet x.SelectedCoefs)
                    (IntRanges.parseSet x.VisibleCoefs)
                |> Set.map Coef.tryGetByOrder  
                |> Set.toList
                |> List.choose id
                
            if kefs.IsEmpty then Some "не выбрано ни одного коэффициента" else f kefs

    let write() = run "Запись к-тов" ( List.map(fun x -> x, None) >> party.WriteKefs  )
    let read() = run "Считывание к-тов"  party.ReadKefs

module TermoChamber =
    let termocicling (count,  tempMin, tempMax, warmTime)  =         
        "Термоциклирование" -->> fun () -> maybeErr {
            let warmTime _ = warmTime
            let mutable n = 0
            for n = 1 to count do
                let what = sprintf "Термоцикл %d из %d" n count
                Logging.info "%s начат" what
                for temp in [tempMin; tempMax] do
                    Logging.info "%s, уставка %M\"C" what temp
                    do! warm temp
                    do! Delay.perform 
                            (sprintf "%s, выдержка при %M\"C" what temp)
                            warmTime true 
                Logging.info "%s завершён" what
            Logging.info "Термоциклирование : перевод термокамеры в НКУ"
            do! TermoNorm.Warm()  
            do! Delay.perform "выдержка НКУ после термоциклирования" warmTime true                 
        }

    let read () = "Считывание температуры" -->> fun () -> maybeErr{        
        let! (t,stp) = Hardware.Termo.read ()
        ModalMessage.show Logging.Info 
            (sprintf "Температура %M\"C\nУставка %M\"C" t stp)
            "Температура термокамеры" 
        return! None }

    let private (-->>) s f = 
        s -->> fun () ->
            f () |> Result.someErr

    let start() = "Старт" -->> Hardware.Termo.start
    
    let stop() = "Стоп" -->> Hardware.Termo.stop
    
    let setSetpoint value = "Уставка" -->> fun () -> 
        Hardware.Termo.setSetpoint value

let testConnect _ = 
    "Проверка связи" <|> fun () -> 
        let oks, errs =
            [   if appCfg.Hardware.Pneumoblock.Enabled then
                    yield "Пневмоблок", Hardware.Pneumo.switch 0uy
                if appCfg.Hardware.Termochamber.Enabled then
                    yield "Термокамера", Hardware.Termo.stop() 
                if appCfg.Hardware.WarmDevice.Enabled then
                    yield "Устройство подогрева плат", Hardware.WarmingBoard.off()
                for p in party.Products do
                    yield p.What, p.ReadModbus(ReadVar Conc) |> Result.map(fun _ -> () ) ]
            |> List.partition (snd >> Result.isOk)
        if errs.IsEmpty then None else
        errs 
        |> Seq.toStr "\n" (fun (what, err) -> sprintf "%s : %s" what (Result.Unwrap.err err) )
        |> Some
    |> Thread2.run false     
