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

    type CmdProduct = Hard.Product.Cmd
    type CmdStend = Hard.Stend.Cmd
    type Rele = Hard.Stend.Rele



type Bps21.ViewModel.Product with
    static member TestRead (x:Bps21.ViewModel.Product) = maybeErr {
        let! _ = x.ReadProductCurrent()
        let! _ = x.ReadStendCurrent()
        let! {Porog1 = p1; Porog2 = p2; Porog3 = p3} as a = x.ReadStendRele()
        let! _,p1_,p2_,p3_,_  as b = x.ReadProductStatus()
        if (p1,p2,p3) <> (p1_,p2_,p3_) then
            Logging.error "несоответствие данных порогов, прибор: %A, стенд: %A" b a       
        return! None }

type Bps21.ViewModel.Party with
    member x.FailProductionIfConnError prodPt = 
        x.Products 
        |> Seq.filter(fun p ->  p.IsChecked )
        |> Seq.iter(fun p -> p.FailProductionIfConnError prodPt)


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
        x.DoForEachProduct (fun p -> p.WriteStend cmd |> ignore ) 
            |> Result.someErr

    member x.WriteProducts (cmd,value) = 
        x.DoForEachProduct (fun p -> p.WriteProduct (cmd,value) |> ignore ) 
            |> Result.someErr

    member x.SetNetAddrs() = maybeErr{        
        do! x.DoForEachProduct (fun product -> 
            maybeErr{
                do! x.DoForEachProduct (fun p -> 
                    let on = 
                        obj.ReferenceEquals(p,product)
                        |> Hard.Stend.PowerState.fromBool 
                    let cmd = Hard.Stend.SetPower(Hard.Stend.PowerMain, on)
                    p.WriteStend cmd |> ignore )
                
                Hard.Product.comportConfig.BaudRate <- 2400
                do! Hard.Product.SetAddr.Perform 0uy Mdbs.AnswerNotRequired (decimal product.Addr)
                do! sleep 200
                do! Hard.Product.SetBoudRate.Perform product.Addr Mdbs.AnswerNotRequired 9600M
                do! sleep 200
                Hard.Product.comportConfig.BaudRate <- 9600
                
                do! Hard.Product.SetAddr.Perform 0uy Mdbs.AnswerNotRequired (decimal product.Addr)
                do! sleep 200
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
        let f() = party.WriteProducts (cmd,value)
        cmd.What <|> f

    let writeStend cmd = 
        let f() = party.WriteStend cmd 
        cmd.What <|> f

    let pause seconds = 
        let time = TimeSpan.FromSeconds (float seconds)
        Delay.perform 
            (sprintf "Пауза %d с" seconds) 
            (fun () -> time) 
            false

    let delay what time = 
        Delay.perform what (fun () -> time) true

    let delay1 what time = 
        what <|> fun () -> maybeErr{    
            do! Delay.perform what (fun () -> time) true }

    let delay2 what time delayType = 
        (what, time, delayType) <-|-> fun gettime -> 
            Delay.perform what gettime true 

    let _2minute = TimeSpan.FromMinutes 2.
    let _10sec = TimeSpan.FromSeconds 10.

    

    let withСonfig (scenary:Operation) =
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

    let testProdPoint (prodPoint:ProductionPoint) testWork = 
        prodPoint.What <|> fun () -> 
            let r = testWork()
            party.FailProductionIfConnError prodPoint
            r

    let simpleTest x ok (s:string) = 
        if x then Ok ok else Err s

    

let adjust() = maybeErr {
    do! party.WriteStend CmdStend.Set4mA
    do! pause 1
    do! party.WriteProducts CmdProduct.Adjust4mA
    do! pause 1
    do! party.WriteStend CmdStend.Set20mA
    do! pause 1
    do! party.WriteProducts CmdProduct.Adjust20mA
    do! pause 1
    let U_min,U_max = let h,_ = party.Party in h.ProductType.U1            
    do! party.DoForEachProduct(fun p -> 
        maybeErr{
            let! U = p.ReadTensionOpen()
            p.SetProduction 
                Adjust
                (U >= U_min && U <= U_max)
                (sprintf "контроль Uл %M...%M: %M" U_min U_max U)
        } |> ignore
    )
}

let testCurrent nominal (p:P) = result{
    let! stendCurrent = p.ReadStendCurrent()
    let! productCurrent = p.ReadProductCurrent()
    let d = abs (stendCurrent - productCurrent)
    let limit = nominal * 0.005m
    return! 
        simpleTest
            (d < limit )
            stendCurrent
            (   sprintf """Разность тока %M измеренного стендом %M и 
полученного по цифровому каналу %M превысила максимально допустимую %M, 
указанную в настройках приложения"""
                    d stendCurrent productCurrent limit)
    }

let rec tuneProduct (current:ScalePoint) startTime getTimeLimit (p:P) =  maybeErr{        
    let! rele = p.ReadStendRele()
    let a = rele.Status, rele.SpMode, rele.Failure
    let b = true, true,false
    if a <> b then
        return! Some <| sprintf "СТАТУС, СП.РЕЖИМ, ОТКАЗ: %A, должно быть %A" a b else
    let! currentValue = testCurrent current.Current p
    let d = current.Current - currentValue
    if abs d < 0.04m then return! None else
    do! p.WriteProduct ( CmdProduct.Tune current, d )
    do! sleep 100
    return!
            
        if DateTime.Now - startTime > getTimeLimit() then 
            Some <| sprintf "Превышен лимит времени %A" (getTimeLimit()) 
        else
            tuneProduct current startTime  getTimeLimit p 
}

let tune current = 
    let prod = Tune current
    ( prod.What, _2minute, DelayTune) <-|-> fun getTimeLimit -> 
        let tuneResult = maybeErr{
            do! party.WriteStend (CmdStend.SetScalePointCurrent current)
            do! pause 5
            do! party.WriteProducts (CmdProduct.SetTuneMode current, current.Current)
            do! pause 5
            do! party.DoForEachProduct( fun product -> 
                match tuneProduct current DateTime.Now getTimeLimit product with
                | None -> product.SetProduction prod true "ok" 
                | Some err -> product.SetProduction prod false err )
            do! party.WriteProducts (CmdProduct.SetTuneMode current, current.Current)
            do! pause 1
        }
        party.FailProductionIfConnError prod
        tuneResult

let testAlarmFailure = 
    let test x = simpleTest x ()
    testProdPoint TestAlarmFailure <| fun () -> maybeErr{
        do! party.WriteStend CmdStend.SwitchOffCurrent
        do! pause 1
        do! party.DoForEachProduct( fun product -> 
            maybeErr{
                let! rele = product.ReadStendRele()
                do! test 
                        (rele = Rele.failureMode) 
                        ( sprintf "%s, должно быть %s" rele.What Rele.failureMode.What )
                let! current = product.ReadStendCurrent()
                do! test
                        (current <= 2m)
                        (sprintf "ток выхода %M мА, должен быть не более 2 мА" current )
                let! (status,_,_,_,_) = product.ReadProductStatus()
                do! test
                        (status = Hard.Product.Failure)
                        (sprintf "статус %s, должен быть %s" status.What Hard.Product.Failure.What )
                return ()
                }
            |> ignore  )
    }

let testLoadCapacity = 
    let test x = simpleTest x ()
    testProdPoint LoadCapacity <| fun () -> maybeErr{
        do! party.WriteStend (CmdStend.SetScalePointCurrent ScaleBeg)
        do! pause 5
        // ... todo
    }

let mainPowerOn = 
    ("Подача основного питания", _2minute, DelayPowerOn) <-|-> fun getTime -> maybeErr{
        do! party.WriteStend CmdStend.MainPowerOn
        do! Delay.perform "Пауза после подачи осн. питания" getTime true 
    }

let setNetAddrs = 
    "Установка сетевых адресов" <|> fun () -> maybeErr{
        do! party.SetNetAddrs()
        do! party.WriteStend CmdStend.MainPowerOn
    }

let main = 
    "Настройка БПС-21М3" <||> [
        mainPowerOn
        setNetAddrs
        testProdPoint Adjust adjust
        tune ScaleBeg
        tune ScaleEnd
        testAlarmFailure
    ] |> withСonfig

let all = 
    Op.MapReduce Some main 

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
            party.WriteProducts (cmd,value)

    member x.RunSetAddrs() = 
        Thread2.run setNetAddrs
