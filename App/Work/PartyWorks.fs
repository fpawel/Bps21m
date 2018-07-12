module Bps21.PartyWorks

open System

open Thread2

open ViewModel.Operations
open Bps21.Hard.Product

[<AutoOpen>]
module private Helpers = 
    type P = Bps21.ViewModel.Product
    let party = AppContent.party
    let prodType() = (fst party.Party).ProductType   

        

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

    type CmdStend = Hard.Stend.Cmd
    type Rele = Hard.Stend.Rele
    type NPorog = Bps21.NPorog
    



type Bps21.ViewModel.Product with
    static member TestRead (x:Bps21.ViewModel.Product) = 
        if appCfg.InterrogateProducts then 
            maybeErr {
                let! _ = x.ReadProductCurrent()
                let! _ = x.ReadStendCurrent()
                let! {Porog1 = p1; Porog2 = p2; Porog3 = p3} as a = x.ReadStendRele()
                let! _,p1_,p2_,p3_  as b = x.ReadProductStatus()
                if (p1,p2,p3) <> (p1_,p2_,p3_) then
                    Logging.error "несоответствие данных порогов, прибор: %A, стенд: %A" b a       
                return! None }  
        else 
            maybeErr {
                let! _ = x.ReadStendCurrent()
                let! _ = x.ReadStendRele()
                return! None  }  
type Bps21.ViewModel.Party with

    member x.FailProductionIfConnError prodPt = 
        x.Products 
        |> Seq.filter(fun p ->  p.IsChecked )
        |> Seq.iter(fun p -> p.FailProductionIfConnectionError prodPt)

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

    member x.SetProduction (prodPoint:ProductionPoint) work = 
        x.DoForEachProduct( fun (product:P) -> 
            let r = 
                work product
                |> Option.map ( fun s -> false, s )
                |> Option.withDefault (true, "успешно") 
            if notKeepRunning() then () else
            match product.Connection with
            | Some (Err err) -> 
                (false,err)
            | _ -> r            
            |> product.SetProduction prodPoint 
        )

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

     member x.MainPowerOn() = maybeErr{        
        do! x.DoForEachProduct (fun product -> 
            maybeErr{
                do! x.DoForEachProduct (fun p -> 
                    Hard.Stend.SetPower(Hard.Stend.PowerMain, Hard.Stend.PowerOn)
                    |> p.WriteStend 
                    |> ignore )
            } |> ignore         
        )   
    }

    member x.SetNetAddrs() = maybeErr{        
        do! x.DoForEachProduct (fun product -> 
            maybeErr{
                do! x.DoForEachProduct (fun p -> 
                    let on = 
                        obj.ReferenceEquals(p,product)
                        |> Hard.Stend.PowerState.fromBool 
                    Hard.Stend.SetPower(Hard.Stend.PowerMain, on)
                    |> p.WriteStend 
                    |> ignore )
                do! sleep 5000
                
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

    member x.SetNetAddrs1() = maybeErr{        
        do! x.DoForEachProduct (fun product -> 
            maybeErr{
                do! x.DoForEachProduct (fun p -> 
                    let on = 
                        obj.ReferenceEquals(p,product)
                        |> Hard.Stend.PowerState.fromBool 
                    Hard.Stend.SetPower(Hard.Stend.PowerMain, on)
                    |> p.WriteStend 
                    |> ignore )
                do! sleep 5000
                
                do! Hard.Product.SetAddr.Perform 0uy Mdbs.AnswerNotRequired 1m
                do! sleep 200
                let! _ = Hard.Product.readCurrent 1uy
                return ()
            } |> ignore
        )   
    }

    member x.SwitchOffRS485() = maybeErr{        
        do! x.DoForEachProduct (fun product -> 
            product.SwitchOffRS485()
            |> ignore
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

    let simpleTest x ok (s:string) = 
        if x then Ok ok else Err s

    let test1 x s = simpleTest x () s

    let testCurrentError what current nominal errorLimit =
        let d = abs (current - nominal)
        let errorLimit = abs errorLimit
        Logging.debug "%s : %M мА. Должен быть %M ± %M" what current nominal errorLimit
        test1 
            (d < errorLimit) 
            (sprintf "%s : %M мА. Должен быть %M ± %M" what current nominal errorLimit)

    let testReleState (rele:Rele) (mustRele:Rele) = 
        test1 
            (rele = mustRele ) 
            ( ReleInfo.diffHtml rele mustRele)

 

let adjust = Bps21.Adjust.What <|> fun () ->maybeErr {
    let t = prodType()
    if not t.Tune then return! None else
    
    do! party.WriteStend CmdStend.Set4mA
    do! pause 1
    do! party.WriteProducts Cmd.Adjust4mA
    do! pause 1
    do! party.WriteStend CmdStend.Set20mA
    do! pause 1
    do! party.WriteProducts Cmd.Adjust20mA
    do! pause 1
       
    do! party.SetProduction Bps21.Adjust (fun p -> 
        maybeErr{
            let! U = p.ReadTensionOpen()
            if t.Uout = 16m || t.Uout=24m then 
                let umin = t.Uout - t.DUcc1
                let umax = t.Uout + t.DUcc2
                do! test1
                        (U >= umin && U <= umax)
                        (sprintf "контроль Uл %M...%M: %M" umin umax U)
        } 
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


let testTuneMode (p:P) = maybeErr{        
    let! rele = p.ReadStendRele()
    let a = rele.Status, rele.SpMode, rele.Failure
    let b = true, true,false
    if a <> b then
        return! Some <| sprintf "СТАТУС, СП.РЕЖИМ, ОТКАЗ: %A, должно быть %A" a b 
    }

let tuneProduct (product:P) scalePoint  getTimeout =  maybeErr{        
    product.Connection <- None
    let tuneErrorLimit = 
        match scalePoint with
        | ScaleBeg -> appCfg.TuneI4
        | ScaleEnd -> appCfg.TuneI20
        |> abs
    let In = scalePoint.Current.Value      
    
    let mutable current = 0m
    let readCurrent() = maybeErr{
        let! a = product.ReadStendCurrent()
        current <- a
    }

    let isOK() = 
        abs(current - In) < tuneErrorLimit

    let mutable d = 50m
    let tune() = maybeErr{        
        if d > 999990m then 
            d <- 999990m
        elif d < -999990m then 
            d <- -999990m
        elif d = 0m then
            d <- 50m
        do! product.WriteProduct ( Cmd.Tune scalePoint, d )
        do! sleep 100
    }

    do! readCurrent()
    if isOK() then return! None else
    let mutable I1 = current       
    do! tune()    
    do! readCurrent()
    let mutable I2 = current    

    let startTime = DateTime.Now
    let testTimeout() = 
        if DateTime.Now - startTime  > getTimeout() then
            Some <| sprintf "Превышен лимит времени подстройки %A" (getTimeout()) 
        else 
            None 

    while (not <| isOK()) do
        do! testTimeout()        
        do! testTuneMode product
        let sign = if current > In then -1m else 1m
        if I2 = I1 then 
            do! Some <| sprintf "I2 = I1 = %M" I1
        d <- sign * abs(  d * (In - I2) / (I2 - I1) )
        do! tune()
        do! readCurrent()
        Logging.info "I=%M I1=%M I2=%M  d=%M " current I1 I2  d
        I1 <- I2
        I2 <- current
}

let tune scalePoint = 
    let prod = Bps21.Tune scalePoint
    ( prod.What, _2minute, DelayTune) <-|-> fun getTimeLimit -> 
        
        let t = prodType()

        if not t.Tune then None else

        let tuneResult = maybeErr{
            do! party.WriteStend (CmdStend.SetCurrent scalePoint.Current)
            do! pause 5
            do! party.WriteProducts (Cmd.SetTuneMode scalePoint, scalePoint.Current.Value)
            do! pause 5
            do! party.DoForEachProduct( fun product ->                 
                match tuneProduct product scalePoint  getTimeLimit  with
                | None -> product.SetProduction prod (true,"успешно") 
                | Some err -> product.SetProduction prod (false,err) )
            do! party.WriteProducts (Cmd.SetTuneMode scalePoint, scalePoint.Current.Value)
            do! pause 1
        }
        party.FailProductionIfConnError prod
        tuneResult

let testAlarmFailure = 
    
    let mustRele : Rele =
        {   Status  = true
            Failure = true
            SpMode  = false                        
            Porog3  = false
            Porog2  = false
            Porog1  = false } 
    
    TestAlarmFailure.What <|> fun () -> maybeErr{
        do! party.WriteStend CmdStend.TurnCurrentOff
        do! pause 10
        do! party.SetProduction  TestAlarmFailure ( fun product -> 
            maybeErr{
                let! rele = product.ReadStendRele()
                do! test1 
                        (rele = mustRele) 
                        ( ReleInfo.diffHtml rele mustRele )
                let! current = product.ReadStendCurrent()
                do! test1
                        (current <= 2m)
                        (sprintf "ток выхода %M мА, должен быть не более 2 мА" current )
                let! (status,_,_,_) = product.ReadProductStatus()
                do! test1
                        (status = Hard.Product.Failure)
                        (sprintf "статус %s, должен быть %s" status.What Hard.Product.Failure.What )
                return ()
                }  )
        party.FailProductionIfConnError TestAlarmFailure
    }

let testLoadCapacity = 
    let mustRele : Rele = 
        {   Status  = true
            Failure = false
            SpMode  = false
            Porog1  = true
            Porog2  = true
            Porog3  = true }  
    "Проверка нагрузочной способности" <|> fun () -> maybeErr{
        let prodType = prodType()
        if not prodType.LoadCapacity then return! None else
        do! party.WriteStend CmdStend.Set20mA
        do! pause 5
        // подключить нагрузку
        do! prodType.ExplosionProtection.R
            |> CmdStend.LoadLine
            |> party.WriteStend 
        do! pause 5

        do! party.SetProduction Bps21.LoadCapacity (fun product -> 
            maybeErr{
               
                let! i = product.ReadStendCurrent()
                do! testCurrentError "ток выхода, стенд" i 20m 0.2m 

                let! i = product.ReadProductCurrent()
                do! testCurrentError "ток выхода, цифровой канал" i 20m 0.2m

                let! U = product.ReadTensionOpen()
                let u1,u2 = prodType.Uout - prodType.DUcc1, prodType.Uout + prodType.DUcc2
                do! test1
                        (U >= u1 && U <= u2)
                        (sprintf "контроль Uл %M...%M: %M" u1 u2 U)                
            } 
        )
        
        // отключить нагрузку
        do! party.WriteStend CmdStend.UnloadLine
        do! pause 5

        party.FailProductionIfConnError LoadCapacity
    }

//let mainPowerOn = 
//    ("Подача основного питания", _2minute, DelayPowerOn) <-|-> fun getTime -> maybeErr{
//        do! party.WriteStend CmdStend.MainPowerOn
//        do! sleep (getTime().TotalMilliseconds |> int)
//    }

    

let setupPorogs (p1,p2,p3) = maybeErr{
    for n,v in List.zip NPorog.values [p1; p2; p3] do    
        if prodType().Porog n then      
            do! party.WriteProducts (Cmd.SetPorog(n, NonblockInc), v)    
}
let testPorog nporog = 
    
    let porogValues, prodPoint, mustP1, mustP2, mustP3 = 
        match nporog with
        | NPorog1 -> (11.76m, 12.48m, 12.62m), TestPorog1, true, false, false
        | NPorog2 -> (11.52m, 11.76m, 12.48m), TestPorog2, true, true, false
        | NPorog3 -> (11.28m, 11.52m, 11.76m), TestPorog3, true, true, true

    let mustRele : Rele = 
        {   Status  = true
            Failure = false
            SpMode  = false
            Porog1  = mustP1
            Porog2  = mustP2
            Porog3  = mustP3 }    
    prodPoint.What <|> fun () -> maybeErr{

        if not (prodType().Porog nporog) then return! None else
        
        do! party.WriteStend CmdStend.Set4mA
        do! pause 10
        do! setupPorogs porogValues
        do! pause 1
        do! party.WriteStend CmdStend.Set12mA
        do! pause 10
        do! party.SetProduction prodPoint ( fun product ->   
            maybeErr{
                let! rele = product.ReadStendRele()
                do! testReleState rele mustRele
                let! i = product.ReadStendCurrent()
                do! testCurrentError "ток выхода, стенд" i 12m 0.19m 
                let! ip = product.ReadProductCurrent()
                do! testCurrentError "ток выхода, цифровой канал" ip 12m 0.19m 
                let! _,p1,p2,p3 = product.ReadProductStatus()
                do! test1 
                        ((p1,p2,p3) = (mustP1, mustP2, mustP3)) 
                        (sprintf "признак срабатывания порогов (цифровой канал): %b,%b,%b. Должно быть: %b,%b,%b" 
                            p1 p2 p3 mustP1 mustP2 mustP3) 
                do! test1
                        (rele.Porog1=mustP1)
                        (sprintf "ПОРОГ 1: %b, должен быть: %b" rele.Porog1 mustP1) 
                do! test1
                        (rele.Porog2=mustP2)
                        (sprintf "ПОРОГ 1: %b, должен быть: %b" rele.Porog2 mustP2) 
                do! test1
                        (rele.Porog3=mustP3)
                        (sprintf "ПОРОГ 3: %b, должен быть: %b" rele.Porog3 mustP3) }
        )    
    }

let test4mA (product:P) = 
    maybeErr{
        let! i = product.ReadStendCurrent()
        do! testCurrentError "ток выхода, стенд" i 4m 0.04m 
        let! ip = product.ReadProductCurrent()
        do! testCurrentError "ток выхода, цифровой канал" i 4m 0.04m 
        let! rele = product.ReadStendRele()
        do! testReleState rele 
                {   Status  = true
                    Failure = false
                    SpMode  = false
                    Porog1  = false
                    Porog2  = false
                    Porog3  = false }
        }


let testReservedPower = 
    ReservedPower.What <|> fun () -> maybeErr{
        if not (prodType().ReservedPower) then return! None else         
        do! party.WriteStend CmdStend.Set4mA    
        do! pause 2
        do! party.WriteStend CmdStend.ReservePowerOn
        do! pause 2
        do! party.WriteStend CmdStend.MainPowerOff
        do! pause 10
        do! party.DoForEachProduct ( test4mA >> ignore)
        do! party.WriteStend CmdStend.MainPowerOn
        do! pause 2
        do! party.WriteStend CmdStend.ReservePowerOff
        do! pause 10
        do! party.SetProduction ReservedPower test4mA
    }


let writeIDs = 
    "Запись серийных номеров" <|> fun () -> 
        
        maybeErr{ 
            do! party.DoForEachProduct <| fun  p ->
                maybeErr{
                    do! p.WriteID()                    
                } |> ignore

            do! pause 10

            do! party.DoForEachProduct <| fun  p ->
                maybeErr{
                    let! a = Hard.Product.readID p.Addr
                    if a <> (p.Kind, p.SerialQuarterYear) then
                        Logging.error "%d: ошибка записи серийного номера: записано %A, считано %A"  
                            p.Addr (p.Kind, p.SerialQuarterYear) a
                } |> ignore
        }
                

let readIDs = 
    "Считывание серийных номеров" <|> fun () -> 
        party.DoForEachProduct <| fun  p ->
            p.ReadID() |> ignore
        |> Result.someErr

let main() = 
    let prodType = prodType()
    let test x = simpleTest x ()
    "Настройка БПС-21М3" <||> [

        
        yield ("Установка сетевых адресов") <|> fun _ -> maybeErr{
            do! party.SetNetAddrs()
            do! pause 10
        }

        yield ("Подача питания") <|> fun _ -> maybeErr{
            do! party.MainPowerOn()
            do! pause 10
        }

        yield ("Прогрев", TimeSpan.FromMinutes 5., DelayPowerOn) <-|-> fun getTime -> maybeErr{
            do! Delay.perform  "Прогрев" getTime false 
        }
                    
        
        yield writeIDs
        yield readIDs
        if prodType.Tune then
            yield adjust  
            yield tune ScaleBeg
            yield tune ScaleEnd
        if prodType.LoadCapacity then 
            yield testLoadCapacity
        yield testAlarmFailure
        
        if prodType.Porog1 then 
            yield testPorog NPorog1
        if prodType.Porog2 then 
            yield testPorog NPorog2
        if prodType.Porog3 then 
            yield testPorog NPorog3
        
        if prodType.ReservedPower then 
            yield  testReservedPower

        if prodType.Porog1 || prodType.Porog2 || prodType.Porog3 then
            yield "Установка порогов" <|> fun () -> 
                setupPorogs (5.6m, 7.2m, 18.4m)

        yield ("Установка сетевого адреса 1") <|> fun _ -> maybeErr{
            do! party.SetNetAddrs1()
            do! pause 10
        }

        if prodType.Ucc = 220M then
            yield ("Отключение RS-485") <|> fun _ -> maybeErr{
                do! party.SwitchOffRS485()
            }


    ] |> withСonfig

let all() = 
    Op.MapReduce Some (main())

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