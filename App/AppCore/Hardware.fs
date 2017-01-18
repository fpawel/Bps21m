module Hardware 
open System
open System.Text

open WinFormsControlUtils

type private R = Mdbs.Request
let private cfg = AppConfig.config

module Pneumo =

    let clapan = Ref.Observable(None)

    let private codeToString = function
        | 0uy -> "отключить"
        | ValueInRange (1uy,4uy) true as code -> sprintf "ПГС%d" code
        | code -> sprintf "%X" code

    let isOpened() = 
        match clapan.Value with
        | None 
        | Some (Ok 0uy) -> false
        | _ -> true
        
    
    let switch (code:byte)  = 
        
        let port = cfg.Hardware.Pneumoblock.Comport
        let req = 
            {   R.addy = cfg.Hardware.Pneumoblock.Addr
                R.cmd = 0x10uy
                R.data = [| 0uy; 0x10uy; 0uy; 1uy; 2uy; 0uy; code |]
                R.what = codeToString code |> sprintf "пневмоблок - %s" }
        let r = Mdbs.getResponse port req (fun _ -> "OK") ( fun _ ->  Ok code )
        clapan.Value <- Some r
        match r with
        | Ok _ -> sprintf "%d" code, None
        | Err e -> "Ошибка", Some e
        |> MainWindow.HardwareInfo.peumo.setTextSafe r.Level 
        r |> Result.map(fun _ -> ())


module WarmingBoard =
    type WarmingBoardState = 
        | On | Off
        static member what = function
            | On -> "включение" 
            | _ -> "выключение"


        static member d = function
            | On ->  [3uy; 0xE8uy] 
            | Off -> [0uy; 0uy]

        static member fromBool = function
            | true ->  On
            | _ -> Off

        member x.WhatShort = 
            match x with
            | On -> "вкл." 
            | _ -> "выкл."

    let state = Ref.Observable(None)

    let switch newstate =
        let request = 
            {   R.addy = cfg.Hardware.WarmDevice.Addr
                R.cmd = 0x10uy
                R.data = 
                    [   yield! [0uy;0uy;0uy;1uy;2uy ] 
                        yield! WarmingBoardState.d newstate ]
                R.what = WarmingBoardState.what newstate }
        let r =
            Mdbs.getResponse cfg.Hardware.WarmDevice.Comport request (fun _ -> "Ok") <| function
                | [  0x00uy; 0x00uy; 0x00uy; 0x01uy ] -> Ok ()
                | xs -> bytesToStr xs |> sprintf "не соответствие формата ответа : %A"  |> Err
        state.Value <- Some ( r |> Result.map(fun _ -> newstate) ) 

        match r with
        | Ok _ -> newstate.WhatShort, None
        | Err e -> "Ошибка", Some e
        |> MainWindow.HardwareInfo.peumo.setTextSafe r.Level 

        r

    let private (|OvenStateFromBool|) = WarmingBoardState.fromBool


    let on() = switch On
    let off() = switch Off

    let private (|LessOn|) t = t < cfg.Hardware.WarmDevice.TempOn 
    let private (|GreatOff|) t = t > cfg.Hardware.WarmDevice.TempOff

    let private (|InOnOff|) = function
        | LessOn false & GreatOff false -> true
        | _ -> false

    let update currentTemperature = 
        match state.Value, currentTemperature with
        | Some (Ok On), InOnOff true -> Ok ()
        | Some (Ok Off), InOnOff false -> Ok ()
        | _, InOnOff (OvenStateFromBool value) -> 
            switch value 

module Termo =   

    type TermoState = 
        | Start | Stop | Setpoint of decimal
        static member what = function
            | Start -> "cтарт"            
            | Stop -> "cтоп"
            | Setpoint(v) -> sprintf "уставка %g\"C" v

        member x.What = TermoState.what x

    type Request =  
        | Write of TermoState
        | Read
        static member what = function
            | Write s -> s.What.ToUpper()
            | _ -> "запрос температуры"
        static member requestString = function
            | Read -> "01RRD,02,0001,0002"
            | Write Start -> "01WRD,01,0101,0001"
            | Write Stop -> "01WRD,01,0101,0004"
            | Write (Setpoint v) -> 
                let s = v*10m |> int |> sprintf "%X"
                let s1 = 
                    let len = s.Length
                    if len=4 then s else        
                    if len>4 then s.Substring(len-4, 4) else
                    String.init (4-len) (fun _ -> "0") + s  
                "01WRD,01,0102," + s1 

    let state = Ref.Observable(None)
    let temperature = Ref.Observable(None)

    [<AutoOpen>]
    module private Helpers = 
        let bytesToAscii = Seq.toArray >> System.Text.Encoding.ASCII.GetString

        let config = AppConfig.config
        let getResponse1 req = 
            let scmd = Request.requestString req
            let port = config.Hardware.Termochamber.Comport
            let result = 
                [|  yield 2uy
                    yield! sprintf "%s\r\n" scmd |> Text.Encoding.ASCII.GetBytes |]    
                |> Comport.getResponse port
                |> Result.map bytesToAscii
            match req with
            | Write _ ->                
                Logging.write 
                    (if Result.isOk result then Logging.Info else Logging.Error) 
                    "Термокамера, %s, %s, %s : %A" (Request.what req) 
                    port.PortName scmd result
            | _ -> ()

            result

    
        let formatError req responseString = 
            //if notKeepRunning() then ignore() else
            if String.IsNullOrEmpty responseString then 
                "Термокамера не отвечает"
            else
                sprintf "Неправильный формат ответа термокамеры. %A -> %s." req responseString

        let validWriteResponse = sprintf "%c01WRD,OK\r\n" '\u0002'
        let checkWriteResponse s = if s = validWriteResponse then Ok() else Err "invalid response on write"
    
        type Regex = System.Text.RegularExpressions.Regex

        let parseTemperature response =
            let m = Regex.Match(response, "01RRD,OK,([0-9a-fA-F]{4,4}),([0-9a-fA-F]{4,4})\r\n$")
            if m.Success && m.Groups.Count=3 |> not then Err "can't parse responsed temperature (1)" else 
            let (~%%) (n:int) = Hex.tryParse m.Groups.[n].Value
            match %% 1, %% 2 with 
            | Some t, Some setpoint -> Ok( (decimal t) / 10m, (decimal setpoint) / 10m )
            | _ -> Err "can't parse responsed temperature (2)"



        let getResponse2<'a> request (parse : string -> Result<'a,string>) = 
            getResponse1 request
            |> Result.bind ( fun response -> 
                parse response
                |> Result.mapErr( fun err ->                     
                    sprintf "%s, %s" err <| formatError request response ) )

    let read() = 
        let r = getResponse2 Read parseTemperature
        temperature.Value <- Some r
        
        match r with
        | Ok (x,y) -> sprintf "%M (%M)" x y, None
        | Err e -> "Ошибка", Some e
        |> MainWindow.HardwareInfo.termo.setTextSafe r.Level 

        r

    let write newstate = 
        let r = getResponse2 (Write newstate) checkWriteResponse
        let r1 = Result.map(fun _ -> newstate ) r
        state.Value <- Some r1

        match r with
        | Ok () -> newstate.What, None
        | Err e -> "Ошибка", Some e
        |> MainWindow.HardwareInfo.termo.setTextSafe r.Level 

        r

    let start() = write Start

    let stop() = write Stop

    let setSetpoint setpoint = result {
        Logging.info "Уставка термокамеры %M" setpoint
        do! stop()
        do! write (Setpoint setpoint)
        return! start() }
   
module Warm =
    
    type S = {
        destT : decimal
        startTime : DateTime }

    let rec private loopWarm s isKeepRunning work = result {
    
        if (not <| isKeepRunning()) then return! Err "прервано" else
        let! (temperature,setPointTemperature) = Termo.read()
        do!
            if cfg.Hardware.WarmDevice.Enabled then
                WarmingBoard.update temperature
            else 
                Ok ()
        if abs( s.destT - temperature ) < cfg.Hardware.Termochamber.SetpointErrorLimit then
            return temperature 
        else
            if DateTime.Now - s.startTime > cfg.Hardware.Termochamber.SetpointTimeOut then
                return! Err <| sprintf "таймаут %A" cfg.Hardware.Termochamber.SetpointTimeOut
            else
                do! work()
                return! loopWarm s isKeepRunning work }

    let warm destTemperature isKeepRunning work = 
        result {
            Logging.info "Начало прогрева %M\"C" destTemperature
            do! Termo.setSetpoint destTemperature
            let! resTemp = loopWarm {destT = destTemperature; startTime = DateTime.Now} isKeepRunning work        
            Logging.info "Прогрев %M\"C завершён с температурой %M\"C" destTemperature resTemp 
            return () } 
        |> Result.mapErr(fun err -> 
            Logging.error "Прогрев %M\"C завершён с ошибкой : %s" destTemperature err
            err )
        