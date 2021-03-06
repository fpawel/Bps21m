﻿module Bps21.Hard.Stend

open System
open Bps21


let comportConfig = AppConfig.config.Comport
    
[<AutoOpen>]
module private Helpers =
    let getResponse n cmd data what =
        Mdbs.getResponse
            AppConfig.config.Comport
            {   addy = 0x20uy + (byte n)
                cmd = cmd
                data = data
                what = what }

    let addrbyte n = (0x20uy +  n) 

type PowerState = 
    | PowerOn 
    | PowerOff
    member x.What = 
        match x with
        | PowerOn -> "включить"
        | PowerOff -> "выключить"   

    static member fromBool = function 
        | true ->  PowerOn
        | _ -> PowerOff

type PowerType = 
    | PowerMain 
    | PowerReserve
    member x.What =
        match x with
        | PowerMain -> "основное"
        | PowerReserve -> "резервное"

let whatDoPower (powerState : PowerState) (powerType : PowerType) =
    sprintf "СТЕНД: %s %s питание" powerState.What powerType.What

let setPower powerType powerState n =
    
    let firstreg = 
        match powerType with
        | PowerMain -> 0x31
        | PowerReserve -> 0x32

    let a = 
        match powerState with
        | PowerOn -> 0x01uy
        | PowerOff -> 0x00uy

    let what = sprintf "#%d: %s %s питание" n powerState.What powerType.What

    Mdbs.write16 
        comportConfig 
        (whatDoPower powerState powerType) 
        (addrbyte n) Mdbs.AnswerRequired 
        firstreg 
        [|0uy; a|]

let read3 n addr what = 
    Mdbs.read3decimal comportConfig (addrbyte n) addr (sprintf "СТЕНД %d: %s" n what)

let readCurrent n = 
    if (n < 1) || (n > 5) then Err (sprintf "n=%d: must be 1..5" n) else
    let xs = [0x04; 0x0A; 0x10; 0x16; 0x1C]
    let addr = 0x10uy
    let reg = xs.[n-1]
    Mdbs.read3float comportConfig addr reg (sprintf "СТЕНД %d: считывание тока" n)
    |> Result.map( (*) 1.24M )
    
let readTensionOpenCircuit n =
    read3 n 2 "напряжение линии питания датчика без нагрузки" 

let readTensionLoad n r =
    let s = sprintf "напряжение линии питания датчика под нагрузкой %M Ом" r
    match r with
    | 68m -> read3 n 6 s
    | 91m -> read3 n 8 s
    | _ -> Err ( sprintf "недопустимое значение R, %s" s)

let loadLine addr r = 
    let code, what = 
        match r with
        | 68m -> 0x42, "подключить нагрузку 68 Ом"
        | 91m -> 0x44, "подключить нагрузку 91 Ом"
        | _   -> 0x46, "отключить нагрузку"
    Mdbs.write16 
        comportConfig 
        (sprintf "СТЕНД %d: %s" addr what) 
        (addrbyte addr) Mdbs.AnswerRequired 
        code [|0uy; 0uy|]

type Rele = 
    {   Status : bool
        Failure  : bool
        SpMode : bool        
        Porog1 : bool
        Porog2 : bool
        Porog3 : bool  
    }
    member x.What =
        let (~%%) a = if a then "замкнуто" else "разомкнуто" 
        sprintf """ СТАТУС:%s ОТКАЗ:%s СПЕЦ.РЕЖИМ:%s П1:%s П2:%s П3:%s""" 
            (%% x.Status)
            (%% x.Failure)
            (%% x.SpMode)
            (%% x.Porog1)
            (%% x.Porog2)
            (%% x.Porog3)

let readRele n = 
    Mdbs.read3 
        comportConfig 
        (sprintf "#%d: стенд: реле" n)  
        (addrbyte n) 4 1
        (sprintf "%A")
        (function 
            | [_;b] ->
                {   SpMode  = not <| b.Bit Byte.Bit5
                    Failure = not <| b.Bit Byte.Bit4
                    Porog3  = not <| b.Bit Byte.Bit3
                    Porog2  = not <| b.Bit Byte.Bit2
                    Porog1  = not <| b.Bit Byte.Bit1
                    Status  = not <| b.Bit Byte.Bit0  } 
                |> Ok
            | BytesToStr s -> 
                Err <| sprintf "не верный формат ответа на запрос реле %s" s )

let setCurrent n current =    
    let b1,b2 = AppConfig.config.Stend.GetDI n current    
    Mdbs.write16 
        comportConfig 
        (sprintf "#%d: СТЕНД: установить ток %M мА" n current.Value)
        (addrbyte n) 
        Mdbs.AnswerRequired 0x30 
        [|b1; b2|]

let turnCurrentOff n =    
    Mdbs.write16 
        comportConfig 
        (sprintf "#%d: СТЕНД: отключить ток" n )
        (addrbyte n) 
        Mdbs.AnswerRequired 0x30 
        [|0uy; 0uy|]
   
type Cmd = 
    | SetPower of PowerType * PowerState
    | SetCurrent of Bps21.Current
    | TurnCurrentOff
    | LoadLine of decimal

    member cmd.Perform n =
        match cmd with
        | SetCurrent current -> setCurrent n current
        | SetPower ( powerType, powerState) ->
            setPower powerType powerState n            
        | TurnCurrentOff ->
            turnCurrentOff n

        | LoadLine r ->
            loadLine n r
        
    member x.What = 
        match x with
        | SetPower ( powerType, powerState) ->             
            whatDoPower powerState powerType
        | SetCurrent current -> sprintf "СТЕНД: установить ток %M мА" current.Value
        | TurnCurrentOff -> "СТЕНД: отключить ток" 
        | LoadLine rLine ->
            match rLine with
            | 68m -> 
                "подключить нагрузку 68 Ом"
            | 91m -> 
                "подключить нагрузку 91 Ом"
            | _ -> 
                "отключить нагрузку"
        

    static member MainPowerOn = 
        SetPower(PowerMain, PowerOn)

    static member MainPowerOff = 
        SetPower(PowerMain, PowerOff)

    static member ReservePowerOn = 
        SetPower(PowerReserve, PowerOn)

    static member ReservePowerOff = 
        SetPower(PowerReserve, PowerOff)

    static member Set4mA = 
        (SetCurrent I4)

    static member Set12mA = 
        (SetCurrent I12)

    static member Set20mA = 
        (SetCurrent I20)

    static member Load68 = 
        (LoadLine 68m)

    static member Load91 = 
        (LoadLine 91m)

    static member UnloadLine = 
        (LoadLine 0m)

    static member values = 
        [   for pt in [PowerMain; PowerReserve] do     
                for ps in [PowerOn; PowerOff] do 
                    yield SetPower(pt,ps)
            for v in [ I4; I12; I20] do
                yield SetCurrent v
            yield  LoadLine 68m
            yield  LoadLine 91m
            yield  LoadLine 0m
        ]