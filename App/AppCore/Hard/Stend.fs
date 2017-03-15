module Bps21.Hard.Stend

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

let readCurrent n = 
    Mdbs.read3decimal comportConfig (addrbyte n) 0 (sprintf "#%d: ток СТЕНД" n)

let readTension n = 
    Mdbs.read3decimal comportConfig (addrbyte n) 2 (sprintf "#%d: напряжение СТЕНД" n) 

type Rele = 
    {   Status : bool
        Failure  : bool
        SpMode : bool        
        Porog1 : bool
        Porog2 : bool
        Porog3 : bool  }

    member x.What =
        let (~%%) a = if a then "замкнуто" else "разомкнуто" 
        sprintf """ СТАТУС:%s ОТКАЗ:%s СПЕЦ.РЕЖИМ:%s П1:%s П2:%s П3:%s""" 
            (%% x.Status)
            (%% x.Failure)
            (%% x.SpMode)
            (%% x.Porog1)
            (%% x.Porog2)
            (%% x.Porog3)

    static member failureMode =
        {   Status  = true
            Failure = true
            SpMode  = false                        
            Porog3  = false
            Porog2  = false
            Porog1  = false } 

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
    let xs, whatCurr = 
        match current with
        | Some I_4mA ->  [| 0x03uy; 0x0Auy |], "установить ток 4 мА"
        | Some I_20mA -> [| 0x0Duy; 0x6Buy |], "установить ток 20 мА"
        | _ -> [| 0uy; 0uy |], "отключить ток"
    let what = sprintf "#%d: СТЕНД: %s" n whatCurr
    Mdbs.write16 comportConfig what (addrbyte n) Mdbs.AnswerRequired 0x30 xs


type Cmd = 
    | SetPower of PowerType * PowerState
    | SetCurrent of Current option

    member cmd.Perform n =
        match cmd with
        | SetCurrent current -> setCurrent n current
        | SetPower ( powerType, powerState) ->
            setPower powerType powerState n            

    member x.What = 
        match x with
        | SetPower ( powerType, powerState) ->             
            whatDoPower powerState powerType
        | SetCurrent (Some current) -> sprintf "СТЕНД: установить ток %M мА" current.Current
        | SetCurrent None -> "СТЕНД: отключить ток"

    static member MainPowerOn = 
        SetPower(PowerMain, PowerOn)

    static member MainPowerOff = 
        SetPower(PowerMain, PowerOff)

    static member Set4mA = 
        SetCurrent (Some I_4mA)

    static member Set20mA = 
        SetCurrent (Some I_20mA)

    static member values = 
        [   for pt in [PowerMain; PowerReserve] do     
                for ps in [PowerOn; PowerOff] do 
                    yield SetPower(pt,ps)
            yield SetCurrent (Some I_4mA)
            yield SetCurrent (Some I_20mA)
            yield SetCurrent None
        ]