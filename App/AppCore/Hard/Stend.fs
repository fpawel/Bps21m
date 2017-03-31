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

let read3 n addr what = 
    Mdbs.read3decimal comportConfig (addrbyte n) addr (sprintf "СТЕНД %d: %s" n what)

let readCurrent n = 
    read3 n 0 "ток"
    
let readTensionOpenCircuit n =
    read3 n 2 "напряжение линии питания датчика без нагрузки" 

let readTensionLoad n rLoad =
    let reg = 
        match rLoad with
        | RLoadLine68 -> 6
        | RLoadLine91 -> 8
    read3 n reg (sprintf "напряжение линии питания датчика под нагрузкой %M Ом" rLoad.Value) 


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

    member cmd.Perform n =
        match cmd with
        | SetCurrent current -> setCurrent n current
        | SetPower ( powerType, powerState) ->
            setPower powerType powerState n            
        | TurnCurrentOff ->
            turnCurrentOff n
        
    member x.What = 
        match x with
        | SetPower ( powerType, powerState) ->             
            whatDoPower powerState powerType
        | SetCurrent current -> sprintf "СТЕНД: установить ток %M мА" current.Value
        | TurnCurrentOff -> "СТЕНД: отключить ток" 
        

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

    static member values = 
        [   for pt in [PowerMain; PowerReserve] do     
                for ps in [PowerOn; PowerOff] do 
                    yield SetPower(pt,ps)
            for v in [ I4; I12; I20] do
                yield SetCurrent v
        ]