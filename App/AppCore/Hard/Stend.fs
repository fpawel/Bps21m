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
    sprintf "%s %s питание" powerState.What powerType.What

let setPower powerType powerState n =
    
    let firstreg = 
        match powerType with
        | PowerMain -> 0x31
        | PowerReserve -> 0x32

    let a = 
        match powerState with
        | PowerOn -> 0x01uy
        | PowerOff -> 0x00uy

    let what = sprintf "#%d: стенд: %s %s питание" n powerState.What powerType.What

    Mdbs.write16 
        comportConfig 
        (whatDoPower powerState powerType) 
        (addrbyte n) 
        firstreg 
        [|0uy; a|]

let readCurrent n = 
    Mdbs.read3decimal comportConfig (addrbyte n) 0 (sprintf "#%d: стенд: ток" n)

let readTension n = 
    Mdbs.read3decimal comportConfig (addrbyte n) 2 (sprintf "#%d: стенд: напряжение" n) 

type Rele = 
    {   Status : bool
        Failure  : bool
        SpMode : bool        
        Porog1 : bool
        Porog2 : bool
        Porog3 : bool 
    }

let readRele n = 
    Mdbs.read3decimal comportConfig (addrbyte n) 4 (sprintf "#%d: стенд: реле" n) 
    |> Result.map( fun value ->
        let b = byte value
        {   SpMode  = not <| b.Bit Byte.Bit5
            Failure = not <| b.Bit Byte.Bit4
            Porog3  = not <| b.Bit Byte.Bit3
            Porog2  = not <| b.Bit Byte.Bit2
            Porog1  = not <| b.Bit Byte.Bit1
            Status  = not <| b.Bit Byte.Bit0  } )

let setCurrent n current =
    let xs, whatCurr = 
        match current with
        | I_4mA ->  [| 0x03uy; 0x0Auy |], "4"
        | I_20mA -> [| 0x0Duy; 0x6Buy |], "20"
    let what = sprintf "#%d: стенд: установка тока %s мА" n whatCurr
    Mdbs.write16 comportConfig what (addrbyte n) 0x30 xs


type Cmd = 
    | SetPower of PowerType * PowerState
    | SetCurrent of Current

    member cmd.Perform n =
        match cmd with
        | SetCurrent current -> setCurrent n current
        | SetPower ( powerType, powerState) ->
            setPower powerType powerState n            

    member x.What = 
        match x with
        | SetPower ( powerType, powerState) ->             
            whatDoPower powerState powerType
        | SetCurrent current -> sprintf "установить ток %M мА" current.Current

    static member MainPowerOn = 
        SetPower(PowerMain, PowerOn)

    static member MainPowerOff = 
        SetPower(PowerMain, PowerOff)

    static member Set4mA = 
        SetCurrent I_4mA

    static member Set20mA = 
        SetCurrent I_20mA