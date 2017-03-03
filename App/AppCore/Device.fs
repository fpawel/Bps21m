module Device

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
    | PowerOn | PowerOff
    member x.What = 
        match x with
        | PowerOn -> "включить"
        | PowerOff -> "выключить"   

    static member fromBool = function 
        | true ->  PowerOn
        | _ -> PowerOff

type PowerType = 
    | PowerMain | PowerReserve
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

    let what = sprintf "#%d: %s %s питание" n powerState.What powerType.What

    Mdbs.write16 
        comportConfig 
        (whatDoPower powerState powerType) 
        (addrbyte n) 
        firstreg 
        [|0uy; a|]


let readCurrent n = 
    Mdbs.read3decimal comportConfig (addrbyte n) 0 (sprintf "#%d: ток" n)

let readTension n = 
    Mdbs.read3decimal comportConfig (addrbyte n) 2 (sprintf "#%d: напряжение" n) 

type ReleState = 
    {   SpMode : bool
        Failure  : bool
        Porog3 : bool
        Porog2 : bool
        Porog1 : bool 
        Status : bool}

let readReleState n = 
    Mdbs.read3decimal comportConfig (addrbyte n) 4 (sprintf "#%d: реле" n) 
    |> Result.map(fun value ->
        let b = byte value
        {   SpMode  = not <| b.Bit Byte.Bit5
            Failure = not <| b.Bit Byte.Bit4
            Porog3  = not <| b.Bit Byte.Bit3
            Porog2  = not <| b.Bit Byte.Bit2
            Porog1  = not <| b.Bit Byte.Bit1
            Status  = not <| b.Bit Byte.Bit0 }  )


let setCurrent n current =
    let xs, whatCurr = 
        match current with
        | I_4mA ->  [| 0x03uy; 0x0Auy |], "4"
        | I_20mA -> [| 0x0Duy; 0x6Buy |], "20"

    let what = sprintf "#%d: установка тока %s мА" n whatCurr
    Mdbs.write16 comportConfig what (addrbyte n) 0x30 xs

type Conc = 
    {   Porog1 : bool
        Porog2 : bool
        Porog3 : bool
        Value : decimal
    }

let private read3decimalIgumenov port addy registerNumber what  =
    Mdbs.read3 port what addy registerNumber 2 
        (fun (x:Conc) -> 
            sprintf "%M П1:%b П2:%b П3:%b" x.Value x.Porog1 x.Porog2 x.Porog3
            ) 
        (function
            | (Bin.AnalitBCD6 v) & (b :: _ )   -> 
                let (~%%) bit = b.Bit bit
                Ok{ Value = v
                    Porog1 = b.Bit Byte.Bit3
                    Porog2 = b.Bit Byte.Bit4
                    Porog3 = b.Bit Byte.Bit5 }                
            | BytesToStr x -> Err <| sprintf "Ошибка преобразования BCD %s" x )


let readConc n = 
    read3decimalIgumenov comportConfig n 0 "считать показания"  

let private read3decimal n regn what = 
    Mdbs.read3decimal comportConfig n regn what


let readVPorogs n = result {
    let! p1 = read3decimal n 2 "запрос установленного значения порога 1" 
    let! p2 = read3decimal n 4 "запрос установленного значения порога 2" 
    let! p3 = read3decimal n 6 "запрос установленного значения порога 3" 
    return p1,p2,p3
    } 


type Status35 = 
    {   SpMode : bool
        Failure  : bool
        Porog3 : bool
        Porog2 : bool
        Porog1 : bool }

let readStatus35 n = 
    Mdbs.read3bytes comportConfig n 35 1
    |> Result.bind(function 
        | [b;_] -> 
            {   SpMode    = b.Bit Byte.Bit4
                Failure = b.Bit Byte.Bit3
                Porog3  = b.Bit Byte.Bit2
                Porog2  = b.Bit Byte.Bit1
                Porog1  = b.Bit Byte.Bit0
            } |> Ok
        | BytesToStr s -> Err ( sprintf "неожиданный ответ %A" s) )

// тип срабатывания порога
type ThresholdTriggerType = 
    | ThresholdTriggerInc
    | ThresholdTriggerDec
    static member values = [ ThresholdTriggerInc; ThresholdTriggerDec]

type CmdDevice =
    | CmdAdjust of Current
    | CmdAddy
    | CmdThreshold of ThresholdIndex * ThresholdTriggerType
    | CmdSetTuneMode of Current
    | CmdTune of Current * int
    static member context = function  
        | CmdAddy -> 5, "Установка сетевого адреса"
        | CmdAdjust I_4mA -> 1, "Корректировка 4 мА"        
        | CmdAdjust I_20mA -> 2, "Корректировка 20 мА"
        | CmdSetTuneMode I_4mA -> 6, "Переключение режима подстройки 4 мА"
        | CmdSetTuneMode I_20mA -> 8, "Переключение режима подстройки 20 мА"
        | CmdTune (I_4mA,n) -> (n <<< 8) + 7, sprintf "Подстройка 4 мА на %d" (n+1)
        | CmdTune (I_20mA,n) -> (n <<< 8) + 9, sprintf "Подстройка 20 мА на %d" (n+1)
        | CmdThreshold (th,tt) -> 
            let x,s1 = 
                match tt with
                | ThresholdTriggerInc -> 0, "повышение" 
                | _ -> 1, "понижение"
            let y = 
                match th with
                | Th1 -> 3
                | Th2 -> 4
                | Th3 -> 10
            (x <<< 8) + y, sprintf "Установка порога %d на %s" (th.Order + 1) s1

    static member what = CmdDevice.context >> snd
    static member code = CmdDevice.context >> fst

    member x.What = CmdDevice.what x
    member x.Code = CmdDevice.code x

type CmdStend = 
    | SetPower of PowerType * PowerState
    | SetCurrent of Current

    member x.What = 
        match x with
        | SetPower ( powerType, powerState) ->             
            whatDoPower powerState powerType
        | SetCurrent current -> sprintf "установить ток %M мА" current.Current

type Cmd = 
    | CmdStend of CmdStend
    | CmdDevice of CmdDevice * decimal

    static member Send n = function
        | CmdStend (SetPower (powerType,powerState)) ->
            setPower powerType powerState n
        | CmdStend (SetCurrent current) ->
            setCurrent n current
        | CmdDevice(cmd, value) ->
            Mdbs.write comportConfig n cmd.Code cmd.What value

    static member what = function
        | CmdStend x -> x.What
        | CmdDevice(cmd, value) -> sprintf "%s <- %M" cmd.What value

    member x.What = Cmd.what x

    static member MainPowerOn = 
        CmdStend(SetPower(PowerMain, PowerOn))

    static member MainPowerOff = 
        CmdStend(SetPower(PowerMain, PowerOff))

    static member Set4mA = 
        CmdStend(SetCurrent I_4mA)

    static member Set20mA = 
        CmdStend(SetCurrent I_20mA)

    static member Adjust4mA = 
        CmdDevice(CmdAdjust I_4mA, 4m)

    static member Adjust20mA = 
        CmdDevice(CmdAdjust I_20mA, 20m)

    static member SetAddr addr = 
        CmdDevice(CmdAddy, addr)

    static member CommandsStend1 = 
        [   Cmd.MainPowerOn
            Cmd.MainPowerOff
            Cmd.Set4mA
            Cmd.Set20mA
            Cmd.Adjust4mA
            Cmd.Adjust20mA 
        ]

    static member CommandsDevice1 = 
        [   for i in [I_4mA; I_20mA] do
                yield CmdAdjust i
            for i in [I_4mA; I_20mA] do
                yield CmdSetTuneMode i
            for i in [I_4mA; I_20mA] do
                yield CmdSetTuneMode i
            for th in [Th1; Th2; Th3] do
                for tt in [ThresholdTriggerDec; ThresholdTriggerInc] do
                    yield CmdThreshold (th,tt)
        ]

    
    
        

let setAddr addr = 
    Cmd.Send 0uy ( CmdDevice (CmdAddy,addr) )

