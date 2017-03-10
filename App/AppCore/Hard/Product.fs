module Bps21.Hard.Product

open System
open Bps21

let comportConfig = AppConfig.config.Comport

let private read3decimal n regn what = 
    Mdbs.read3decimal comportConfig n regn what

let readCurrent addy  =
    read3decimal addy 2 "цифровой канал: показания: ток "



// тип срабатывания порога
type PorogTriggerType = 
    | PorogInc
    | PorogDec
    static member values = [ PorogInc; PorogDec]

type NPorog= 
    | NPorog1
    | NPorog2
    | NPorog3
    static member values = [ NPorog1; NPorog2; NPorog3]
    
    member x.Order = NPorog.GetOrder x

    static member GetOrder = function
        | NPorog1 -> 0
        | NPorog2 -> 1
        | NPorog3 -> 2

type Status = 
    | Norm | Failure | SpMode
    member  x.What = 
        match x with
        | Norm      -> "НОРМ"
        | Failure   -> "ОТКАЗ"
        | SpMode    -> "СП.Р."

let (|ByteStatus|_|) = function    
    | 0uy -> Some Norm
    | 1uy -> Some Failure
    | 2uy -> Some SpMode
    | _ -> None

let readStatus n = 
    Mdbs.read3bytes comportConfig n 0 2
    |> Result.bind(fun bytes ->
        match bytes with 
        | [ ByteStatus status ;_; Byte.Bits(p1,_,_,_,p2,_,_,_);Byte.Bits(p3,_,_,_,_,_,_,_)]  ->                      
            Ok (status, p1, p2, p3, bytes)
        | BytesToStr s -> Err ( sprintf "неожиданный ответ %A" s) )


type Cmd =
    | Adjust of Current
    | SetAddr
    | SetPorog of NPorog * PorogTriggerType
    | SetTuneMode of Current
    | Tune of Current * int
    static member context = function  
        | SetAddr -> 0x3E00, "Установка сетевого адреса"
        | Adjust I_4mA -> 1, "Корректировка 4 мА"        
        | Adjust I_20mA -> 2, "Корректировка 20 мА"
        | SetTuneMode I_4mA -> 6, "Переключение режима подстройки 4 мА"
        | SetTuneMode I_20mA -> 8, "Переключение режима подстройки 20 мА"
        | Tune (I_4mA,n) -> (n <<< 8) + 7, sprintf "Подстройка 4 мА на %d" (n+1)
        | Tune (I_20mA,n) -> (n <<< 8) + 9, sprintf "Подстройка 20 мА на %d" (n+1)
        | SetPorog (th,tt) -> 
            let x,s1 = 
                match tt with
                | PorogInc -> 0, "повышение" 
                | _ -> 1, "понижение"
            let y = 
                match th with
                | NPorog1 -> 3
                | NPorog2 -> 4
                | NPorog3 -> 10
            (x <<< 8) + y, sprintf "Установка порога %d на %s" (th.Order + 1) s1

    static member what = Cmd.context >> snd
    static member code = Cmd.context >> fst
    member x.What = Cmd.what x
    member x.Code = Cmd.code x

    member x.Perform n value = 
        Mdbs.write comportConfig n x.Code x.What value

    static member PerformSetAddr addr = 
        SetAddr.Perform 0uy addr

    static member Adjust4mA = 
        (Adjust I_4mA, 4m)

    static member Adjust20mA = 
        (Adjust I_20mA, 20m)

