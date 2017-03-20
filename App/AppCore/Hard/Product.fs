module Bps21.Hard.Product

open System
open Bps21

let comportConfig = AppConfig.config.Comport

let private read3decimal n regn what = 
    Mdbs.read3decimal comportConfig n regn what

let readCurrent addy  =
    read3decimal addy 2 "ток БПС"



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
        | [ ByteStatus status ;_; Byte.Bits(p3,_,_,_,_,_,_,_); Byte.Bits(p1,_,_,_,p2,_,_,_)]  ->
            Ok (status, p1, p2, p3, bytesToStr bytes)
        | BytesToStr s -> Err ( sprintf "неожиданный ответ %A" s) )


type Cmd =
    | Adjust of ScalePoint
    | SetAddr
    | SetBoudRate
    | SetPorog of NPorog * PorogTriggerType
    | SetTuneMode of ScalePoint
    | Tune of ScalePoint 
    static member values = 
        [   Adjust ScaleBeg
            Adjust ScaleEnd
            SetBoudRate
            SetPorog (NPorog1, PorogInc)
            SetPorog (NPorog1, PorogDec)
            SetPorog (NPorog2, PorogInc)
            SetPorog (NPorog2, PorogDec)
            SetPorog (NPorog3, PorogInc)
            SetPorog (NPorog3, PorogDec)
            SetTuneMode ScaleBeg
            SetTuneMode ScaleEnd
            Tune ScaleBeg
            Tune ScaleEnd
        ]

    static member context = function  
        | SetAddr ->            0x3E00, "БПС: установка сетевого адреса"
        | SetBoudRate ->        0x3F00, "БПС: установка скорости обмена"
        | Adjust ScaleBeg ->       0x0100, "БПС: корректировка 4 мА"        
        | Adjust ScaleEnd ->      0x0200, "БПС: корректировка 20 мА"
        | SetTuneMode ScaleBeg ->  0x0600, "БПС: режима подстройки 4 мА"
        | Tune ScaleBeg ->         0x0700, "БПС: подстройка 4 мА"
        | SetTuneMode ScaleEnd -> 0x0800, "БПС: режим подстройки 20 мА"
        | Tune ScaleEnd ->        0x0900, "БПС: подстройка 20 мА"
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
            (x <<< 8) + y, sprintf "БПС: установка порога %d на %s" (th.Order + 1) s1

    static member what = Cmd.context >> snd
    static member code = Cmd.context >> fst
    member x.What = Cmd.what x
    member x.Code = Cmd.code x

    member x.Perform n answerRequired value  = 
        Mdbs.write comportConfig n x.Code x.What answerRequired value

    static member PerformSetAddr addr = 
        SetAddr.Perform 0uy addr

    static member Adjust4mA = 
        (Adjust ScaleBeg, 4m)

    static member Adjust20mA = 
        (Adjust ScaleEnd, 20m)

