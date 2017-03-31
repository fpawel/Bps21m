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
    | BlockInc
    | BlockDec
    | NonblockInc
    | NonblockDec
    static member values = [ BlockInc; BlockDec; NonblockInc; NonblockDec]

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
            Ok (status, p1, p2, p3)
        | BytesToStr s -> Err ( sprintf "неожиданный ответ %A" s) )


type Cmd =
    | Adjust of ScalePoint
    | SetAddr
    | SetBoudRate
    | SetPorog of NPorog * PorogTriggerType
    | SetTuneMode of ScalePoint
    | Tune of ScalePoint 
    | CustomCmd of int
    static member values1 = 
        [   yield Adjust ScaleBeg
            yield Adjust ScaleEnd
            yield SetBoudRate
            yield SetTuneMode ScaleBeg
            yield SetTuneMode ScaleEnd
            yield Tune ScaleBeg
            yield Tune ScaleEnd
        ]

    static member context = function  
        | CustomCmd code ->
            code, sprintf "команда %X %X" (code >>> 8) code
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
                | NonblockInc -> 0x0, "повышение, реле не блокируется" 
                | NonblockDec -> 0x1, "понижение, реле не блокируется"
                | BlockInc -> 0x10, "повышение, реле блокируется" 
                | BlockDec -> 0x11, "понижение, реле блокируется"
            let y = 
                match th with
                | NPorog1 -> 0x10
                | NPorog2 -> 0x11
                | NPorog3 -> 0x12
            (y <<< 8) + x, sprintf "БПС: установка порога %d на %s" (th.Order + 1) s1

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

let private (|Bytes2|) = 
    List.map fst >> bytesToStr

let private tryGetID m = 
    match Map.tryFind 4uy m, Map.tryFind 5uy m  with 
    | Some kind, Some serial -> Ok (kind,serial)
    | None, Some _ -> 
        sprintf "ObjectID 4 not found in %A" m |> Err 
    | Some _, None -> 
        sprintf "ObjectID 5 not found in %A" m |> Err 
    | _ -> 
        sprintf "ObjectIDs 4,5 not found in %A" m |> Err 

let rec private parseID acc = function
    | [] -> Ok acc
    | (bp,_) :: (len_,nLen) :: xs  ->
        let len = int len_
        if len = 0 then 
            sprintf "длина строки [%d] = 0, %A" nLen acc
            |> Err
        elif xs.Length < len then
            sprintf "длина строки [%d] = %d больше длины среза [%d..] = %d, %A" 
                nLen len (nLen+1) xs.Length acc
            |> Err
        else
            let x =
                Seq.take len xs
                |> Seq.map fst
                |> Seq.toArray
                |> String.FromWindows1251Bytes
            parseID ( (bp,x)::acc ) (List.drop len xs)
    | Bytes2 str ->
        sprintf "%s - не соответсвует образцу, %A" 
            str acc
        |> Err



let readID addy =
    // 03 2b 0e 02 03 crc crc
    Mdbs.getResponse 
        comportConfig 
        {   addy = addy
            cmd = 0x2Buy
            data = [0x0Euy; 2uy; 3uy; ]
            what  = "чтение идентификационных данных"}
            (fun _ -> "")
            (   List.drop 6 
                >> List.mapi(fun n x -> x,n )
                >> parseID []  
                >> Result.map Map.ofList
                >> Result.bind tryGetID )

let setID addy kind serial =
    let bytesKind = String.ToWindows1251Bytes kind
    let bytesSerial = String.ToWindows1251Bytes serial
    let data =
        [   yield 4uy
            yield byte bytesKind.Length 
            yield! bytesKind
            yield 5uy
            yield byte bytesSerial.Length 
            yield! bytesSerial ]
    
    if data.Length > 167 then 
        sprintf 
            "для записи идентификационных данных %A %A требуется %d байт, а доступно 167" 
                kind serial data.Length
        |> Err 
    else
        Mdbs.getResponse
            comportConfig 
            {   addy = addy
                cmd = 0x42uy
                data = 
                    [   yield! 
                            [   0uy
                                0x2Buy
                                0uy
                                byte (data.Length / 2 + data.Length % 2)
                                byte data.Length
                            ]
                        
                        yield! data
                    ]
                what  = "запись идентификационных данных"}
                (fun _ -> "")
                (fun _ -> Ok () )

