module Mdbs

open Bin
open Comport

module private CRC16 = 

    let private auchCRCHi = 
        [   0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy;
            0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy;
            0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy;
            0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy;
            0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy;
            0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy; 0xC0uy;
            0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy;
            0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy;
            0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy;
            0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy;
            0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy;
            0xC0uy; 0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy;
            0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy;
            0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy;
            0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy;
            0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy;
            0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy;
            0x40uy ]

    let private auchCRCLo = 
        [   0x00uy; 0xC0uy; 0xC1uy; 0x01uy; 0xC3uy; 0x03uy; 0x02uy; 0xC2uy; 0xC6uy; 0x06uy; 0x07uy; 0xC7uy; 0x05uy; 0xC5uy; 0xC4uy;
            0x04uy; 0xCCuy; 0x0Cuy; 0x0Duy; 0xCDuy; 0x0Fuy; 0xCFuy; 0xCEuy; 0x0Euy; 0x0Auy; 0xCAuy; 0xCBuy; 0x0Buy; 0xC9uy; 0x09uy;
            0x08uy; 0xC8uy; 0xD8uy; 0x18uy; 0x19uy; 0xD9uy; 0x1Buy; 0xDBuy; 0xDAuy; 0x1Auy; 0x1Euy; 0xDEuy; 0xDFuy; 0x1Fuy; 0xDDuy;
            0x1Duy; 0x1Cuy; 0xDCuy; 0x14uy; 0xD4uy; 0xD5uy; 0x15uy; 0xD7uy; 0x17uy; 0x16uy; 0xD6uy; 0xD2uy; 0x12uy; 0x13uy; 0xD3uy;
            0x11uy; 0xD1uy; 0xD0uy; 0x10uy; 0xF0uy; 0x30uy; 0x31uy; 0xF1uy; 0x33uy; 0xF3uy; 0xF2uy; 0x32uy; 0x36uy; 0xF6uy; 0xF7uy;
            0x37uy; 0xF5uy; 0x35uy; 0x34uy; 0xF4uy; 0x3Cuy; 0xFCuy; 0xFDuy; 0x3Duy; 0xFFuy; 0x3Fuy; 0x3Euy; 0xFEuy; 0xFAuy; 0x3Auy;
            0x3Buy; 0xFBuy; 0x39uy; 0xF9uy; 0xF8uy; 0x38uy; 0x28uy; 0xE8uy; 0xE9uy; 0x29uy; 0xEBuy; 0x2Buy; 0x2Auy; 0xEAuy; 0xEEuy;
            0x2Euy; 0x2Fuy; 0xEFuy; 0x2Duy; 0xEDuy; 0xECuy; 0x2Cuy; 0xE4uy; 0x24uy; 0x25uy; 0xE5uy; 0x27uy; 0xE7uy; 0xE6uy; 0x26uy;
            0x22uy; 0xE2uy; 0xE3uy; 0x23uy; 0xE1uy; 0x21uy; 0x20uy; 0xE0uy; 0xA0uy; 0x60uy; 0x61uy; 0xA1uy; 0x63uy; 0xA3uy; 0xA2uy;
            0x62uy; 0x66uy; 0xA6uy; 0xA7uy; 0x67uy; 0xA5uy; 0x65uy; 0x64uy; 0xA4uy; 0x6Cuy; 0xACuy; 0xADuy; 0x6Duy; 0xAFuy; 0x6Fuy;
            0x6Euy; 0xAEuy; 0xAAuy; 0x6Auy; 0x6Buy; 0xABuy; 0x69uy; 0xA9uy; 0xA8uy; 0x68uy; 0x78uy; 0xB8uy; 0xB9uy; 0x79uy; 0xBBuy;
            0x7Buy; 0x7Auy; 0xBAuy; 0xBEuy; 0x7Euy; 0x7Fuy; 0xBFuy; 0x7Duy; 0xBDuy; 0xBCuy; 0x7Cuy; 0xB4uy; 0x74uy; 0x75uy; 0xB5uy;
            0x77uy; 0xB7uy; 0xB6uy; 0x76uy; 0x72uy; 0xB2uy; 0xB3uy; 0x73uy; 0xB1uy; 0x71uy; 0x70uy; 0xB0uy; 0x50uy; 0x90uy; 0x91uy;
            0x51uy; 0x93uy; 0x53uy; 0x52uy; 0x92uy; 0x96uy; 0x56uy; 0x57uy; 0x97uy; 0x55uy; 0x95uy; 0x94uy; 0x54uy; 0x9Cuy; 0x5Cuy;
            0x5Duy; 0x9Duy; 0x5Fuy; 0x9Fuy; 0x9Euy; 0x5Euy; 0x5Auy; 0x9Auy; 0x9Buy; 0x5Buy; 0x99uy; 0x59uy; 0x58uy; 0x98uy; 0x88uy;
            0x48uy; 0x49uy; 0x89uy; 0x4Buy; 0x8Buy; 0x8Auy; 0x4Auy; 0x4Euy; 0x8Euy; 0x8Fuy; 0x4Fuy; 0x8Duy; 0x4Duy; 0x4Cuy; 0x8Cuy;
            0x44uy; 0x84uy; 0x85uy; 0x45uy; 0x87uy; 0x47uy; 0x46uy; 0x86uy; 0x82uy; 0x42uy; 0x43uy; 0x83uy; 0x41uy; 0x81uy; 0x80uy;
            0x40uy ]

    let get bytes = 
        let fcrc (hi,uchCRCLo) (b:byte) =
            let i = int (hi ^^^ b)
            let hi = uchCRCLo ^^^ auchCRCHi.[i]
            let lo = auchCRCLo.[i]
            (hi,lo)
        let hi,lo = Array.fold  fcrc (0xFFuy,0xFFuy)  bytes 
        (uint16 hi<<<8)+(uint16 lo)

    let add bytes = 
        let u = get bytes
        [|  yield! bytes 
            yield byte(u >>> 8)
            yield  byte u  |]

type Request = {
    addy : byte
    cmd : byte
    data : byte seq 
    what : string}

type Notification = {
    Request : Request
    Port : ComportConfig.Config
    Response : byte [] option
    Result : Result<string,string> }


[<AutoOpen>]
module private Helper = 
    type NotificationEvent = Event<Notification> 

    //let commPort = AppConfig.config.ComportProducts

    let notify = new Event<_>()
    let requestBytes x =
        [|  yield byte x.addy
            yield byte x.cmd 
            yield! x.data |] 
        |> CRC16.add

    let checkResponse request (respnseBytes:byte [])  = 
        let requestBytes = requestBytes request
        let len = Array.length respnseBytes 
        if request.addy <> 0uy && len=0 then Err "не отвечает"
        elif len<4 then Err ( sprintf "несоответствие дины ответа %d" len ) else
        let crc16 = CRC16.get respnseBytes
        if crc16>0us then Err ( sprintf "ненулевая crc16 %x"  crc16 ) else
        let rxd = respnseBytes.[0..(len-3)] |> Array.toList     
        match rxd with
        | b::_ when b <> byte request.addy -> Err ( sprintf "несовпадение адреса %d" request.addy )
        | _ :: b :: errorCode::[] when b=( byte request.cmd ||| 0x80uy) -> Err ( sprintf "код ошибки %d" errorCode )
        | _::b::_ when b <> byte request.cmd -> Err ( sprintf "несовпадение кода команды %d" request.cmd )
        | _::_::data -> Ok data
        | _ -> Err "неизвестный формат ответа"

    let tell (ntf : Notification ) =
        let port = ntf.Port
        let req = ntf.Request
        let cmd = req.cmd
        let level, result, needLog = 
            match ntf.Result with
            | Ok x -> Logging.Info, x, port.CanLog || cmd = 0x10uy || cmd = 0x16uy
            | Err e -> Logging.Error, e, true

        notify.Trigger ntf
        
        if needLog then             
            Logging.write level "%s %s адр.%d, %s, ком.%d%s" 
                port.PortName "MODBUS" req.addy req.what req.cmd 
                (if result = "" then "" else " ==> " + result )                 
        

    
let getResponse port request formatResult parseResponse  =
    let requestBytes = requestBytes request
    let response,result = getProtocolResponse port requestBytes (checkResponse request) parseResponse        
    tell
        {   Request = request
            Port = port
            Response = response
            Result = Result.map formatResult result }
        
    result

[<AutoOpen>]
module private Helpers1 = 

    
    let sendBroadcast port request = 
        let r = 
            match Comport.write port (requestBytes request) with
            | Some e -> Err e
            | _ -> Ok ()
        tell
            {   Request = request
                Port = port
                Response = None
                Result = Result.map (fun _ -> "Ok") r }
        r

    let write16 port what addy firstRegister dt =
        let len = dt |> Array.length
        let registersCount = len / 2
        let dx = 
            [|  (uint16 firstRegister ) >>> 8 |> byte
                firstRegister |> byte
                (uint16 registersCount ) >>> 8 |> byte
                byte registersCount |]
        let data =             
            [|  yield! dx
                yield byte len 
                yield! dt |]
        let request = 
            {   cmd = 0x10uy
                addy = addy
                data  = data
                what  = what }        
        
        if addy=0uy then sendBroadcast port request else
            getResponse port request (fun x -> "")  <| function
                |  [ x0; x1; x2; x3 ] as xs when xs = Array.toList dx.[0..3]  -> Ok ()
                | _ -> Err "Неверный формат ответа %s"
        

    let read3<'a> port what addy registerNumber registersCount formatResult parse : Result<'a, string>=
        let data = 
            [|  registerNumber >>> 8
                registerNumber
                registersCount >>> 8
                registersCount |]
            |> Array.map byte

        let request = 
            {   cmd = 3uy
                addy = addy
                data  = data
                what  = what }

        getResponse port request formatResult <| function    
            | _::rxd when rxd.Length=int(registersCount)*2  -> parse rxd
            | rx -> Err "Неверный формат ответа"

[<CLIEvent>]
let NotifyEvent = notify.Publish

let write port addy cmd what value =
    let what = sprintf "%s <-- %s" what ( System.Decimal.toStr6 value)
    [|  yield byte (cmd >>> 8)
        yield byte cmd
        yield! Bin.decimalToAnalitBCD6 value |]
    |> write16 port what addy 32

let read3bytes port addy registerNumber registersCount  =
    read3 port 
        ( sprintf "запрос %d-%d регистров" registerNumber (registerNumber + registersCount - 1) ) 
        addy
        registerNumber 
        registersCount 
        bytesToStr
        Ok

let read3decimal port addy registerNumber what =
    read3 port what addy registerNumber 2 (sprintf "%M") <| function
        | Bin.AnalitBCD6 v -> Ok v
        | BytesToStr x -> Err <| sprintf "Ошибка преобразования BCD %s" x


let private (|ConvList|) f = List.map f
let private (|NIn|) xs n = 
    let len = List.length xs
    if n>=0 && n<len then Some xs.[n] else None 

let private readOneReg port addy n = 
    read3 port (sprintf "рег.%d" n) addy n 1 (sprintf "0x%x") <| function
        | ConvList int [b1; b2] -> 
            Ok (b1 * 256 + b2)
        | BytesToStr x -> Err ( sprintf "в ответ на запрос одного регистра МОДБАС ожидалось два байта данных, но получено %A" x )

let readStatus =
    let reg35s =
            [   "норма"
                "ПОРОГ 1"
                "ПОРОГ 2"
                "отказ" ]
    let reg36s =
        [   "измерение"
            "корр-ка \"нуля\""
            "корр-ка чувст."
            "настройка"
            "сохр. ПОРОГ 1"
            "сохр. ПОРОГ 2" ]    
    fun port addy ->
        read3 port "запрос статуса прибора из регистров 35,36" addy 35 2 (fun (_,_,x) -> x) <| function
            | ConvList int [b1; b2; b3; b4] -> 
                let n1,n2 = b1 * 256 + b2, b3 * 256 + b4
                let s1 = 
                    match n1 with
                    | NIn reg35s (Some s1) as n1 -> s1
                    | _ -> sprintf "%d" n1
                let s2 = 
                    match n2 with
                    | NIn reg36s (Some s2) as n2 -> s2
                    | _ -> sprintf "%d" n2
                Ok(n1,n2, sprintf "%s, %s" s1 s2)
            | BytesToStr bytes -> 
                Err <| sprintf "в ответ на запрос статуса прибора из регистров 35,36 получены некоретктные значения %s " bytes