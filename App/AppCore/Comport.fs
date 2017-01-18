module Comport

open System
open System.IO.Ports
open ComportConfig

module Ports =
    let mutable serialPorts : Map<string,SerialPort * string > = Map.empty
    
    let getPort config = 
        match serialPorts.TryFind config.PortName with
        | Some (port,descr) when descr<> config.Description ->             
            serialPorts <- Map.add config.PortName (port,config.Description) serialPorts
            Ok port
        | Some (port,_) -> Ok port
        | _ ->
            try
                let newport = new SerialPort(config.PortName)
                serialPorts <- Map.add config.PortName (newport,config.Description) serialPorts
                Ok newport
            with e -> Err e.Message               

    let getPortByName portName =        
        match serialPorts |> Map.tryFind portName with
        | Some (port,_) -> Ok port
        | _ ->  
            try
                new SerialPort(portName) |> Ok
            with e -> Err e.Message

    let removeFailedPort config = 
        serialPorts <- serialPorts |> Map.filter( fun p' _ -> config.PortName <> p')
        getPort config

    let getPorts =  
        serialPorts |> Map.toList |> List.map( fun( portName, (_,descr)) -> portName, descr)

    let getPortsNames = ""::(SerialPort.GetPortNames() |> Array.toList)
    let closeOpenedPorts( pred ) =  
        serialPorts 
        |> Map.filter( fun portName (port, descr) -> 
            port.IsOpen 
            &&  SerialPort.GetPortNames() |> Set.ofArray |> Set.contains portName 
            && (pred portName descr) )
        |> Map.iter ( fun _ (port, _) -> 
            Logging.debug "Закрытие порта %s" port.PortName
            port.Close() )
        serialPorts <- Map.empty


[<AutoOpen>]
module private Helpers2 =     
    let formatError canLog (portName : string) (msg : string) txd = 
        if msg.Contains portName then msg else 
        sprintf "COM порт %A, %s%s" 
            portName msg 
            (if canLog then sprintf ", %s" <| bytesToStr txd else "")

    let tickcount() = System.Environment.TickCount

    let sleep (n:int) = 
        let x0 = tickcount()
        while tickcount() - x0 < n  do
            System.Threading.Thread.Sleep 10

    let discardBuffers (port:SerialPort) = 
        port.DiscardInBuffer()
        port.DiscardOutBuffer()

    let private doApply (sets : Config) (port : SerialPort) = 
        port.Close()
        port.PortName <- sets.PortName
        port.BaudRate <- sets.BaudRate     
        port.Open()
        discardBuffers port
        Ok port

    let applyPort sets = 
        if sets.PortName |> String.IsNullOrEmpty then
            (sprintf "%A : не установлен СОМ порт" sets.Description) |> Err
        elif SerialPort.GetPortNames() |> Set.ofArray |> Set.contains sets.PortName |> not then 
            (sprintf "Недопустимое имя СОМ порта %A" sets.PortName ) |> Err else
        match Ports.getPort sets with
        | Err x -> Err x
        | Ok port ->
            if port.IsOpen && port.PortName<>sets.PortName && port.BaudRate<>sets.BaudRate then Ok port else                
            try doApply sets port with e1 ->
            try
                match Ports.removeFailedPort sets with
                | Err x -> Err x
                | Ok port -> doApply sets port
            with e2 ->
                Logging.error "При открытии порта %A возникли исключительные ситуации\n(1) %A\n(2) %A" 
                    sets.PortName e1 e2
                if e1.Message <> e2.Message then sprintf "%s, %s" e1.Message e2.Message else e1.Message
                |> sprintf "Не удалось открыть порт %s: %s" sets.PortName
                |> Err 

    let writeToPort (port:SerialPort) (txd:byte seq)  =        
        let txd = Seq.toArray txd
        port.Write( txd, 0, txd.Length)

    let checkCondWithTimeout timeout cond  work = 
        let t0 = tickcount()
        let rec loop() = 
            work()
            if  (cond() |> not) &&  (tickcount() - t0 < timeout) then 
                loop() 
        loop()

let tryApplyPort = applyPort

let testPort x =
    match tryApplyPort x with 
    | Err error -> 
        Logging.error "Не удалось открыть порт %s - %s" x.PortName error
        Some error 
    | _ -> None
    
   
let write sets (txd:byte seq)  =
    let fail error = formatError sets.CanLog sets.PortName error txd |> Some
    try
        match tryApplyPort sets with
        | Err error -> fail error
        | Ok port ->
            if sets.CanLog then bytesToStr txd |> Logging.info "%s <== %s" sets.PortName 
            let txd = Seq.toArray txd
            port.Write( txd, 0, txd.Length)
            None
    with e ->
        fail e.Message

[<AutoOpen>]
module private Helpers3 =   

    type Request =  {
        serial : SerialPort
        config : Config
        txd : byte []
        n : int }


    let rec loopRecive req recived = 
        let hasBytes() = req.serial.BytesToRead>0
        checkCondWithTimeout req.config.Timeout hasBytes ( fun() -> sleep 1 )
        if req.serial.BytesToRead=0 then Ok recived else
        let rxdlen = req.serial.BytesToRead
        let oldlen = recived |> Array.length
        let recived = [| yield! recived; yield! Array.create rxdlen 0uy |]            
        if req.serial.Read(recived, oldlen, rxdlen) <> rxdlen then 
            Err <| sprintf "error reading comport %s, %s" req.serial.PortName (bytesToStr req.txd)
        else
            checkCondWithTimeout (max req.config.Chartime 1) hasBytes ( fun() -> sleep 5 )
            if req.serial.BytesToRead=0 then 
                Ok recived 
            else 
                loopRecive req recived

    let rec loopTransmit req =  
        writeToPort req.serial req.txd
        let tickStart = tickcount()
        let response = loopRecive req [||]
        let ms = tickcount()-tickStart
        if req.config.CanLog then
            let whatRecived = 
                match response with
                | Err e -> e
                | Ok [||] -> "нет ответа"
                | Ok rxd -> bytesToStr rxd 
            Logging.debug "%s, %s ==> %s, %d мс" req.config.PortName (bytesToStr req.txd) whatRecived ms
        response 
        |> Result.bind( function
            | [||] when req.n < req.config.RepeatCount -> 
                loopTransmit { req with n = req.n + 1 }
            | [||] -> Ok [||] 
            | rxd ->
                sleep req.config.Delay
                Ok rxd )

let getResponse port requestBytes : Result<byte[], string>=         
    try
        match tryApplyPort port with
        | Err error -> formatError port.CanLog port.PortName error requestBytes |> Err
        | Ok serial ->
            loopTransmit { config = port; serial = serial; n =  0; txd =  requestBytes }            
    with e ->
        formatError port.CanLog port.PortName e.Message requestBytes |> Err

let getProtocolResponse port requestBytes checkResp parseResp =
    match getResponse port requestBytes with
    | Err error -> None, Err error
    | Ok responsedBytes -> 
        Some responsedBytes, 
            checkResp responsedBytes  
            |> Result.bind parseResp