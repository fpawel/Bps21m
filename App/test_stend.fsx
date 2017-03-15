#I "../packages"
#I "../packages/FParsec.1.0.2/lib/net40-client/"
#r "FParsec.dll"
#r "FParsecCS.dll"

#I "../packages/FsPickler.2.3.0/lib/net40/"
#r "FsPickler.dll"


#load "Utils/FsharpRuntime.fs"
#load "Utils/State.fs"

#load "Utils/StrUtils.fs"
#load "Utils/PathUtils.fs"
#load "Utils/DateTimeUtils.fs"
#load "Utils/Logging.fs"
#load "Utils/Utils.fs"

#load "Json/Json.fs" 
#load "Json/JsonSerialization.fs" 
#load "Json/JsonConfig.fs" 

#I "../MyWinForms/bin/Release"
#r "MyWinForms.dll"
#load "WinForms/WinFormsConverters.fs" 
#load "AppCore/ComportConfig.fs" 

#load "AppCore/Comport.fs" 
#load "AppCore/AppConfig.fs" 

#load "Utils/Bin.fs" 
#load "Utils/Hex.fs" 
#load "AppCore/Mdbs.fs" 
#load "AppCore/BPS21m.fs"
#load "AppCore/Hard/Product.fs" 
#load "AppCore/Hard/Stend.fs" 

Comport.Ports.getPortsNames

open Bps21.Hard  
open System

Stend.comportConfig.PortName <- "COM8"
Stend.comportConfig.CanLog <- true
Stend.comportConfig.BaudRate <- 9600

let crc16 (s:string) =
    s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map(fun x -> 
        Convert.ToByte(x, 16) )
    |> Mdbs.CRC16.get 
    |> printfn "%x"
        

crc16 "21 04 31 00 01 57 66"
crc16 "21 03 20 10 10 61 9F 51"

while true do
    printfn "%A" <| Stend.readCurrent 1uy

setPower PowerMain PowerOn 1
setCurrent 1 I_4mA

Mdbs.write comportConfig 0uy 5 "установка адреса" 1m
Mdbs.read3decimal comportConfig 1uy 0 "считать конц."     


System.Convert.ToString(238,2)
