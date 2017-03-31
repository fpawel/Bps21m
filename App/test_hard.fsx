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
#load "AppCore/Bps21m.fs"
#load "AppCore/AppConfig.fs" 

#load "Utils/Bin.fs" 
#load "Utils/Hex.fs" 
#load "AppCore/Mdbs.fs" 

#load "AppCore/Hard/Product.fs" 
#load "AppCore/Hard/Stend.fs" 

open Bps21.Hard  
open System

Stend.comportConfig.PortName <- "COM2"
Stend.comportConfig.CanLog <- true
Stend.comportConfig.BaudRate <- 9600

open Bps21.Hard.Stend
open Bps21.Hard.Product

//Mdbs.write comportConfig 0uy 5 "установка адреса" Mdbs.AnswerNotRequired 3m  

setPower PowerMain PowerOn 1uy
 

readID 1uy    
setID 1uy "АНКАТ" "99009988"
readID 1uy

let parse (s:string) =
    s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map(fun x -> Convert.ToByte(x, 16) )

let bx1 = parse "42 00 2B 00 13 26 04 10 C1 CF D1 2D 32 31 CC 33 2D 32 34 F5 32 34 2D D0 05 12 20 C7 E0 E2 2E 20 B9 20 31 30 30 20 31 2E 32 30 31 36"
let bx2 = [| yield 3uy; yield! bx1 |]
let bx = Mdbs.CRC16.add bx2

Comport.getResponse comportConfig bx



(*
Запись идкнтификационных данных не работает.


До записи

*)