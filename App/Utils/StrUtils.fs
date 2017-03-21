[<AutoOpen>]
module StrUtils

open System 
open System.Text.RegularExpressions

[<AutoOpen>]
module private Helpers1 = 
    let rec getUniqueKeyLoop x len = 
        let x = x + Guid.NewGuid().ToString().GetHashCode().ToString("x")
        if x.Length < len then getUniqueKeyLoop x len else x
        

type String with 
    static member getUniqueKey len = 
        getUniqueKeyLoop "" len

    static member md5hash (input : string) =
        let md5 = Security.Cryptography.MD5.Create()    
        input
        |> System.Text.Encoding.ASCII.GetBytes
        |> md5.ComputeHash
        |> Seq.map (fun c -> c.ToString("X2"))
        |> Seq.reduce (+)

    static member replaceComaDot = 
        String.map(fun x -> if x = ',' then '.' else x )

    static member tryParseDecimal s = 
        if s=null then None else
        let sep = System.Globalization.CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator
        match Decimal.TryParse( Regex.Replace( s, "[,\\.]", sep) ) with
        | true, v -> Some v
        | _ -> None

    static member getHashCode (x:string) = x.GetHashCode()

    static member TryParseHexDecimalByte value =
        Byte.TryParse(value,  Globalization.NumberStyles.HexNumber, null )


module Seq =

    let toStr<'T> delimString conv (collection : 'T seq )  = 
        collection |> Seq.fold( fun acc x ->
            acc + (if acc |> String.IsNullOrEmpty then acc else delimString) + (conv x) ) ""

let intToHex len x = 
    let x = sprintf "%X" x
    let n = String.length x
    (if n < len then String('0', len-n ) else "") + x

let bytesToStrings bytes =      
    Seq.fold ( fun (acc,i) b ->
        let s = sprintf "%s%X" (if b<0x10uy then "0" else "") b
        if i%16=0 then (s::acc, i+1) else                     
        match acc with 
        | [] -> ( [s], i+1) 
        | [s'] -> ([ s'+" "+s], i+1)
        | s'::acc  -> ((s'+" "+s)::acc, i+1)) ( [], 0 ) bytes |> fst |> List.rev

let rec intToBin i =
    match i with
    | 0 | 1 -> string i
    | _ ->
        let bit = string (i % 2)
        (intToBin (i / 2)) + bit

let bytesToStr bytes = Seq.fold ( fun acc s -> if acc="" then s else acc + " "+s) "" (bytesToStrings bytes)
let (|BytesToStr|) = bytesToStr


type Decimal with    
    member x.ToStr spec = Decimal.toStr spec x
    member x.ToStr6 = Decimal.toStr6 x


    static member toStr6 value = 
        Decimal.toStr "0.######" value 

    static member toStr (spec : string) (value:decimal)  = 
        value.ToString(spec)
        |> String.replaceComaDot

