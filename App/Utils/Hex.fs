module Hex 

open System

let digToHex n =
    if n < 10 then char (n + 0x30) else char (n + 0x37)
    
let hexToDig c =
    if c >= '0' && c <= '9' then Some(int c - int '0')
    elif c >= 'A' && c <= 'F' then Some( (int c - int 'A') + 10 )
    elif c >= 'a' && c <= 'f' then Some( (int c - int 'a') + 10 )
    else None
     
let encode (buf:byte array) (prefix:bool) =
    let hex = Array.zeroCreate (buf.Length * 2)
    let mutable n = 0
    for i = 0 to buf.Length - 1 do
        hex.[n] <- digToHex ((int buf.[i] &&& 0xF0) >>> 4)
        n <- n + 1
        hex.[n] <- digToHex (int buf.[i] &&& 0xF)
        n <- n + 1
    if prefix then String.Concat("0x", new String(hex)) 
    else new String(hex)
    
let tryParse (s:string) =
    if String.IsNullOrEmpty s then None else        
    let rec hexx acc (s:string) = 
        let len = s.Length
        if len=0 then acc else
        match hexToDig s.[0], acc with                 
        | Some(v), Some(acc) ->  hexx ( Some( (acc <<< 4) + v ) ) (s.Substring(1, len-1))
        | _ -> None
    let s = let len = s.Length
            if len >= 2 && s.[0]='0' && (s.[1]='x' || s.[1] = 'X') then  (s.Substring(2, len-2)) else s
    match hexx (Some(0)) s with
    | Some(v) -> Some( int16 v )
    | _ -> None