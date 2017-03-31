module Hex 

open System

let digToHex n =
    if n < 10 then char (n + 0x30) else char (n + 0x37)
    
let hexToDig c =
    if c >= '0' && c <= '9' then Some(byte c - byte '0')
    elif c >= 'A' && c <= 'F' then Some( (byte c - byte 'A') + 10uy )
    elif c >= 'a' && c <= 'f' then Some( (byte c - byte 'a') + 10uy )
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



let tryParse s =
    if String.IsNullOrEmpty s then None else 
    let s = let len = s.Length
            if len >= 2 && s.[0]='0' && (s.[1]='x' || s.[1] = 'X') then  (s.Substring(2, len-2)) else s

    let xs = 
        [|  for n = 0 to s.Length - 1 do
                match  hexToDig s.[n] with
                | None -> ()
                | Some x -> 
                    yield x 
        |]
    if xs.Length <> s.Length then None else 
    Array.Reverse xs
    let mutable x = 0UL
    for n = 0 to s.Length - 1 do
        x <- x + uint64 (if n % 2 = 0 then xs.[n] else xs.[n] <<< 4)
    Some x