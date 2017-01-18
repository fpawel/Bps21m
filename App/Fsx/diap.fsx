open System 
open System.Text.RegularExpressions

let toRanges (xs : int Set) = 
    xs  |> Seq.sort  |> Seq.fold( fun acc n ->  
        match acc with
        | [] -> [n,n]
        | (x, y) :: acc when n <= y + 1 -> (x, max (y+1) n) :: acc
        | _  -> (n, n) :: acc ) []

let fromRanges xs =
    seq{
        for x,y in xs do
            for n = x to y do
                yield n }
    |> Set.ofSeq   
    
   
let (==>) xs x = 
    seq{
        yield! xs
        yield x }
    |> Set.ofSeq 
    |> toRanges


let parse (s:string) =
    [   for m in Regex.Matches(s, @"(\d+)(\s*-\s*(\d+))?") do
            let x = Int32.Parse m.Groups.[1].Value
            let y = m.Groups.[3].Value

            if String.IsNullOrEmpty y then
                yield x,x
            else
                yield x, Int32.Parse y ]
    |> fromRanges
    |> toRanges
    |> List.rev



    
