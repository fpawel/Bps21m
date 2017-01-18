open System
open System.IO
open System.Text.RegularExpressions

let filename = __SOURCE_DIRECTORY__ + "\\koefs.cfg"

File.Exists filename

let cc (s:string) =
    let xs =  
        [|  for m in Regex.Matches(s, @"([A-Za-z][A-Za-z]*)_?") do
                if m.Success then
                    yield m.Groups.[1].Value 
            let m = Regex.Match(s, @"_(\d+)")
            if m.Success then
                yield m.Groups.[1].Value |]
        |> Array.choose(fun s -> 
            let s1 = String(s.[0],1).ToUpper()
            let s2 = s1 + s.Substring(1).ToLower() 
            if s2 = "Coef" then None else Some s2 )
    String.Join ("",xs)



let coefs = File.ReadAllLines filename |> Array.mapi (fun n s -> 
    let m = Regex.Match(s, @"^(\w+)\s+(-?\d+)\s*([^$]*)$")
    let (~%%) (n:int) = m.Groups.[n].Value
    let coef = cc s
    let defVal = Double.Parse <| %% 2
    let descr = (%% 3).Trim()
    coef, n, defVal, descr )

File.WriteAllLines
    (   __SOURCE_DIRECTORY__ + "\\coefs.cfg", 
        coefs |> Array.map(fun (coef, n, _, descr) -> sprintf "%d %s %s" n coef descr ) )


let customCoefs1 = 
    File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\coefs.cfg")
    |> Array.map (fun s -> 
        let m = Regex.Match(s, @"^(\d+)\s+(\w+)\s*([^$]*)$")
        let (~%%) (n:int) = m.Groups.[n].Value
        let n = Int32.Parse (%% 1)
        let coef = %% 2
        let descr = (%% 3).Trim()
        let descr = if String.IsNullOrEmpty descr then None else Some descr
        n, coef, descr ) 

customCoefs1.Length
coefs.Length

