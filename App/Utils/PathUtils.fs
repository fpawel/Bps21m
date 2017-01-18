[<AutoOpen>]
module PathUtils

open System
open System.IO

type Path with

    static member ofExe =     
        try
            IO.Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly().Location)
        with _ -> 
            Environment.CurrentDirectory

    static member ofDateTime(root) =
        let (~%%) = sprintf "%d"

        let year year = 
            Path.Combine(Path.ofExe, root, %% year )

        let month y month = 
        
            let smonth = (DateTime.MinValue.AddMonths (month-1)).ToString("MMM", System.Globalization.CultureInfo.InvariantCulture)
            Path.Combine(year y, sprintf "%d-%s" month smonth)

        let day y m day = 
            Path.Combine(month y m, %% day )

        let path canCreate (dt:DateTime)  =         
            let month = dt.ToString("MMM", System.Globalization.CultureInfo.InvariantCulture)
            let path = day dt.Year dt.Month dt.Day 
            if canCreate && not ( IO.Directory.Exists path) then 
                IO.Directory.CreateDirectory path |> ignore
            path

        path, year, month, day

