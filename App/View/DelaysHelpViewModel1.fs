namespace Bps21.ViewModel

open System
open System.ComponentModel
open System.Collections.Generic

open Bps21
open Operations
open PartyWorks

[<AutoOpen>]
module private Helpers =
    
    type DelayContext with
        static member getWorks ctx =
            Works.all |> List.choose( function 
                | Timed (op, ({DelayContext = EqualsTo ctx true } as d), _) -> Some (op,d)
                | _ -> None ) 

[<AbstractClass>]
type DelaysHelperViewModel1() =
    inherit ViewModelBase()    

    member x.GetDelay ctx =
        DelayContext.getWorks ctx
        |> List.maybeHead
        |> Option.map( fun (_,d) -> d.Time )
        |> Option.withDefault (TimeSpan.FromMinutes 3.)
        
    member x.SetDelay ctx value = 
        if x.GetDelay ctx <> value then            
            DelayContext.getWorks ctx
            |> List.iter( fun (i,_) -> i.GetRunInfo().SetDelayTime value )
            x.RaisePropertyChanged ctx.Prop
            