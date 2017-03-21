module AppConfig

open System
open System.IO

open System
open System.Collections.ObjectModel
open System.ComponentModel
open System.ComponentModel.DataAnnotations
open System.Drawing.Design

open MyWinForms.Converters

open Bps21

module View = 
    type Grid =  
        {   mutable ColWidths : int list
            mutable ColumnHeaderHeight : int 
        }

    type Config =  
        {   mutable PartyId : string
            mutable Grids : Map<string,Grid>   
            mutable ScnDetailTextSplitterDistance : int }
        static member create() = 
            {   PartyId = ""
                Grids = Map.empty
                ScnDetailTextSplitterDistance = 0 
            }

type TwoBytes = byte*byte

type DACCurrents = Map<Current,TwoBytes>

module Stend =
    module Def =
        let DI4 = 0x02uy, 0xFCuy
        let DI12 = 0x08uy, 0x2Cuy
        let DI20 = 0x0Duy, 0x63uy

    type Place =
        {   DACCurrents : DACCurrents
        } 
        static member NewDefault() = 
            {   DACCurrents = 
                    [   I4, Def.DI4
                        I12, Def.DI12
                        I20, Def.DI20 ]
                    |> Map.ofList   
            }

    type Sets = 
        {   mutable Places : Map<byte,Place>
            mutable DI4 : TwoBytes
            mutable DI12 : TwoBytes
            mutable DI20 : TwoBytes
        }
        member y.GetDI n current =         
            y.Places.TryFind n
            |> Option.bind (fun x -> Map.tryFind current  x.DACCurrents )
            |> Option.getWithDefault(fun () -> 
                match current with
                | I4 -> y.DI4
                | I12 -> y.DI12
                | I20 -> y.DI20
                )
        static member NewDefault() = 
            {   Places =
                    Map.ofList 
                        [ for n in [1uy..10uy] -> n, Place.NewDefault() ] 
                DI4 = 0x02uy, 0xFCuy
                DI12 = 0x08uy, 0x2Cuy
                DI20 = 0x0Duy, 0x63uy
            }

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type ApplicatioConfig = 
    {   View : View.Config
        mutable Comport : ComportConfig.Config 
        mutable Stend : Stend.Sets  }
    
    static member create() = 
        {   View = View.Config.create()
            Comport = ComportConfig.Config.dummy() 
            Stend = Stend.Sets.NewDefault()
        }

let config, error, save = Json.Config.create "app.config.json" ApplicatioConfig.create