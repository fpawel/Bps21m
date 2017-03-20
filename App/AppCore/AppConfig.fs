module AppConfig

open System
open System.IO

open System
open System.Collections.ObjectModel
open System.ComponentModel
open System.ComponentModel.DataAnnotations
open System.Drawing.Design

open MyWinForms.Converters



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
     
type StednPlace =
    {   DACCurrent : Map<decimal,byte*byte>
    } 

type Stend = 
    {   mutable Places : Map<byte,StednPlace>
        mutable DACCurrent : Map<decimal,byte*byte>
    }
    member y.GetDACCurrent n current = 
        (   match y.Places.TryFind n with
            | Some x -> x.DACCurrent
            | _ -> y.DACCurrent )
        |> Map.tryFind current   


[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type ApplicatioConfig = 
    {   View : View.Config
        mutable Comport : ComportConfig.Config 
        mutable Stend : Stend  }
    
    static member create() = 
        {   View = View.Config.create()
            Comport = ComportConfig.Config.dummy() 
            Stend = 
                {   Places = Map.empty
                    DACCurrent  = 
                        [   4m,  (0x02uy, 0xFCuy)
                            12m, (0x08uy, 0x2Cuy)
                            20m, (0x0Duy, 0x63uy)  ] 
                        |> Map.ofList
                }
        }

let config, save = Json.Config.create "app.config.json" ApplicatioConfig.create