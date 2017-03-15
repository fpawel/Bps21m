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
            mutable ColumnHeaderHeight : int }

    type Config =  
        {   mutable PartyId : string
            mutable Grids : Map<string,Grid>   
            mutable ScnDetailTextSplitterDistance : int   }
        static member create() = 
            {   PartyId = ""
                Grids = Map.empty
                ScnDetailTextSplitterDistance = 0  }


[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type ApplicatioConfig = 
    {   View : View.Config
        mutable CurrentDifferenceLimit : decimal
        mutable Comport : ComportConfig.Config }
    
    static member create() = {   
        View = View.Config.create()
        CurrentDifferenceLimit = 10m
        Comport = ComportConfig.Config.dummy() }

let config, save = Json.Config.create "app.config.json" ApplicatioConfig.create