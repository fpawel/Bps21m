#load "Utils/FsharpRuntime.fs"
#load "Utils/State.fs"
#load "Utils/StrUtils.fs"
#load "Utils/PathUtils.fs"
#load "Utils/DateTimeUtils.fs"
#load "Utils/Logging.fs"
#load "Utils/Utils.fs"
#load "Bps21/Coef.fs"
#load "Bps21/ProductType.fs"
#load "Bps21/Bps21.fs"

open System

open Bps21

let createSourcefile path (source : string []) = 
    System.IO.File.WriteAllLines (__SOURCE_DIRECTORY__ + "/" + path, source)



[|  yield """namespace Bps21.ViewModel
open System
open System.ComponentModel
open Bps21

type InterrogateConverter() =
    inherit MyWinForms.Converters.BooleanTypeConverter("Опрашивать", "Не опрашивать")

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type SelectPhysVars() = 
    let cfg = AppConfig.config.View """
    

    for x in PhysVar.values do
        yield sprintf """
    [<DisplayName("%s")>]
    [<Description("%s")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    member x.%A 
        with get() =
            Set.contains %A cfg.VisiblePhysVars 
        and set value =
            cfg.VisiblePhysVars <- 
                (if value then Set.add else Set.remove) %A cfg.VisiblePhysVars            
            """ x.What x.Dscr x x x
    yield """
    override x.ToString() = cfg.VisiblePhysVars |> Seq.toStr ", " PhysVar.what"""
|]
|> createSourcefile "ViewModels/SelectPhysVarsViewModel.fs" 