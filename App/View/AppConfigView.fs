namespace Bps21.View

open System
open System.ComponentModel

open AppConfig
open Bps21

[<AutoOpen>]
module private Helpers =
    let party = AppContent.party

type AppConfigView() = 
    
    [<DisplayName("Исполнение")>]    
    [<Description("Исполнение приборов партии")>]
    [<TypeConverter (typeof<PartyProductsDialogs.ProductTypesConverter>) >]
    member x.ProductType 
        with get() = party.ProductType
        and set v = 
            party.ProductType <- v
            Thread2.scenary.Set PartyWorks.main
            Scenary.updateGridViewBinding()
            Products.Columns.setVisibilityFromConfig()
            
    [<DisplayName("Наименование")>]    
    [<Description("Наименование партии")>]
    member x.Name 
        with get() = party.Name
        and set v = 
            party.Name <- v

    [<DisplayName("СОМ порт")>]
    [<Description("Настройка параметров приёмопередачи СОМ порта, к которому подключены настраиваемые приборы по RS 485")>]
    member x.Comport
        with get() = config.Comport
        and set v = 
            config.Comport <- v

    override __.ToString() = ""


type InterrogateConverter() =
    inherit MyWinForms.Converters.BooleanTypeConverter("Опрашивать", "Не опрашивать")

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type SelectPhysVars() = 
    let cfg = AppConfig.config.View 

    [<DisplayName("Конц.")>]
    [<Description("Концентрация")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    member x.Conc 
        with get() =
            Set.contains DevConc cfg.DevVars 
        and set value =
            cfg.DevVars <- 
                (if value then Set.add else Set.remove) DevConc cfg.DevVars            
            

    [<DisplayName("I")>]
    [<Description("Ток")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    member x.Curr 
        with get() =
            Set.contains DevCurr cfg.DevVars 
        and set value =
            cfg.DevVars <- 
                (if value then Set.add else Set.remove) DevCurr cfg.DevVars            
            

    [<DisplayName("U")>]
    [<Description("Напряжение")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    member x.Var1 
        with get() =
            Set.contains DevTens cfg.DevVars 
        and set value =
            cfg.DevVars <- 
                (if value then Set.add else Set.remove) DevTens cfg.DevVars

    override x.ToString() = cfg.DevVars |> Seq.toStr ", " DevVar.What