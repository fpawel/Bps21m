namespace Bps21.View

open System
open System.ComponentModel

open AppConfig
open Bps21

[<AutoOpen>]
module private Helpers =
    let party = AppContent.party


type RLoadLineConverter() = 
    inherit  StringConverter()
    override this.GetStandardValuesSupported _ = true
    override this.GetStandardValuesExclusive _ = true
    override this.GetStandardValues _ =         
        TypeConverter.StandardValuesCollection [| RLoadLine68.What; RLoadLine91.What |]

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

    [<DisplayName("Нагрузочный резистр")>]    
    [<Description("Выбор резистра для проверки нагрузочной способности")>]
    [<TypeConverter (typeof<RLoadLineConverter>) >]
    member x.RLoadLine 
        with get() = party.RLoadLineUI
        and set v = 
            party.RLoadLineUI <- v
            
            
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

    [<DisplayName("4 мА, ворота")>]    
    [<Description("Погрешность подстройки 4 мА")>]
    member x.TuneI4 
        with get() = config.TuneI4
        and set v = 
            config.TuneI4 <- v

    [<DisplayName("20 мА, ворота")>]    
    [<Description("Погрешность подстройки 20 мА")>]
    member x.TuneI20 
        with get() = config.TuneI20
        and set v = 
            config.TuneI20 <- v


    [<DisplayName("Unmin")>]    
    [<Description("Минимально допустимое напряжение линии, В")>]
    member x.UloadMin
        with get() = party.UloadMin
        and set value =
            party.UloadMin <- value

    [<DisplayName("Unmax")>]    
    [<Description("Максимально допустимое напряжение линии, В")>]
    member x.UloadMax
        with get() = party.UloadMax
        and set value =
            party.UloadMax <- value

    

    override __.ToString() = ""


