namespace Bps21.View

open System
open System.ComponentModel

open AppConfig
open Bps21

[<AutoOpen>]
module private Helpers =
    let party = AppContent.party

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type PgsConfigView() =

    [<DisplayName("ПГС1")>]    
    [<Description("ПГС1, начало шкалы, концентрация ")>]
    member x.PgsGas0
        with get() = party.GetPgs(ScaleBeg)
        and set v = party.SetPgs ( (ScaleBeg), v) 

    [<DisplayName("ПГС2")>]    
    [<Description("ПГС2, середина шкалы, концентрация ")>]
    member x.PgsGas1
        with get() = party.GetPgs(ScaleMid)
        and set v = party.SetPgs ( (ScaleMid), v) 

    [<DisplayName("ПГС3")>]    
    [<Description("ПГС3, конец шкалы, концентрация ")>]
    member x.PgsGas5
        with get() = party.GetPgs(ScaleEnd)
        and set v = party.SetPgs ( (ScaleEnd), v) 

    override __.ToString() = ""

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type TemperatureConfigView() =

    [<DisplayName("T-")>]    
    [<Description("Пониженная температура")>]
    member x.L 
        with get() = party.GetTermoTemperature TermoLow
        and set v = party.SetTermoTemperature (TermoLow,v) 

    [<DisplayName("НКУ")>]    
    [<Description("Нормальная температура")>]
    member x.N 
        with get() = party.GetTermoTemperature TermoNorm
        and set v = party.SetTermoTemperature (TermoNorm,v) 

    [<DisplayName("T+")>]    
    [<Description("Повышенная температура")>]
    member x.H 
        with get() = party.GetTermoTemperature TermoHigh
        and set v = party.SetTermoTemperature (TermoHigh,v) 

    override __.ToString() = ""

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type PartyConfigView() =
    [<DisplayName("Исполнение")>]    
    [<Description("Исполнение приборов партии")>]
    [<TypeConverter (typeof<PartyProductsDialogs.ProductTypesConverter>) >]
    member x.ProductType 
        with get() = party.ProductType
        and set v = 
            party.ProductType <- v

            Thread2.scenary.Set PartyWorks.bps21
            Scenary.updateGridViewBinding()            
            match TabPages.getSelected() with
            | MainWindow.TabsheetChart -> 
                TabPages.TabChart.update()           
            | _ -> ()
            Products.updatePhysVarsGridColsVisibility() 
            
    [<DisplayName("Наименование")>]    
    [<Description("Наименование партии")>]
    member x.Name 
        with get() = party.Name
        and set v = 
            party.Name <- v

    [<DisplayName("Концентрация ПГС")>]
    member val  Pgs = PgsConfigView() with get,set

    [<DisplayName("Температура")>]
    [<Description("Значения температур уставки термокамеры в температурных точках термокомпенсации приборов")>]
    member val  Temperature = TemperatureConfigView() with get,set

    [<DisplayName("Середина шкалы")>]
    [<Description("Выполнять снятие данных в середине шкалы при автоматической настройке термокомпенсации")>]
    [<TypeConverter(typeof<MyWinForms.Converters.YesNoConverter>)>]
    member x.UseMidleScale 
        with get() = config.UseMidleScale
        and set v = 
            if v <> config.UseMidleScale then
                config.UseMidleScale <- v

    override __.ToString() = ""


type AppConfigView() = 
    
    [<DisplayName("Партия")>]    
    member val  Party = PartyConfigView() with get,set

    [<DisplayName("СОМ приборы")>]
    [<Description("Имя СОМ порта, к которому подключены настраиваемые приборы")>]
    [<TypeConverter (typeof<ComportConfig.ComPortNamesConverter>) >]
    member x.ComportProducts
        with get() = config.Hardware.ComportProducts.PortName
        and set v = 
            if v <> config.Hardware.ComportProducts.PortName then
                config.Hardware.ComportProducts.PortName <- v

    [<DisplayName("СОМ пневмоблок")>]
    [<Description("Имя СОМ порта, к которому подключен пневмоблок")>]
    [<TypeConverter (typeof<ComportConfig.ComPortNamesConverter>) >]
    member x.ComportPneumo
        with get() = config.Hardware.Pneumoblock.Comport.PortName
        and set v = 
            if v <> config.Hardware.Pneumoblock.Comport.PortName then
                config.Hardware.Pneumoblock.Comport.PortName <- v
                
    [<DisplayName("СОМ термокамера")>]
    [<Description("Имя СОМ порта, к которому подключена термокамера")>]
    [<TypeConverter (typeof<ComportConfig.ComPortNamesConverter>) >]
    member x.ComportTermo
        with get() = config.Hardware.Termochamber.Comport.PortName
        and set v = 
            if v <> config.Hardware.Termochamber.Comport.PortName then
                config.Hardware.Termochamber.Comport.PortName <- v


    [<DisplayName("СОМ подогрев плат")>]
    [<Description("Имя СОМ порта, к которому подключено устройство подогрева плат")>]
    [<TypeConverter (typeof<ComportConfig.ComPortNamesConverter>) >]
    member x.ComportDeviceWarm
        with get() = config.Hardware.WarmDevice.Comport.PortName
        and set v = 
            if v <> config.Hardware.WarmDevice.Comport.PortName then
                config.Hardware.WarmDevice.Comport.PortName <- v


    [<DisplayName("Параметры апаратной части")>]
    [<Description("Параметры пневмоблока, термокамеры и настройки СОМ портов")>]
    member x.Hardware
        with get() = config.Hardware
        and set v = 
            config.Hardware <- v

    [<DisplayName("Используемые коэффициенты")>]
    [<Description("Диаипазоны порядковых номеров используемых коэффициентов")>]
    member x.VisibleCoefs 
        with get() = config.View.VisibleCoefs
        and set v = 
            if v <> config.View.VisibleCoefs then
                config.View.VisibleCoefs <- v

    
        

    override __.ToString() = ""

    



    

    