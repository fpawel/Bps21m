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
type Termochamber = 
    {   [<DisplayName("СОМ порт")>]
        [<Description("Параметры приёмопередачи СОМ порта, к которому подключена термокамера")>]
        mutable Comport : ComportConfig.Config

        [<DisplayName("Использовать термокамеру")>]
        [<Description("Использовать термокамеру при снятии для расчёта термокомпенсации")>]
        [<TypeConverter(typeof<MyWinForms.Converters.YesNoConverter>)>]
        mutable Enabled : bool

        [<DisplayName("Погрешность уставки")>]
        [<Description("""Минимальная разница между показаниями и уставкой термокамеры, при которой температура считается установившейся \"С""")>]
        mutable SetpointErrorLimit : decimal

        [<DisplayName("Таймаут уставки")>]
        [<Description("""Максимальная длительность уставки термокамеры, по истечении которой выполнение настройки будет прекращено с сообщением об ошибке""")>]
        mutable SetpointTimeOut : TimeSpan }
    override __.ToString() = ""

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type Pneumoblock = 
    {   [<DisplayName("Адрес MODBUS")>]
        [<Description("MODBUS адрес пневмоблока")>]
        mutable Addr : byte
    
        [<DisplayName("СОМ порт")>]
        [<Description("Параметры приёмопередачи СОМ порта, к которому подключен пневмоблок")>]
        mutable Comport : ComportConfig.Config

        [<DisplayName("Использовать пневмоблок")>]
        [<Description("Использовать пневмоблок при автоматической настройке")>]
        [<TypeConverter(typeof<MyWinForms.Converters.YesNoConverter>)>]
        mutable Enabled : bool  }
    override __.ToString() = ""


[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type WarmDevice = 
    {   [<DisplayName("СОМ порт")>]
        [<Description("Параметры приёмопередачи СОМ порта, к которому подключено устройство подогрева плат")>]
        Comport : ComportConfig.Config

        [<DisplayName("Адрес MODBUS")>]
        [<Description("MODBUS адрес устройства подогрева плат")>]
        mutable Addr : byte

        [<DisplayName("Использовать")>]
        [<Description("Использовать подогрев плат")>]
        [<TypeConverter(typeof<YesNoConverter>)>]
        mutable Enabled : bool

        [<DisplayName("Температура включения, \"С")>]
        [<Description("Начало температурного диапазона работы устройства подогрева плат, \"С")>]
        mutable TempOn : decimal

        [<DisplayName("Температура выключения, \"С")>]
        [<Description("Конец температурного диапазона работы устройства подогрева плат, \"С")>]
        mutable TempOff : decimal
        
        
        }
    override __.ToString() = ""

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type Hardware =
    {   
        [<DisplayName("СОМ порт приборов")>]
        [<Description("Настройка параметров приёмопередачи СОМ порта, к которому подключены настраиваемые приборы по RS 485")>]
        mutable ComportProducts : ComportConfig.Config        
        
        [<DisplayName("Термокамера")>]
        [<Description("Настройка параметров термокамеры")>]
        mutable Termochamber : Termochamber

        [<DisplayName("Пневмоблок")>]
        [<Description("Настройка параметров пневмоблока")>]
        mutable Pneumoblock : Pneumoblock 
        
        [<DisplayName("Подогрев плат")>]
        [<Description("Настройка параметров устройства подогрева плат")>]
        mutable WarmDevice : WarmDevice
        }
    static member create() = {   
        
        Pneumoblock = 
            {   Comport = ComportConfig.Config.withDescr "пневмоблок"
                Enabled = true 
                Addr = 100uy }

        Termochamber = 
            {   Comport = ComportConfig.Config.withDescr "термокамера"
                Enabled = true
                SetpointTimeOut = TimeSpan.FromHours 4.
                SetpointErrorLimit = 2m  }
                
        ComportProducts = ComportConfig.Config.withDescr "приборы" 
        
        WarmDevice = 
            {   Comport = ComportConfig.Config.withDescr "подогрев_плат"
                Addr = 0x63uy
                Enabled = false
                TempOn = -40m
                TempOff = -30m }
        
        }
    override __.ToString() = ""

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type ApplicatioConfig = 
    {   mutable Hardware : Hardware
        
        View : View.Config

        mutable UseMidleScale : bool }
    
    static member create() = {   
        View = View.Config.create()
        Hardware = Hardware.create()
        UseMidleScale = false }

let config, save = Json.Config.create "app.config.json" ApplicatioConfig.create