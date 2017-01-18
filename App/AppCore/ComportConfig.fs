module ComportConfig

open System
open System.IO

open System
open System.ComponentModel
open System.ComponentModel.DataAnnotations
open System.IO.Ports

open MyWinForms.Converters


type ComPortNamesConverter() = 
    inherit  StringConverter()
    override this.GetStandardValuesSupported _ = true
    override this.GetStandardValuesExclusive _ = true
    override this.GetStandardValues _ =         
        SerialPort.GetPortNames() 
        |> TypeConverter.StandardValuesCollection

let comPortBoudRates = 
    [|  1200
        2400
        9600
        19200
        38400
        57600
        115200 |] 

type ComPortBoudRatesConverter() = 
    inherit Int32Converter()
    override this.GetStandardValuesSupported _ = true
    override this.GetStandardValuesExclusive _ = true
    override this.GetStandardValues _ =         
        TypeConverter.StandardValuesCollection( comPortBoudRates )

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type Config =
    {   [<DisplayName("Порт")>]
        [<Description("Выбор имени используемого СОМ порта")>]
        [<TypeConverter (typeof<ComPortNamesConverter>) >]
        mutable PortName : string

        [<DisplayName("Таймаут, мс")>]
        [<Description("Длительность ожидания ответа от прибора в милисекундах")>]   
        mutable Timeout : int

        [<DisplayName("Задержка отправки, мс")>]
        [<Description("Задержка отправки запроса прибору в милисекундах")>]
        mutable Delay : int

        [<DisplayName("Время ожидания символа, мс")>]
        [<Description("Длительность ожидания символа ответа в милисекундах")>]
        mutable Chartime : int

        [<DisplayName("Колличество повторов запроса")>]
        [<Description("Колличество повторов запроса прибору")>]
        mutable RepeatCount : int

        [<DisplayName("Показывать посылки")>]
        [<Description("Разрешить показывать посылки приёмопередачи СОМ порта в консоли данного приложения")>] 
        [<TypeConverter(typeof<YesNoConverter>)>]
        mutable CanLog : bool 

        [<Description("Скорость передачи")>]        
        [<DisplayName("Скорость передачи СОМ порта, заданная в килобитах в секунду (бодах)")>]
        [<TypeConverter (typeof<ComPortBoudRatesConverter>) >]
        mutable BaudRate : int 

        [<Browsable(false)>]
        Description : string }  
    override x.ToString() = 
        if String.IsNullOrEmpty x.PortName then "не установлен" else
        sprintf "%s, %d Б/с, %d" x.PortName x.BaudRate x.Timeout

    static member dummy() = {   
        PortName = ""
        Timeout = 1000
        Delay = 0
        Chartime = 20
        RepeatCount = 0
        CanLog = false 
        BaudRate = 9600
        Description = "-" }

    static member withDescr s = { Config.dummy() with Description = s}

