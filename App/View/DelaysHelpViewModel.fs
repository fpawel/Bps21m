namespace Bps21.ViewModel
open System
open System.ComponentModel
open Bps21
open Operations
open PartyWorks

open MyWinForms.Converters

type DelaysHelperViewModel() =
    inherit DelaysHelperViewModel1()
    override x.RaisePropertyChanged propertyName = 
        ViewModelBase.raisePropertyChanged x propertyName

    [<DisplayName("Продувка ПГС1")>]    
    [<Description("Продувка ПГС1, длительность час:мин:сек")>]
    [<TypeConverter(typeof<TimeSpanConverter>)>]
    member x.ScaleBeg 
        with get() = x.GetDelay (BlowDelay ScaleBeg)
        and set value = x.SetDelay (BlowDelay ScaleBeg) value  

    [<DisplayName("Продувка ПГС3")>]    
    [<Description("Продувка ПГС3, длительность час:мин:сек")>]
    [<TypeConverter(typeof<TimeSpanConverter>)>]
    member x.ScaleMid 
        with get() = x.GetDelay (BlowDelay ScaleMid)
        and set value = x.SetDelay (BlowDelay ScaleMid) value  

    [<DisplayName("Продувка ПГС4")>]    
    [<Description("Продувка ПГС4, длительность час:мин:сек")>]
    [<TypeConverter(typeof<TimeSpanConverter>)>]
    member x.ScaleEnd 
        with get() = x.GetDelay (BlowDelay ScaleEnd)
        and set value = x.SetDelay (BlowDelay ScaleEnd) value  

    [<DisplayName("Прогрев НКУ")>]    
    [<Description("Прогрев НКУ, длительность час:мин:сек")>]
    [<TypeConverter(typeof<TimeSpanConverter>)>]
    member x.TermoNorm 
        with get() = x.GetDelay (WarmDelay TermoNorm)
        and set value = x.SetDelay (WarmDelay TermoNorm) value  

    [<DisplayName("Прогрев T-")>]    
    [<Description("Прогрев T-, длительность час:мин:сек")>]
    [<TypeConverter(typeof<TimeSpanConverter>)>]
    member x.TermoLow 
        with get() = x.GetDelay (WarmDelay TermoLow)
        and set value = x.SetDelay (WarmDelay TermoLow) value  

    [<DisplayName("Прогрев T+")>]    
    [<Description("Прогрев T+, длительность час:мин:сек")>]
    [<TypeConverter(typeof<TimeSpanConverter>)>]
    member x.TermoHigh 
        with get() = x.GetDelay (WarmDelay TermoHigh)
        and set value = x.SetDelay (WarmDelay TermoHigh) value  

    [<DisplayName("Прогрев +90")>]    
    [<Description("Прогрев +90, длительность час:мин:сек")>]
    [<TypeConverter(typeof<TimeSpanConverter>)>]
    member x.Termo90 
        with get() = x.GetDelay (WarmDelay Termo90)
        and set value = x.SetDelay (WarmDelay Termo90) value  

    [<DisplayName("Выдержка, техпрогон")>]    
    [<Description("Выдержка, техпрогон, длительность час:мин:сек")>]
    [<TypeConverter(typeof<TimeSpanConverter>)>]
    member x.TexprogonDelay 
        with get() = x.GetDelay (TexprogonDelay)
        and set value = x.SetDelay (TexprogonDelay) value  

    [<DisplayName("Продувка ПГС1, калибровка")>]    
    [<Description("Продувка ПГС1, калибровка, длительность час:мин:сек")>]
    [<TypeConverter(typeof<TimeSpanConverter>)>]
    member x.AdjustDelay_0 
        with get() = x.GetDelay (AdjustDelay false)
        and set value = x.SetDelay (AdjustDelay false) value  

    [<DisplayName("Продувка ПГС4, калибровка")>]    
    [<Description("Продувка ПГС4, калибровка, длительность час:мин:сек")>]
    [<TypeConverter(typeof<TimeSpanConverter>)>]
    member x.AdjustDelay_1 
        with get() = x.GetDelay (AdjustDelay true)
        and set value = x.SetDelay (AdjustDelay true) value  
