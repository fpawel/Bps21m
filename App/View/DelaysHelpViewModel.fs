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

    [<DisplayName("Включение питания")>]    
    [<Description("Задержка после включения питания, час:мин:сек")>]
    [<TypeConverter(typeof<TimeSpanConverter>)>]
    member x.ScaleBeg 
        with get() = x.GetDelay DelayPowerOn 
        and set value = x.SetDelay DelayPowerOn value  

    [<DisplayName("Установка входного тока")>]    
    [<Description("Задержка после установка входного тока, час:мин:сек")>]
    [<TypeConverter(typeof<TimeSpanConverter>)>]
    member x.ScaleMid 
        with get() = x.GetDelay DelaySetCurrent
        and set value = x.SetDelay DelaySetCurrent value  

    [<DisplayName("Корректировка показаний")>]    
    [<Description("Задержка после корректировки показаний, час:мин:сек")>]
    [<TypeConverter(typeof<TimeSpanConverter>)>]
    member x.ScaleEnd 
        with get() = x.GetDelay DelayAdjust
        and set value = x.SetDelay DelayAdjust value  

    
