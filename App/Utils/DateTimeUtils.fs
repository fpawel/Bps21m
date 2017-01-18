[<AutoOpen>]
module DateTimeUtils

open System

let monthByNumber n = (Globalization.CultureInfo.CurrentCulture.DateTimeFormat.GetMonthName n).ToLower()
  
type TimeSpan with
    static member toString (x:TimeSpan) = 
        x.ToString( @"hh\:mm\:ss" )


type DateTime with

    static member isValidDate (year,month,day) = 
        year >= DateTime.MinValue.Year && year <= DateTime.MaxValue.Year &&
        month > 0 && month < 13 &&
        day > 0 && day <= DateTime.DaysInMonth(year, month)

    static member isValidTime (h,m,s) = 
        h>(-1) && h < 24 &&
        m>(-1) && m < 60 &&
        s>(-1) && s < 60

    
    static member isValidDateTime (year,month,day,h,m,s) = 
        DateTime.isValidDate (year,month,day) && DateTime.isValidTime (h,m,s)


    static member format (spec:string) (x:DateTime) = 
        x.ToString(spec)

    static member toString x = 
        DateTime.format "HH:mm:ss" x

    static member toString1 (x:DateTime) = 
        DateTime.format "HH:mm" x

    static member toString2 (x:DateTime) = 
        DateTime.format "dd.MM.yy HH:mm" x



