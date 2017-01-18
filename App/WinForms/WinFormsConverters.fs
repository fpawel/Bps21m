module MyWinForms.Converters

open System
open System.ComponentModel
open System.Drawing.Design
open System.Windows.Forms
open System.Windows.Forms.Design
open System.Globalization

type Header() = 
    inherit TypeConverter()
    override x.CanConvertTo(_, destType) =  
        destType = typeof<string>
    override x.ConvertTo(_, _, _, _)  =
        box "<...>"

type TypeConverter with
    static member standardValuesCollection x =
        new TypeConverter.StandardValuesCollection(x)

   
type BooleanTypeConverter(true':string, false':string) = 
    inherit BooleanConverter()
    
    override __.ConvertTo(_,_,value,_) =
        if value |> box :?> bool then true' else false'
        |> box
    
    override __.ConvertFrom(_,_,value) =
        value |> box :?> string = true' 
        |> box

type YesNoConverter() =
    inherit BooleanTypeConverter("Да", "Нет")

type TimePickerEditor() =
    inherit UITypeEditor()
    let picker = new DateTimePicker(Format = DateTimePickerFormat.Custom, CustomFormat = "HH:mm:ss", ShowUpDown = true )
    
       
    let mutable editorService : IWindowsFormsEditorService = null

    override x.EditValue(context, provider, value) =
        
        let mutable value = value
        if provider <> null then
            editorService <- provider.GetService(typeof<IWindowsFormsEditorService>) :?> IWindowsFormsEditorService
            if editorService <> null then           
                let t = context.PropertyDescriptor.GetValue(context.Instance) :?> TimeSpan
                picker.MinDate <- DateTime.MinValue
                picker.MaxDate <- DateTime.MaxValue
                picker.Value <- picker.MinDate + t
                editorService.DropDownControl(picker)
                let dt = picker.Value
                value <- new TimeSpan(dt.Hour, dt.Minute, dt.Second)
        value

    override x.GetEditStyle(context) =        
        if context <> null then
            try 
                let dt = context.PropertyDescriptor.GetValue(context.Instance) :?> TimeSpan
                picker.Value <- new DateTime(0,0,0,dt.Hours, dt.Minutes, dt.Seconds)
            with _ -> ()
        UITypeEditorEditStyle.DropDown

type TimeSpanConverter() =
    inherit TypeConverter()

    override __.CanConvertFrom(context, sourceType) =
        if sourceType = typeof<string> then true else
            base.CanConvertFrom(context, sourceType)
    
    override __.ConvertFrom(context, culture, value) =
        if value.GetType() = typeof<string> then
            let b,v = TimeSpan.TryParse(value :?> string, new DateTimeFormatInfo( ShortTimePattern = "HH:mm:ss" ) )
            if b then v else TimeSpan()
            |> box
        else
            base.ConvertFrom(context, culture, value)

    override __.CanConvertTo(context, destinationType) =
        if destinationType=typeof<TimeSpan> || destinationType=typeof<string> then true else
            base.CanConvertTo(context, destinationType)

     override __.ConvertTo(context, culture, value, destinationType) =
        if destinationType=typeof<string> then
            value :?> TimeSpan |> TimeSpan.toString |> box
        elif destinationType = typeof<TimeSpan> then
            value
        else
            base.ConvertTo(context,culture,value,destinationType)
    
    override __.IsValid(context, value) =
        if value.GetType() = typeof<string> then
            try 
                let b,_ = TimeSpan.TryParse(value :?> string, new DateTimeFormatInfo( ShortTimePattern = "HH:mm:ss" ) )
                b
            with _ -> false
        else 
            false

