namespace Bps21.ViewModel

open System
open System.ComponentModel

[<AllowNullLiteral>]
[<AbstractClass>]
type ViewModelBase() =
    
    let propertyChangedEvent = DelegateEvent<PropertyChangedEventHandler>()
    
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChangedEvent.Publish

    member private x.RaisePropertyChangedBase propertyName = 
        MainWindow.form.PerformThreadSafeAction <| fun () ->
            propertyChangedEvent.Trigger([| x; PropertyChangedEventArgs(propertyName) |])

    static member raisePropertyChanged<'a when 'a :> ViewModelBase>  (x:'a ) propertyName =         
        let t = typeof<'a>
        if t.GetProperty propertyName = null then
            Logging.debug "Property %A does not exist in type %A" propertyName t
        else
            x.RaisePropertyChangedBase propertyName

    abstract RaisePropertyChanged : string -> unit
