#I "../packages"
#I "../packages/FSharp.Compiler.CodeDom.1.0.0.1/lib/net40"
#r "FSharp.Compiler.CodeDom.dll"
#r @"System.Windows.Forms.dll"
#r @"System.Drawing.dll"

open System
//open System.IO
//open System.CodeDom.Compiler
open Microsoft.FSharp.Compiler
open System.Windows.Forms
open System.CodeDom

open FSharp.Compiler.CodeDom
let provider = new FSharpCodeProvider()


let f(s:string) = ()

let safe (ctrl : System.Windows.Forms.Control) =
    if ctrl.InvokeRequired then 
        ctrl.Invoke (  MethodInvoker( fun () -> f "" ) )
        |> ignore        
    else f ""

open System.ComponentModel
let source = """

module DynamicViewModel
open System
open System.ComponentModel
open System.Windows.Forms

type ViewModel(baseControl : System.Windows.Forms.Control) =
    let propertyChangedEvent = DelegateEvent<PropertyChangedEventHandler>()
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChangedEvent.Publish


    member private x.raise1(propertyName) = 
        propertyChangedEvent.Trigger([| x; PropertyChangedEventArgs(propertyName) |])

    member x.RaisePropertyChanged propertyName = 
        let t = typeof<ViewModel>
        if t.GetProperty propertyName = null then
            failwithf "Property %A does not exist in type %A" propertyName t
        if baseControl.InvokeRequired then 
            baseControl.Invoke (  MethodInvoker( fun () -> x.raise1 propertyName ) )
            |> ignore        
        else x.raise1 propertyName
""" 
let opt = 
    Compiler.CompilerParameters
        (   [|  "System.dll"
                "FSharp.Core.dll"
                "System.Windows.Forms.dll"
                "System.Drawing.dll"|], 
            "MyDynamicProductViewModel.dll")
let r = provider.CompileAssemblyFromSource(opt,[|source|])

open System.Text

for error in r.Errors do
    let errorText =
        Encoding.GetEncoding( 866 ).GetString( Encoding.Default.GetBytes( error.ErrorText ) )
    //printfn "%s" error.ErrorText
    printfn "%s" errorText

r.TempFiles
r.Errors