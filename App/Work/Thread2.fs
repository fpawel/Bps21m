module Thread2

open System
open System.ComponentModel

open Bps21
open Bps21.ViewModel.Operations
open MainWindow

let operations = BindingList<RunOperationInfo>()
let showScenaryReport = Ref.Initializable<_>(sprintf "show'performing'report %s:%s" __LINE__ __SOURCE_FILE__ )
let show'performing'message = Ref.Initializable<_>(sprintf "show'performing'message %s:%s" __LINE__ __SOURCE_FILE__ )

[<AutoOpen>]
module private Helpers1 =
    let none _ = None
    let safe = System.Windows.Forms.Control.performThreadSafeAction
    let party = Bps21.AppContent.party
    
    let scenaryKeepRunning = Ref.Observable(false)

let isKeepRunning () = scenaryKeepRunning.Get()
let notKeepRunning() = not (scenaryKeepRunning.Get())
let forceStop() = scenaryKeepRunning.Value <- false

let add'keep'running f = 
    scenaryKeepRunning.AddChanged f



let scenary = 
    let x = Ref.Observable(Operation.CreateSingle("", none) none)
    x.AddChanged <| fun (o,n) ->
        if scenaryKeepRunning.Value  then
            failwithf "can not change performing scenary from %A to %A" o.FullName n.FullName
        operations.Clear()
        let ops = Operation.tree n
        ops |> List.iter ( fun x -> 
            let i = Operation.GetRunInfo  x 
            i.Root <- Some ops.[0]
            operations.Add i )        
    x

[<AutoOpen>]
module private Helpers2 =
    let operation = 
        let x = Ref.Observable(None)
        Logging.addLogger <| fun l s ->        
            match  x.Value with
            | Some (op:Operation) -> 
                op.RunInfo.AddLogging l s                 
                show'performing'message.Value l s
            | _ -> ()
            
        |> ignore
        x

    open System.Windows.Forms
    
    let showCantExitMessage() = 
        let s = "Нельзя выйти из приложения пока идёт настройка приборов."
        MessageBox.Show(s,"СТМ-30М", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        |> ignore

let add'operation'changed f = 
    operation.AddChanged (snd >> f)

let private is'running = 
    let x = Ref.Observable(false)
    MainWindow.form.Closing.Add <| fun e ->
        if x.Value then 
            e.Cancel <- true
            forceStop()
            showCantExitMessage()
    x

module IsRunningChangedEvent =

    let addHandler f = 
        is'running.AddChanged <| fun e -> 
            safe form <| fun () -> 
                f e 


type PerformBuilder() =
    let (>>=) p rest = 
        match p with         
        | None when notKeepRunning() -> None
        | None -> rest()
        | x -> x 
    
    member b.Bind(p, rest) = p >>= rest

    member b.Bind(p, rest) = 
        match p with 
        | Ok _ when notKeepRunning() -> None
        | Ok x -> rest x
        | Err x -> Some x         

    member b.Delay(f : unit -> string option) = f

    member x.Run(f) = f()

    member x.While(cond, f) =
        if isKeepRunning() && cond() then f() >>= fun _ -> x.While(cond, f)
        else None

    member x.For(e:seq<_>,f) = 
        let cursor = e.GetEnumerator()
        let body () =  f cursor.Current        
        try
            x.While(cursor.MoveNext, x.Delay(body) )
        with _ ->
            cursor.Dispose()
            reraise()
        
    member b.ReturnFrom (x : string option) = x

    member b.ReturnFrom (x : Result<string,_>) = 
        match x with
        | Err x -> Some x
        | Ok _ -> None

    member b.Return (error:string) = Some error
    member b.Return (x:unit) = None    

    member b.Combine(m, f ) = 
        m >>= f
        
    member b.Zero() = None

    member x.TryWith(p, handler) = 
        try 
            p()
        with  exn -> 
            handler exn 

    member m.TryFinally(f, finalizer) = 
        try 
            f()
        finally 
            finalizer()

let maybeErr = PerformBuilder ()


let sleep50() = Threading.Thread.Sleep 50

let sleep t = 
    let t = TimeSpan.FromMilliseconds( float t )
    let start = DateTime.Now
    while DateTime.Now - start < t && isKeepRunning() do
        System.Threading.Thread.Sleep 50
    None    

let doWork work = 
    if notKeepRunning() then None
    elif party.HasNotOneCheckedProduct() then Some "выполнение прервано, так как не выбрано ни одного прибора"
    else work()

let private perfomOperationEvent = new Event<_>()
[<CLIEvent>]
let PerfomOperationEvent = perfomOperationEvent.Publish

let run =
    
    let do'beg op = 
        let prev'op = operation.Get()
        operation.Set (Some op)
        show'performing'message.Value Logging.Info ""
        perfomOperationEvent.Trigger(op,true)
        fun () -> 
            operation.Set prev'op
            perfomOperationEvent.Trigger(op,false)


    fun  (x : Operation)  -> 
        
        if scenaryKeepRunning.Value || is'running.Value then
            failwith "already performing"
        scenary.Set x
        operation.Set (Some x)
        scenaryKeepRunning.Value <- true
        is'running.Set true   
        for p in party.Products do
            p.Connection <- None 
        Logging.info "Начало выполнения сценария %A" x.FullName     
        
        let dostart, dostop = MyWinForms.Utils.timer 10000 AppContent.save
        dostart()

        async{
            let result = Operation.Perform do'beg isKeepRunning x
            let scenaryWasBreakedByUser = (scenaryKeepRunning.Value = false)
            scenaryKeepRunning.Value <- false
            
            let level,message = 
                match result with
                | Some error ->
                    Logging.Error, sprintf "Выполнение завершилось с ошибкой. %s" error
                | _ ->

                    if Operation.WasErrorWhenRunning x then
                        Logging.Warn, "при выполнении произошли ошибки"
                    elif scenaryWasBreakedByUser then
                        Logging.Warn, "выполнение было прервано"
                    else
                        Logging.Info, "Выполнено"
                                    
            safe form dostop
            Comport.Ports.closeOpenedPorts (fun _ _ -> true) 
            Logging.write level "Окончание выполнения сценария %A - %s" x.FullName message
            let title = sprintf "%s %s" x.RunInfo.Status x.FullName
            showScenaryReport.Value title level message 
            operation.Set None 
            is'running.Set false
//            for p in party.Products do
//                p.Connection <- None 
            AppContent.save() }
        |> Async.Start