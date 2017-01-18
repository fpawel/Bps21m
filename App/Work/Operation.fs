module Bps21.ViewModel.Operations

open System

open Bps21

let private party = Bps21.AppContent.party
type private P = Bps21.ViewModel.Product



type OperationInfo = 
    {   Name : string 
        GetParent : unit -> Operation option 
        GetRunInfo : unit -> RunOperationInfo }
    static member create (name, getParent, getRunInfo) = 
        
        {  Name = name; GetParent = getParent; GetRunInfo = getRunInfo }




and Delay =
    {   mutable Time : TimeSpan 
        DelayType : DelayType }
    static member create time delayType =
        {   Time  = time
            DelayType = delayType }

and Operation =
    | Single of OperationInfo * (unit -> string option) 
    | Timed of OperationInfo * Delay  * ( (unit -> TimeSpan) -> string option) 
    | Scenary of OperationInfo * (Operation list)

    member x.Name = Operation.GetName x

    member x.FullName = Operation.GetFullName x

    member x.RunInfo = Operation.GetRunInfo x

    member x.Info = Operation.info x

    member x.Name2 (root) = Operation.GetName2 root x

    static member GetName x = (Operation.info x).Name

    static member GetRunInfo x = (Operation.info x).GetRunInfo()

    static member info : Operation -> OperationInfo = function
        | Single (x,_) -> x
        | Timed (x,_,_) -> x 
        | Scenary (x,_) -> x 

    static member private DoPerform (x:Operation) (do'beg) f = 
        
        
        x.RunInfo.SetStart()
        let do'end = do'beg x
        let opName = Operation.GetFullName x
        Logging.info "Начало %A" opName
        let r = 
            if party.HasNotOneCheckedProduct() then Some "не отмечено ни одного прибора" else
            try                 
                f() 
            with e -> 
                Logging.error "Исключительная ситуация при выполнении %A - %A" x e
                Some e.Message
        Logging.write (match r with None -> Logging.Info | _ -> Logging.Error) "Окончание %A" opName
        do'end()
        x.RunInfo.SetEnd()
        r

    static member Perform do'beg isKeepRunning = function
        | _ when not (isKeepRunning()) -> None
        | Single (_,f) as x-> Operation.DoPerform x do'beg f
        | Timed (_, time, f) as x -> 
            Operation.DoPerform x do'beg ( fun () -> f ( fun () -> time.Time ) )
        | Scenary (_,items) as x -> 
            let rec loop = function
                | _ when not (isKeepRunning()) -> None
                | [] -> None
                | operation::rest -> 
                    match Operation.Perform do'beg isKeepRunning operation with 
                    | Some _ as failed -> failed
                    | _ -> loop rest
            Operation.DoPerform x do'beg ( fun () -> loop items)

    static member getDelay = function
        | Timed (_,t,_) -> Some t.Time
        | _ -> None

    static member tree = function
        | Single _ as x -> [x]
        | Timed _ as x -> [x]
        | Scenary (_,items) as x -> x::(items |> List.map Operation.tree  |> List.concat)

    static member GetRoot x =
        let rec loop x = 
            match ( Operation.info x).GetParent() with 
            | Some x -> loop x                
            | _ -> x
        loop x

    static member GetParents x =
        let rec loop acc x = 
            match ( Operation.info x).GetParent() with 
            | Some x -> loop (x::acc) x                
            | _ -> acc
        loop [] x |> List.rev

    static member GetRelativeLevel root x =
        Operation.GetParents x 
        |> List.rev
        |> Seq.skipWhile( fun z -> Operation.GetFullName z <> Operation.GetFullName root )
        |> Seq.length 

    static member GetName2 root x =
        sprintf "%s%s" (String(' ', (Operation.GetRelativeLevel root x) * 2)) (Operation.GetName x)

    static member GetFullName x =
        let parents = Operation.GetParents x
        match parents with
        | [] | [_] -> x.Name             
        | xs -> x::xs |> List.rev |> List.tail |> List.fold( fun acc x  -> 
            let what = x.Name
            if what="" then acc else            
            let what = if what.EndsWith "." then what.Substring(0,what.Length-1) else what 
            let s = acc + ( if acc="" then "" else ". ") + ( sprintf "%s" what)
            acc + ( if acc="" then "" else ". ") + ( sprintf "%s" what) ) ""    

    static member GetConfig x =
        let i = Operation.GetRunInfo x
        {   Time = 
                match x with
                | Timed (_,x,_) -> Some x.Time
                | _ -> None
            Items = 
                match x with
                | Scenary (_,items) -> Some ( List.map Operation.GetConfig items )
                | _ -> None 
            Name = x.Name  }

    static member SetConfig (x,cfg:Config) =
        match x, cfg with
        | Timed (_,x,_) , {Time = Some time} -> 
            x.Time <- time
        | _ -> ()
        match x, cfg.Items with
        | Scenary (_,xitems), Some items  -> 
            List.zip2 xitems items |> List.iter Operation.SetConfig
        | _ -> ()  

    static member CreateScenary (name,getParent) items =
        let runInfo' : RunOperationInfo option ref = ref None
        let getRunInfo() = (!runInfo').Value
        let this' : Operation option ref = ref None
        
        let thisParent() = !this'
        let items = items |> List.map( function
            | Scenary (i,items) -> Operation.CreateScenary ( i.Name, thisParent) items
            | Single ( i, f ) -> Operation.CreateSingle ( i.Name, thisParent) f
            | Timed ( i, t, f ) -> Operation.CreateTimed (i.Name, thisParent) t f )

        let x = Scenary ( ( OperationInfo.create( name, getParent, getRunInfo)), items )

        runInfo' := Some ( RunOperationInfo(x) )
        this' := Some x
        x

    static member Create (name,getParent) constructor' =        
        let x' : RunOperationInfo option ref = ref None
        let getRunInfo() = (!x').Value
        let x = constructor'( OperationInfo.create (name, getParent, getRunInfo) )
        x' := Some ( RunOperationInfo(x) )
        x

    static member CreateSingle x f =        
        Operation.Create x <| fun i -> 
            Single ( i, f )
        

    static member CreateTimed x delay f =
        Operation.Create x <| fun i -> 
            Timed ( i, delay, f )

    static member WasErrorWhenRunning (x:Operation) = 
        party.TryGetLogOperation x.FullName
        |> Option.map( function 
            | {RunStart = Some starttime} -> 
                Operation.tree x |> List.exists( fun o -> 
                    o.RunInfo.LoggingRecords |> List.exists( function 
                        | dt, Logging.Error,_ when dt > starttime ->  true
                        | _ -> false ) ) 
            | _ -> false )
        |> Option.withDefault false

    static member Choose1 f = function        
        | Scenary (i,items) as x -> 
            if f x then 
                Some <| Scenary (i, items |> List.choose (Operation.Choose1 f) )                
            else                
                None
        | x -> if f x then Some x else None

    static member TryGetWith f = function        
        | Scenary (i,items) as x -> 
            match f x with
            | Some r -> Some r
            | _ -> items |> List.tryGetWith f
        | x -> f x 

    static member MapReduce f op = [ 
        match f op with
        | Some x -> yield x
        | _ -> ()
        match op with
        | Scenary (_,items) as x -> 
            yield! items |> List.map (Operation.MapReduce f) |> Seq.concat
        | _ -> ()  ]


and Config = 
    {   Name : string
        Time : TimeSpan option
        Items : (Config list) option  }
    static member CreateNew() = 
        {   Time  = None
            Items = None
            Name = ""  }

and RunOperationInfo( operation : Operation) = 
    inherit ViewModelBase() 
    let mutable elepsedTime : TimeSpan option = None
    let mutable root = None
    
    let getRoot() = 
        match root with
        | Some root -> root
        | _ -> Operation.GetRoot operation

    let nfo() = party.GetLogOperation operation.FullName

    override x.RaisePropertyChanged propertyName = 
        ViewModelBase.raisePropertyChanged x propertyName

    member private x.updateView() = 
        [   "Status"            
            "Name"
            "DelayTime" ]
        |> List.iter x.RaisePropertyChanged

    member x.LoggingRecords : Logging.Line list = 
        let ops = Operation.tree operation
        [   for op in ops do
                yield! (party.GetLogOperation op.FullName).LogLines ]
        |> List.sortBy( fun(t,_,_) -> t)

    member x.Errors = 
        x.LoggingRecords
        |> List.choose(function (_, Logging.Error, s )-> Some s | _ -> None)
        |> Seq.toStr "\n" id

    member x.HasErrors = x.LoggingRecords |> List.exists( function(_,Logging.Error,_) -> true| _ -> false)

    member x.AddLogging l s = 
        party.WriteJournal operation.FullName l s  
        x.updateView()
    
    member x.Root 
        with get() = root
        and set v = 
            root <- v
            x.updateView()

    member x.Level = Operation.GetRelativeLevel (getRoot()) operation
            
    member __.Name = 
        Operation.GetName2 (getRoot()) operation
    member x.Operation = operation

    member x.SetDelayTime value =
        match operation with
        | Timed (_,t,_) ->            
            t.Time <- value
            x.updateView()
        | _ -> ()
    
    member x.DelayTime 
        with get() = 
            match Operation.getDelay operation with 
            | Some t ->TimeSpan.toString t
            | _ -> ""
        and set v =
            match operation with
            | Timed _ ->            
                let b, v = TimeSpan.TryParse v
                if not b then () else
                x.SetDelayTime v
            | _ -> ()

    member x.SetStart() =
        party.LogStartOperation operation.FullName
        x.updateView()

    member x.SetEnd() =
        party.LogStopOperation operation.FullName
        x.updateView()

    member __.IsPerforming = 
        let i = nfo()
        match i.RunStart, i.RunEnd with
        | Some t, None -> true
        | _ -> false

    member __.WasPerformed = 
        let i = nfo()
        match i.RunStart, i.RunEnd with
        | Some _, Some _ -> true
        | _ -> false

    member __.Status = 
        let i = nfo()
        let (|Dt|_|) = function Some x -> Some (DateTime.toString1 x) | _ -> None
        match i.RunStart, i.RunEnd with
        | Dt t, None -> sprintf "%s -->" t
        | Dt t, Dt t2 when t <> t2 -> sprintf "%s - %s" t t2
        | Dt t, Some _ -> t
        | _ -> ""

