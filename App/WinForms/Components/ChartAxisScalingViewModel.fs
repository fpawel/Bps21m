namespace MyWinForms.Components 

open System
open System.ComponentModel
open System.Windows.Forms.DataVisualization.Charting
open System.Windows.Forms


type ChartAxisScalingViewModel(chart : Chart) = 
    let propertyChangedEvent = DelegateEvent<PropertyChangedEventHandler>()
    let mTimer1 = new Timer ( Interval = 500 )
    let mTimer2 = new Timer ( Interval = 500 )
    let mutable minX = None
    let mutable maxX = None
    let mutable minY = None
    let mutable maxY = None
    let mutable enableYAutoCalculateBounds = false
    let axisX,axisY = let a = chart.ChartAreas.[0] in a.AxisX,a.AxisY
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChangedEvent.Publish

    member x.RaisePropertyChanged propertyName = 
        propertyChangedEvent.Trigger([| x; PropertyChangedEventArgs(propertyName) |])


    member x.setMinX checkOrder value = 
        if minX = value then () else
        match maxX, value, checkOrder with
        | Some maxX, Some value, true when maxX <= value -> ()
        | _ -> 
            minX <- value
            if value.IsSome && (Double.IsNaN(axisX.Maximum) || value.Value >= axisX.Maximum) then
                axisX.Maximum <- value.Value + 0.01
            axisX.Minimum <- Double.fromOption value
            x.RaisePropertyChanged "MinX"
            x.RaisePropertyChanged "MinDateTime"

    member x.setMaxX checkOrder value = 
        if maxX = value then () else
        match minX, value, checkOrder with
        | Some minX, Some value, true when minX >= value -> ()
        | _ ->             
            maxX <- value
            if value.IsSome && (Double.IsNaN(axisX.Minimum) || value.Value <= axisX.Minimum) then
                axisX.Minimum <- value.Value - 0.01
            axisX.Maximum <- Double.fromOption value
            x.RaisePropertyChanged "MaxX"
            x.RaisePropertyChanged "MaxDateTime"

    member x.setMinY checkOrder value =
        if minY = value then () else
        match maxY, value, checkOrder with
        | Some maxY, Some value, true when maxY <= value -> ()
        | _ -> 
            minY <- value
            if value.IsSome && (Double.IsNaN(axisY.Maximum) || value.Value >= axisY.Maximum) then
                axisY.Maximum <- value.Value + 0.01
            axisY.Minimum <- Double.fromOption value
            x.RaisePropertyChanged "MinY"

    member x.setMaxY checkOrder value =
        if maxY = value then () else
        match minY, value, checkOrder with
        | Some minY, Some value, true when minY >= value -> ()
        | _ -> 
            maxY <- value
            if value.IsSome && (Double.IsNaN(axisY.Minimum) || value.Value <= axisY.Minimum) then
                axisY.Minimum <- value.Value - 0.01
            axisY.Maximum <- Double.fromOption value
            x.RaisePropertyChanged("MaxY")

    

    member x.MinX 
        with get() = 
            minX
        and set value =
            x.setMinX true value

    member x.MaxX 
        with get() =  
            maxX
        and set value =
            x.setMaxX true value

    member x.MinY 
        with get() = 
            minY
        and set value =            
            x.setMinY true value

    member x.MaxY 
        with get() = 
            maxY
        and set value =            
            x.setMaxY true value

    member x.NormalizeYOrders (yPts : float seq) =        
            if Seq.isEmpty yPts then () else
            let yPts = Seq.distinct yPts  
            if Seq.length yPts = 1 then
                let v = Seq.head yPts
                x.setMaxY false <| Some(v + 0.1)
                x.setMinY false <| Some(v - 0.1)            
            else            
                let min = Seq.min yPts
                let max = Seq.max yPts
                let d = Math.Abs(max - min) * 0.1
                x.setMaxY false <| Some(max + d)
                x.setMinY false <| Some(min - d)

    member x.YPoints = seq{
        for series in chart.Series do
            for pt in series.Points do
                for value in pt.YValues do
                    yield value }

    member x.InitializeAxisTimer() =
        mTimer1.Tick.AddHandler ( fun _ _ ->            
            if minX <> axisX.Minimum.Option then            
                minX <- axisX.Minimum.Option
                x.RaisePropertyChanged "MinX"
                x.RaisePropertyChanged "MinDateTime"
                if enableYAutoCalculateBounds && not mTimer2.Enabled then
                    mTimer2.Start()
            
            if maxX <> axisX.Maximum.Option then            
                maxX <- axisX.Maximum.Option
                x.RaisePropertyChanged("MaxX");
                x.RaisePropertyChanged("MaxDateTime");
                if enableYAutoCalculateBounds && not mTimer2.Enabled then
                    mTimer2.Start()
            
            if minY <> axisY.Minimum.Option then            
                minY <- axisY.Minimum.Option
                x.RaisePropertyChanged "MinY"
            
            if maxY <> axisY.Maximum.Option then            
                maxY <- axisY.Maximum.Option
                x.RaisePropertyChanged "MaxY" )
        mTimer1.Start()
        mTimer2.Tick.AddHandler( fun _ _ ->         
            x.NormalizeYOrders(x.YPoints)
            mTimer2.Stop() )

    member x.EnableYAutoCalculateOrders  
        with get () = enableYAutoCalculateBounds
        and set value =
            if enableYAutoCalculateBounds = value then () else
            enableYAutoCalculateBounds <- value
            if not value then
                mTimer2.Stop()
            x.RaisePropertyChanged "EnableYAutoCalculateOrders"

    member x.MinDateTime
        with get() =
            x.MinX |> Option.map DateTime.FromOADate            
        and set value =
            if x.MinDateTime <> value then    
                x.MinX <- value |> Option.map (fun y -> y.ToOADate())
                

    member x.MaxDateTime
        with get() =
            x.MaxX |> Option.map DateTime.FromOADate            
        and set value =
            if x.MaxDateTime <> value then    
                x.MaxX <- value |> Option.map (fun y -> y.ToOADate())

    member x.scaleMinY delta =
        match x.MinY, x.MaxY with
        | Some minY, Some maxY ->
            let s = if delta > 0 then 1. else -1.;
            let d = (maxY - minY) * 0.05;
            x.MinY <- Math.Round(minY + d * s, 4) |> Some
        | _ -> ()
            
    member x.scaleMaxY delta =
        match x.MinY, x.MaxY with
        | Some minY, Some maxY ->
            let s = if delta > 0 then 1. else -1.
            let d = (maxY - minY) * 0.05;
            x.MaxY <- Math.Round(maxY + d * s, 4) |> Some
        | _ -> ()

    member x.boundsX = 
        match x.MinDateTime, x.MaxDateTime with
        | Some minX, Some maxX -> Some (minX,maxX)
        | _ -> None


    member x.scaleMinX delta =
        x.boundsX |> Option.iter(fun (min,max) -> 
            let s = if delta > 0 then 1L else -1L
            let d = (max - min).Ticks / 20L
            x.MinDateTime <- Some <| min.AddTicks( d * s )  )

    member x.scaleMaxX delta =
        x.boundsX |> Option.iter(fun (min,max) -> 
            let s = if delta > 0 then 1L else -1L
            let d = (max - min).Ticks / 20L
            x.MaxDateTime <-  Some <| max.AddTicks( d * s ) )