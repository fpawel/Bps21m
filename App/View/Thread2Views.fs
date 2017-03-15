module Bps21.View.Thread2 

open System
open System.Windows.Forms
open System.Drawing
open System.Threading

open MyWinForms.Utils
open MyWinForms.TopDialog

open MainWindow

open Bps21.View

let private updLabelTime (tb:Label) startTime = 
    tb.Text <- sprintf "%s - %s" ( DateTime.toString startTime) (TimeSpan.toString <| DateTime.Now - startTime  ) 

let panelPerformingInfo = 
    new Panel (Parent = TopBar.thread2, Dock = DockStyle.Fill)

let labelScenaryName = 
    new Label(  Parent = panelPerformingInfo, 
                        TextAlign = ContentAlignment.MiddleLeft, 
                        Dock = DockStyle.Fill, 
                        Font = new Font("Consolas", 12.f) )

let panelDelay = new Panel (Parent = TopBar.thread2, Dock = DockStyle.Fill, Visible = false)

let panelDelay1 = 
    let x = new Panel (Parent = panelDelay, Dock = DockStyle.Fill)
    let _ = new Panel(Parent = panelDelay, Width = 3, Dock = DockStyle.Left )
    let _ = new Panel(Parent = panelDelay, Width = 3, Dock = DockStyle.Right )
    let _ = new Panel(Parent = panelDelay, Height = 3, Dock = DockStyle.Top )
    let _ = new Panel(Parent = panelDelay, Height = 3, Dock = DockStyle.Bottom )
    x

let panelClosing = 
    TopmostBar( form, Visible = false, Width = 400, Font = new Font("Consolas", 12.f),
                TextForeColor = Some Color.Brown, Placement = RightBottom,
                ButtonAccept = None, ButtonCancel = None,
                Text = "Выполнение прервано! Выполняется подготовка данных...") 

let btnStop = 
    new Button(Parent = TopBar.thread2, Height = 40, Width = 40, Visible = false,
               ImageList = Widgets.Icons.instance.imageList1,
               FlatStyle = FlatStyle.Flat,
               Dock = DockStyle.Left, ImageKey = "close")

let btnSkipDelay = 
    let _ = new Panel(Parent = panelDelay, Width = 3, Dock = DockStyle.Left )
    let x = new Button(Parent = panelDelay, Height = 40, Width = 40,
                            ImageList = Widgets.Icons.instance.imageList1, 
                            FlatStyle = FlatStyle.Flat, Dock = DockStyle.Left,
                            ImageKey = "skip")
    let _ = new Panel(Parent = panelDelay, Width = 3, Dock = DockStyle.Left )
    x

let labelDelay =     
    new Label(  Parent = panelDelay1, 
                        TextAlign = ContentAlignment.MiddleLeft, 
                        Dock = DockStyle.Fill, 
                        Font = new Font("Consolas", 12.f) ) 

let progressBarDelay = 
    new ProgressBar(Parent = panelDelay1, Dock = DockStyle.Bottom,  Height = 15, Value = 0,  
                    Minimum=0, Maximum = Int32.MaxValue )

let panelModalMessage = 
    TopmostBar(form, Visible = false, Width = 400, Font = new Font("Consolas", 12.f),
            Placement = Center, ButtonAccept = Some "Продолжить", ButtonCancel = None )

module Delay = 
    
    
    [<AutoOpen>]
    module private P = 
        let mutable text = ""
        let upd start'time get'time = panelDelay.PerformThreadSafeAction <| fun () ->
            
            let elepsed = DateTime.Now - start'time

            labelDelay.Text <- 
                sprintf "%s %s из %s" text (TimeSpan.toString elepsed) (TimeSpan.toString (get'time()))
            let value, maximum = int elepsed.TotalMilliseconds, int (get'time()).TotalMilliseconds
            if value < maximum then
                if value > progressBarDelay.Maximum then
                    progressBarDelay.Maximum <- Int32.MaxValue
                progressBarDelay.Value <- int elepsed.TotalMilliseconds
                progressBarDelay.Maximum <- int (get'time()).TotalMilliseconds

            

    let initialize =         
        
        btnSkipDelay.Click.Add <| fun _ ->
            panelDelay.Visible <- false
            Bps21.PartyWorks.Delay.cancel()

        Bps21.PartyWorks.Delay.onStart.Value <- fun what get'time -> 
            MainWindow.form.PerformThreadSafeAction <| fun () ->
                text <- what
                panelPerformingInfo.Visible <- false
                progressBarDelay.Value <- 0        
                panelDelay.Visible <- true
                upd DateTime.Now get'time
            

        Bps21.PartyWorks.Delay.onStop.Value <- fun () -> 
            MainWindow.form.PerformThreadSafeAction <| fun () ->
                panelDelay.Visible <- false
                panelPerformingInfo.Visible <- true
            

        Bps21.PartyWorks.Delay.onUpdate.Value <- fun start'time get'time -> 
            upd start'time get'time
            
        fun () -> ()

let initialize = 
    

    Delay.initialize()

    Thread2.addScenaryKeepRunningChangedHandler <| function
        | _, false ->                 
            form.PerformThreadSafeAction <| fun () ->
                panelModalMessage.Visible <- false
        | _ -> ()

    Bps21.PartyWorks.ModalMessage.onShow.Value <- fun title level text ->            
        let t = panelModalMessage
        form.PerformThreadSafeAction <| fun () -> 
            t.Visible <- false
            t.Title <- title
            t.TextForeColor <- Some <| Logging.foreColor level
            t.Text <- text 
            t.Visible <- true

    Bps21.PartyWorks.ModalMessage.onClose.Value <- fun () ->            
        form.PerformThreadSafeAction 
            ( fun () ->  panelModalMessage.Visible <- false )

    Bps21.PartyWorks.ModalMessage.getIsVivisble.Value <- fun () ->
        panelModalMessage.Visible

    Thread2.showPerformingMessage.Value <- fun level text -> 
        if TabPages.getSelected() <> TabsheetScenary then
            form.PerformThreadSafeAction <| fun () ->
                let x = MainWindow.labelPerformingInfo
                x.Text <- text                
                x.ForeColor <- Logging.foreColor level
                MainWindow.setTooltip x Thread2.scenary.Value.FullName
               
               
    let panelSenaryResult = 
        TopmostBar(form, Visible = false, Width = 400, Font = new Font("Consolas", 12.f),
            Placement = Center, ButtonAccept = Some "Закрыть", ButtonCancel = None )         

    Thread2.showScenaryReport.Value <- fun title level text ->        
        form.PerformThreadSafeAction <| fun () -> panelSenaryResult.DoUpdate <| fun () ->
            panelSenaryResult.Title <- title
            panelSenaryResult.Text <- text
            panelSenaryResult.TextForeColor <- Some <| Logging.foreColor level

    Thread2.closeScenaryReport.Value <- fun () ->        
        form.PerformThreadSafeAction <| fun () -> 
            panelSenaryResult.Visible <- false
    

    Thread2.IsRunningChangedEvent.addHandler <| fun (_,v) ->
        MainWindow.form.PerformThreadSafeAction <| fun () ->
            panelPerformingInfo.Visible <- v
            
            labelScenaryName.Text <- sprintf "Выполняется сценарий %A"  Thread2.scenary.Value.FullName

    Thread2.IsRunningChangedEvent.addHandler <| function
        | true,false -> form.PerformThreadSafeAction <| fun () ->
            panelClosing.Visible <- false        
        | _ -> ()
        
    btnStop.Click.Add <| fun _ ->
        Thread2.forceStop()
        Logging.warn "выполнение сценария %A было прервано пользователем" Thread2.scenary.Value.FullName
        panelClosing.DoUpdate <| fun () ->
            panelClosing.Title <- Thread2.scenary.Value.Name
        
    Thread2.addScenaryKeepRunningChangedHandler <| fun (_,keep'running) ->
        form.PerformThreadSafeAction <| fun () ->
            btnStop.Visible <- keep'running 
            if not keep'running then
                Bps21.PartyWorks.Delay.cancel()
                panelDelay.Visible <- false

    fun () -> ()