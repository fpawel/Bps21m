module Bps21.View.TopBar 

open System
open System.Windows.Forms
open System.Drawing

open MainWindow

let placeHolder = 
    let x = new Panel(Parent = form, Dock = DockStyle.Top, Height = 42)
    form.Controls.Add <| new Panel(Dock = DockStyle.Top, Height = 3)
    x
let thread2 =      
    new Panel(Parent = placeHolder, Dock = DockStyle.Fill)
let right = new Panel(Parent = placeHolder, Dock = DockStyle.Right, AutoSize = true)

let thread1ButtonsBar = new Panel(Parent = placeHolder, Dock = DockStyle.Left, AutoSize = true)
    
let buttonAbout = 
    let x =
        new Button( Parent = right, Height = 40, Width = 40, Visible = true,
                    ImageList = Widgets.Icons.instance.imageList1,
                    FlatStyle = FlatStyle.Flat,
                    Dock = DockStyle.Right, ImageKey = "about")
    right.Controls.Add <| new Panel(Dock = DockStyle.Right, Width = 3)

    x.Click.Add <| fun _ ->
        MainWindow.aboutForm.Show()  
        |> ignore  

    x

let buttonProductTypes = 
    let x =
        new Button( Parent = right, Height = 40, Width = 40, Visible = true,
                    ImageList = Widgets.Icons.instance.imageList1,
                    FlatStyle = FlatStyle.Flat,
                    Dock = DockStyle.Right, ImageKey = "tools")
    right.Controls.Add <| new Panel(Dock = DockStyle.Right, Width = 3)
    x
    

let initialize = 
    setTooltip buttonProductTypes "Таблица исполнений"

    let buttonSave = new Button(Parent = thread1ButtonsBar, AutoSize = true, Dock = DockStyle.Left,
                                FlatStyle = FlatStyle.Flat,
                                Text = "Cохранить", Visible = false)
    buttonSave.SetInfoStyle()
    setTooltip buttonSave "Cохранить изменения"
    Thread2.IsRunningChangedEvent.addHandler <| fun (_,is'running) ->
        thread1ButtonsBar.Visible <- not is'running

    Bps21.AppContent.subscribeOnChanged <| fun(_,v) -> 
        Control.performThreadSafeAction buttonSave <| fun () ->
            buttonSave.Visible <- v

    buttonSave.Click.Add <| fun _ -> 
        Bps21.AppContent.save()
        
    fun () -> ()