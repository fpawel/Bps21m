module Bps21.View.LeftBottom

open Bps21

open System
open System.Windows.Forms
open System.Drawing
open WinFormsControlUtils

[<AutoOpen>]
module private Helpers =
    type P = Bps21.ViewModel.Product     
    let party = AppContent.party
    let placeholder =  MainWindow.leftBottomPlaceHolder
    type Dock = DockStyle


let private panPow() = 
    let panPow = new Panel(Font = new Font("Consolas", 12.f), BorderStyle = BorderStyle.FixedSingle,
                            Width = 140, Height = 100)

    let _ = new Panel(Parent = panPow, Dock = Dock.Left, Width = 5)

    let radioButtonPowerMain = 
        new RadioButton(Parent = panPow, Dock = DockStyle.Top, Text = "Основное", Checked = true)
    let radioButtonPowerReserve =
        new RadioButton(Parent = panPow, Dock = DockStyle.Top, Text = "Резервное" )
    
    let panelOnOff = new Panel(Parent = panPow, Dock = Dock.Top, Height = 40)
    let _ = new Panel(Parent = panelOnOff, Dock = Dock.Left, Width = 5)
    let btn = new Button( Parent = panelOnOff, Dock = Dock.Left, Width = 60,
                    FlatStyle = FlatStyle.Flat, Text = "Выкл")
    MainWindow.setTooltip btn "Выключить питание"

    let _ = new Panel(Parent = panelOnOff, Dock = Dock.Left, Width = 5)
    let btn = new Button( Parent = panelOnOff, Dock = Dock.Left, Width = 60,
                    FlatStyle = FlatStyle.Flat, Text = "Вкл")
    MainWindow.setTooltip btn "Включить питание"

    panPow.InvertChildrenOrder()
    panPow

let initialize =
    let btnPow = new Button( Parent = placeholder, Dock = Dock.Top,
                                FlatStyle = FlatStyle.Flat, Text = "Питание",
                                Height = 30)

    let popupPow = new MyWinForms.Popup(panPow())
    
    btnPow.Click.Add(fun _  -> 
        popupPow.Show(btnPow)
        )


    
    
        

    fun () -> ()