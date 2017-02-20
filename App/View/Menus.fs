module Bps21.View.Menus

open System
open System.Windows.Forms
open System.Drawing
open System.Collections.Generic
open System.ComponentModel
open System.ComponentModel.DataAnnotations

open MyWinForms.TopDialog
open MainWindow
open Bps21.ViewModel.Operations
open Thread2
open Bps21
open Bps21.PartyWorks
open Bps21.View

[<AutoOpen>]
module private Helpers =
    type P = Bps21.ViewModel.Product     
    let party = AppContent.party
    let popupDialog = MyWinForms.PopupDialog.create
    type Dlg = MyWinForms.PopupDialog.Options
    let none _ = None
    let form = MainWindow.form


    let getSelectedProducts() = 
        seq{ for x in gridProducts.SelectedCells -> x.RowIndex, x.OwningRow }
        |> Map.ofSeq
        |> Map.toList
        |> List.map( fun (_,x) -> x.DataBoundItem :?> P )

    let simpleMenu = MyWinForms.Utils.buttonsMenu (new Font("Consolas", 12.f)) ( Some 300 ) 

    let popupNumberDialog<'a>     
        prompt title tryParse work 
        (btn : Button) 
        (parentPopup : MyWinForms.Popup) =
        let tb = new TextBox(Width = 290, Text = (party.NewValidAddr() |> string) )                    
        let dialog,validate  = 
            popupDialog 
                { Dlg.def() with 
                    Text = Some prompt
                    ButtonAcceptText = "Применить" 
                    Title = title
                    Width = 300
                    Content = tb }
                ( fun () -> 
                    tryParse tb.Text)
                ( fun (value : 'a) ->  
                    parentPopup.Hide()
                    work value ) 
        tb.TextChanged.Add <| fun _ -> validate()                        
        dialog.Show btn


    let popupTuneStep (current :Current)    
        (btn : Button) 
        (parentPopup : MyWinForms.Popup) =

        let p = new Panel(Width = 290)
        let r2 = new RadioButton(Parent = p, Dock = DockStyle.Top, Text = "Уменьшение")
        let r1 = new RadioButton(Parent = p, Dock = DockStyle.Top, Text = "Увеличение", Checked = true )        
        let tb1 = new TextBox(Parent = p, Dock = DockStyle.Top, Text = "1" )
        let _ = new Label(Parent = p, Dock = DockStyle.Top, Text = "Шаг подстройки")       
        
        let dialog,validate  = 
            popupDialog 
                { Dlg.def() with 
                    ButtonAcceptText = "Применить" 
                    Title = "Подстройка тока " + current.What
                    Width = 300
                    Content = p }
                ( fun () -> 
                    let b,v = Int32.TryParse tb1.Text
                    if b && v > 0 && v < 0xffff then
                        Some (v, if r1.Checked then 1m else 0m)
                    else 
                        None )
                ( fun (value, inc) ->  
                    parentPopup.Hide()
                    Device.CmdTune(current,value).Send inc
                    ) 
        tb1.TextChanged.Add <| fun _ -> validate()
        
        dialog.Show btn


let deviceToolsPopup = 

    
    [   yield "Установка адресов", fun _ (parentPopup : MyWinForms.Popup) ->
            parentPopup.Close()
            party.SetAddrs()
            
        yield! 
            [   Device.Cmd.MainPowerOn
                Device.Cmd.Set4mA
                Device.Cmd.Set20mA
                Device.Cmd.Adjust4mA
                Device.Cmd.Adjust20mA ]
            |> List.map ( fun x ->
                x.What, fun _ (parentPopup : MyWinForms.Popup) -> 
                    parentPopup.Close()
                    x.Send() )
        yield!
            [I_4mA; I_20mA]
            |> List.map (fun x -> 
                let what = "Подстройка тока " + x.What
                what,  popupTuneStep x ) ]
    |> simpleMenu
    

let private initButtons1 = 
    let buttons1placeholder = 
        new Panel
            (   Parent = TabsheetParty.BottomTab, Dock = DockStyle.Top, Height = 89 )

    
    let imgbtn left top key tooltip f = 
        let x = 
            new Button( Parent = buttons1placeholder, Left = left, Top = top,
                        ImageKey = key, Width = 40, Height = 40,
                        FlatStyle = FlatStyle.Flat,
                        ImageList = Widgets.Icons.instance.imageList1)
        MainWindow.setTooltip x tooltip
        x.Click.Add <| fun _ ->  
            f x
        x

    let btnOpenParty = imgbtn 3 3 "open" "Открыть ранее сохранённую партию" OpenPartyDialog.showDialog
    let btnNewParty = imgbtn 46 3 "add" "Создать новую партию" PartyProductsDialogs.createNewParty

    let btnAddProd = imgbtn 3 46 "additem" "Добавить в партию новые приборы" PartyProductsDialogs.addProducts
    let btnDelProd = 
        let b = imgbtn 46 46 "removeitem" "Удалить выбранные приборы из партии" PartyProductsDialogs.deleteProducts
        b.Visible <- false
        let g = gridProducts
        g.SelectionChanged.Add <| fun _ ->
            b.Visible <- g.SelectedCells.Count > 0 
        b

    Thread2.IsRunningChangedEvent.addHandler <| fun (_,isRunning) ->
        [btnOpenParty; btnNewParty; btnAddProd; btnDelProd ]
        |> Seq.iter(fun b -> b.Enabled <- not isRunning )

    let _ = imgbtn 89 3 "todo" "Выбрать опрашиваемые параметры" ( fun b ->
        let popup = 
            MyWinForms.Utils.popupConfig 
                "Опрашиваемые параметры" 
                (SelectPhysVars()) 
                PropertySort.Alphabetical
        popup.Closed.Add( fun _ ->
           View.Products.Columns.setVisibilityFromConfig() )
        popup.Show b )    

    fun () -> ()
    
open Bps21.View.TopBar

let initialize =
    let buttonRun = new Button( Parent = TopBar.thread1ButtonsBar, Dock = DockStyle.Left, AutoSize = true,
                                ImageKey = "run",
                                Text = (sprintf "%A" Thread2.scenary.Value.FullName),
                                ImageAlign = ContentAlignment.MiddleLeft,
                                TextImageRelation = TextImageRelation.ImageBeforeText,
                                TextAlign = ContentAlignment.MiddleCenter,
                                FlatStyle = FlatStyle.Flat,
                                ImageList = Widgets.Icons.instance.imageList1)
    TopBar.thread1ButtonsBar.Controls.Add <| new Panel(Dock = DockStyle.Left, Width = 3)
    MainWindow.setTooltip buttonRun ("Выполнить " + buttonRun.Text)
    buttonRun.Click.Add <| fun _ ->  
        Thread2.run Thread2.scenary.Value
    Thread2.scenary.AddChanged <| fun (_,x) ->
        buttonRun.Text <- sprintf "%A" x.FullName
        MainWindow.setTooltip buttonRun ("Выполнить " + buttonRun.Text)
        buttonRun.AutoSize <- false
        buttonRun.AutoSize <- true

    let (<==) (text,tooltip) f = 
        let b = new Button( Parent = TopBar.thread1ButtonsBar, Dock = DockStyle.Left, 
                            FlatStyle = FlatStyle.Flat,
                            Text = text, AutoSize = true )
        b.Click.Add <| fun _ ->  
            f b    
        MainWindow.setTooltip b tooltip
        TopBar.thread1ButtonsBar.Controls.Add <| new Panel(Dock = DockStyle.Left, Width = 3)

    let imgBtn (key,tooltip) f = 
        let b = new Button( Parent = TopBar.thread1ButtonsBar, Dock = DockStyle.Left, 
                            ImageList = Widgets.Icons.instance.imageList1,
                            FlatStyle = FlatStyle.Flat, ImageKey = key,
                            Width = 40, Height = 40,
                            AutoSize = true )
        b.Click.Add <| fun _ ->  
            f b    
        MainWindow.setTooltip b tooltip
        TopBar.thread1ButtonsBar.Controls.Add <| new Panel(Dock = DockStyle.Left, Width = 3)
        
        
    imgBtn ( "loop","Опрос выбранных параметров приборов партии" ) <| fun _ ->
        runInterrogate()
    
    imgBtn ("network", "Управление приборами") <| fun x ->
        deviceToolsPopup.Show x

    do
        let x = 
            new Button( Parent = TopBar.thread1ButtonsBar, Dock = DockStyle.Left, AutoSize = true,
                        ImageKey = "script", Width = 40, Height = 40,
                        FlatStyle = FlatStyle.Flat,
                        ImageList = Widgets.Icons.instance.imageList1)
        MainWindow.setTooltip x "Выбрать сценарий настройки"
        x.Click.Add <| fun _ ->  
            SelectScenaryDialog.showSelectScenaryDialog x
        TopBar.thread1ButtonsBar.Controls.Add <| new Panel(Dock = DockStyle.Left, Width = 3)

    initButtons1()

    let buttonSettings = 
        new Button( Parent = right, Height = 40, Width = 40, Visible = true,
                    ImageList = Widgets.Icons.instance.imageList1,
                    FlatStyle = FlatStyle.Flat,
                    Dock = DockStyle.Right, ImageKey = "settings")
    right.Controls.Add <| new Panel(Dock = DockStyle.Right, Width = 3)
    setTooltip buttonSettings "Параметры приложения" 

    buttonSettings.Click.Add <| fun _ ->            
        let popup = 
            MyWinForms.Utils.popupConfig 
                "Параметры" 
                (AppConfigView())
                PropertySort.NoSort
        popup.Font <- form.Font        
        popup.Show(buttonSettings)

        
    Thread2.scenary.Set PartyWorks.main

    fun () -> ()