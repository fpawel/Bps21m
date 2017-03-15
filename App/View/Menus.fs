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
open Bps21.Hard 

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

    let simpleMenu  = MyWinForms.Utils.buttonsMenu (new Font("Consolas", 12.f)) ( Some 300 ) 

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


let deviceToolsPopup = 
    [   yield "Установка адресов", fun _ (parentPopup : MyWinForms.Popup) ->
            parentPopup.Close()
            party.RunSetAddrs()       
            
        for cmd in Stend.Cmd.values do     
            yield cmd.What, fun _ (parentPopup : MyWinForms.Popup) ->
                parentPopup.Close()
                party.RunWriteStend(cmd)

        yield!
            Hard.Product.Cmd.values
                |> List.map( fun cmd -> 
                    cmd.What, 
                        popupNumberDialog 
                            (sprintf "Введите значение аргумента команды %A" cmd.What)
                            cmd.What
                            String.tryParseDecimal
                            (fun value -> party.RunWriteProduct(cmd,value) ) ) ]
    |> MyWinForms.Utils.buttonsMenu (new Font("Consolas", 10.f)) ( Some 400 ) 
    

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