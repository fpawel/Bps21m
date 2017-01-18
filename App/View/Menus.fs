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


    let popupTermociclingDialog  
        (btn : Button) 
        (parentPopup : MyWinForms.Popup) =
        let ph = new Panel(Width = 290)
        let addctrl (c:Control) = 
            c.Parent <- ph
            c.Dock <- DockStyle.Top
        let tb caption text =
            addctrl (new Label(Text = caption, AutoSize = true) )
            let tb = new TextBox(Text = text)           
            addctrl tb
            tb
        let tbCount = tb "Количество термоциклов" "3"
        let tbTempMin = tb "Нижняя температура, \"С" (string <| party.GetTermoTemperature TermoLow)
        let tbTempMax = tb "Верхняя температура, \"С" (string <| party.GetTermoTemperature TermoHigh)
        let tbTime = tb "Время прогрева, час:мин" "1:00"
        
        ph.stretchHeightToContent 50
        ph.InvertChildrenOrder()

        let tryGetData() = 
            maybe {
                let! count = 
                    let b, count = Int32.TryParse tbCount.Text
                    if not b || count < 1 || count > 10 then None else Some count
                let! tempMin  = String.tryParseDecimal tbTempMin.Text
                let! tempMax  = String.tryParseDecimal tbTempMax.Text
                let! time  = 
                    let b,v = TimeSpan.TryParse tbTime.Text
                    if not b || v < TimeSpan.FromMinutes 1. || v > TimeSpan.FromHours 24. 
                    then None 
                    else Some v
                return!
                    if tempMax <= tempMin then None else
                    Some( count, tempMin, tempMax, time )
            }
                 
        let dialog,validate  = 
            popupDialog 
                { Dlg.def() with 
                    ButtonAcceptText = "Старт" 
                    Title = "Термоциклирование"
                    Width = 300
                    Content = ph }
                tryGetData
                ( fun x ->  
                    parentPopup.Hide()
                    PartyWorks.TermoChamber.termocicling x ) 
        for tb in [tbCount; tbTempMin; tbTempMax; tbTime] do
            tb.TextChanged.Add <| fun _ -> validate()                        
        dialog.Show btn

let modbusToolsPopup = 
    let setAddr = 
        popupNumberDialog 
            "Ведите адрес MODBUS от 1 до 127" 
            "Установка адреса MODBUS"
            ( fun s ->
                let b,v = Byte.TryParse s
                if b  && v > 0uy && v<128uy then Some v else None)
            (decimal >> setAddr)
    
    [   yield "Установка адреса", setAddr
        yield!
            Command.values
            |> List.filter( (<>) ResetAddy ) 
            |> List.map( fun cmd -> 
                (sprintf "MDBUS: %s" cmd.What), 
                    popupNumberDialog 
                        (sprintf "Введите значение аргумента команды %A" cmd.What)
                        cmd.What
                        String.tryParseDecimal
                        (fun value -> sendCommand (cmd,value) ) ) ]
    |> simpleMenu
    
let pneumoToolsPopup =         
    [   yield! ScalePt.values |> List.map ( fun gas -> 
            ScalePt.what gas, fun _ _  -> PartyWorks.Pneumoblock.switch gas )
        yield "Выкл.", fun _ _ -> PartyWorks.Pneumoblock.close()  ]
    |> simpleMenu

let termoToolsPopup = 

    let setpoint = 
        popupNumberDialog 
            "Введите значение уставки термокамеры"
            "Задать уставку термокамеры"
            String.tryParseDecimal
            PartyWorks.TermoChamber.setSetpoint
    let do' f _  (x : MyWinForms.Popup) = 
        x.Close()
        f()

    [   yield "Термоциклирование", popupTermociclingDialog
        yield "Старт", do' PartyWorks.TermoChamber.start
        yield "Стоп", do' PartyWorks.TermoChamber.stop
        yield "Уставка", setpoint  
        yield "Температура", do' PartyWorks.TermoChamber.read ]
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
                (ViewModel.SelectPhysVars()) 
                PropertySort.Alphabetical
        popup.Closed.Add( fun _ ->
           View.Products.updatePhysVarsGridColsVisibility()  )
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
        Thread2.run true Thread2.scenary.Value
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
    
    imgBtn ("network", "Управление приборами по Modbus") <| fun x ->
        modbusToolsPopup.Show x

    imgBtn ("pneumo", "Управление пневмоблоком") <| fun x ->
        pneumoToolsPopup.Show x   

    imgBtn  ("termochamber", "Управление термокамерой") <| fun x ->
        termoToolsPopup.Show x   

    imgBtn  ("testconn", "Проверка связи с приборами и оборудованием") 
        PartyWorks.testConnect

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

        
    Thread2.scenary.Set PartyWorks.bps21

    fun () -> ()