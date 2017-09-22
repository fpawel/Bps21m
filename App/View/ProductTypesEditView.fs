module Bps21.View.ProductTypesEdit

open System
open System.Windows.Forms
open System.Drawing

open Bps21
open Bps21.View

[<AutoOpen>]
module private Helpers =     

    let popupDialog = MyWinForms.PopupDialog.create
    type Dlg = MyWinForms.PopupDialog.Options

    type Dck = DockStyle

    
    let slts = [| "-"; "P";  "K" |]
    let epss = [| ""; "iaIIB";  "iaIIC";  "ibIIB"; "ibIIC" |]

let create() = 
    let source = Bps21.ProductTypes.values
    let party = Bps21.AppContent.party

    MainWindow.mainLayer.Visible <- false
    let placeHolder = new Panel(Parent = MainWindow.form, Dock = Dck.Fill)

    let g = 
        MyWinForms.Utils.GridView.newFlexible() 
        |> MyWinForms.Utils.GridView.withNoSortColumns
    g.Parent <- placeHolder
    g.BackgroundColor <-  MainWindow.TabsheetScenary.RightTab.BackColor    
    g.DataSource <- source
    g.DataError.Add( fun e ->
        //Logging.error "%d,%d:%A %A" e.ColumnIndex e.RowIndex g.Rows.[e.RowIndex].Cells.[e.ColumnIndex].Value e.Exception
        e.ThrowException <- false
        )
    

    let toolbar = new Panel(Parent = placeHolder, Dock = Dck.Top, Height = 40)

    let (~%%) k = 
        let x = new Button(Parent = toolbar, Height = 40, Width = 40, 
                           ImageList = Widgets.Icons.instance.imageList1,
                           Dock = Dck.Right, ImageKey = k)
        let _ = new Panel(Parent = toolbar, Dock = Dck.Right, Width = 5)
        x

    let getSelectedProductTypes() =     
        seq{ for x in g.SelectedCells -> x.RowIndex, x.OwningRow }
        |> Map.ofSeq
        |> Map.toList
        |> List.map( fun (_,x) -> x.DataBoundItem :?> Bps21.ProductType )

    [   "Number", "№"
        "Ucc", "U пит."
        "Uout", "Uвых"
        "DUcc1", "мин.Uвых"
        "DUcc2", "макс.Uвых" ]
    |> List.iter( fun (pth,hdr) -> 
        (   new DataGridViewTextBoxColumn ( DataPropertyName = pth, HeaderText = hdr ) )
        |> g.Columns.Add 
        |> ignore ) 

    let c = new DataGridViewComboBoxColumn()
    c.DataSource <- epss
    c.DataPropertyName <- "ExplosionProtectionStr"    
    g.Columns.Add c |> ignore

    let c = new DataGridViewComboBoxColumn()
    c.DataSource <- slts
    c.DataPropertyName <- "SwitchLoadTypeStr"    
    g.Columns.Add c |> ignore


    [   "Porog1", "П1"
        "Porog2", "П2"
        "Porog3", "П3"
        "ReservedPower", "Резерв.пит."
        "Tune", "Подстройка" 
        "LoadCapacity", "Нагр.сп."  
        ]
    |> List.iter( fun (pth,hdr) -> 
        (   new DataGridViewCheckBoxColumn (DataPropertyName = pth, HeaderText = hdr) )
        |> g.Columns.Add 
        |> ignore ) 

    let btnSetDefault = %% "clear"    
    MainWindow.setTooltip btnSetDefault "Установить значения по умолчанию" 
    btnSetDefault.Click.Add <| fun _ ->
        let popup,_ = 
            popupDialog
                { Dlg.def() with 
                    Dlg.Text = Some  "Подтвердите необходимость установки значений по умолчанию." 
                    Dlg.ButtonAcceptText = "Применить" 
                    Dlg.Title = "Таблица исполнений"
                    Width = 300 }
                ( fun () -> Some () )
                ( fun () ->
                    ProductTypes.values.Clear()
                    for pt in ProductTypes.defaultProductTypes do
                        ProductTypes.values.Add(pt))
        popup.Show(btnSetDefault)


    let btnAdd = %% "additem"    
    MainWindow.setTooltip btnAdd "Создать новое исполнение" 
    btnAdd.Click.Add <| fun _ ->
        let popup,_ = 
            popupDialog
                { Dlg.def() with 
                    Dlg.Text = Some  "Пожалуйста, подтвердите необходимость создания нового исполнения" 
                    Dlg.ButtonAcceptText = "Создать" 
                    Dlg.Title = "Новое исполнение"
                    Width = 300 }
                ( fun () -> Some () )
                ( fun () ->
                    source.Add(ProductTypes.values.[0]))
        popup.Show(btnAdd)
    
    let btnDel = %% "removeitem"
    g.SelectionChanged.Add <| fun _ ->
        btnDel.Visible <- getSelectedProductTypes() |> List.isEmpty |> not
    MainWindow.setTooltip btnDel "Удалить выделенные исполнения" 
    btnDel.Click.Add <| fun _ ->
        let popup,_ = 
            popupDialog
                { Dlg.def() with 
                    Dlg.Text = 
                        getSelectedProductTypes() 
                        |> Seq.toStr ", " (fun (x:ProductType) -> x.What)
                        |> sprintf "Пожалуйста, подтвердите необходимость удаления исполнений %s" 
                        |> Some
                    Dlg.ButtonAcceptText = "Удалить" 
                    Dlg.Title = "Удалить исполнения" }
                ( fun () -> Some () )
                (fun () -> 
                    getSelectedProductTypes() 
                    |> List.iter ( ProductTypes.values.Remove >> ignore ) )
        popup.Show(btnDel)

    let btnClose = %% "close"
    btnClose.Click.Add <| fun _ ->
        placeHolder.Parent <- null
        MainWindow.mainLayer.Visible  <- true
        party.RaisePropertyChanged "ProductType"
        AppContent.save()
        ProductTypes.save()
        Thread2.scenary.Set (PartyWorks.main())

    placeHolder



    




    


    