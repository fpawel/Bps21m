module Bps21.View.Products

open System
open System.Windows.Forms
open System.Drawing

open MainWindow
open Bps21

[<AutoOpen>]
module private Helpers =

    type C = DataGridViewColumn
    type CheckBoxColumn = MyWinForms.GridViewCheckBoxColumn
    type TextColumn = DataGridViewTextBoxColumn
    let party = Bps21.AppContent.party
    let (~%%) x = x :> C
    type P = Bps21.ViewModel.Product

module Columns =
    let private col header dataprop width readonly = 
        let x = %% new TextColumn(DataPropertyName = dataprop, HeaderText = header, MinimumWidth = width, 
                                    ReadOnly = readonly )    
        
        x
        
    let isChecked = 
        %% new CheckBoxColumn(DataPropertyName = "IsChecked", Width = 50, 
                                AutoSizeMode = DataGridViewAutoSizeColumnMode.AllCells)
    let addr = col "#" "Addr" 50 false
    let serial = col "№" "Serial" 50 false
    let year = col "Год" "Year" 50 false
    let quater = col "Квартал" "Quarter" 50 false
    
    let rele = 
        let col1 header prop =  col header prop 50 true 

        [   col1 "СТ." "Status"
            col1 "СП.РЕЖИМ" "SpMode"
            col1 "ОТКАЗ" "Failure"
            col1 "П1" "Porog1"
            col1 "П2" "Porog2"
            col1 "П3" "Porog3"            
        ] 

    let currProd = col "Iп" "ProductCurrent" 80 true
    let blockStatus = col "СТАТУС" "ProductStatus" 80 true
    let currStend = col "Iс" "StendCurrent" 80 true
    let conn = col "Связь" "Connection" 80 true

    let columns = 
        [   isChecked; addr; serial; year; quater
            conn
            blockStatus
            currProd
            currStend
        ] @  rele

    let init = 
        fun () ->
            gridProducts.Columns.AddColumns columns


let private (===) x y =  obj.ReferenceEquals(x,y)

let private listContainsObj x =
    List.exists( (===) x )

let initialize = 
    Columns.init()
    gridProducts.DataSource <- party.Products
    gridProducts.Columns.CollectionChanged.Add(fun _ ->
        gridProducts.Columns.SetDisplayIndexByOrder()  )
        
    gridProducts.CellFormatting.Add <| fun e ->
        let product = gridProducts.Rows.[e.RowIndex].DataBoundItem :?> P 
        let column = gridProducts.Columns.[e.ColumnIndex]        
        let row = gridProducts.Rows.[e.RowIndex]        
        let cell = row.Cells.[e.ColumnIndex]
        let (~%%) = listContainsObj column
        if column === Columns.conn then
            let tooltip, text, fore, back =
                match e.Value :?> Result<string,string> option with
                | Some (Ok s) -> s, "Ок", Color.Black, Color.White
                | Some (Err s) -> s, "Ошибка", Color.Red, Color.LightGray
                | _ -> "", "", Color.Black, Color.White
            if not <| String.IsNullOrEmpty text then
                e.Value <- text     
            if not <| String.IsNullOrEmpty tooltip then
                cell.ToolTipText <- tooltip      
            cell.Style.ForeColor <- fore
            cell.Style.BackColor <- back
        elif %% Columns.rele then   
            let color, text = 
                match e.Value :?> bool option with
                | Some true -> Color.Red, "включен"
                | Some false -> Color.SkyBlue, "выключен"
                | _ -> Color.White, "нет данных"                             
            cell.Style.BackColor <-
                match e.Value :?> bool option with
                | Some true -> Color.Red
                | Some false -> Color.SkyBlue
                | _ -> Color.White
            cell.ToolTipText <- text
            e.Value <- ""


    gridData.DataSource <- party.Products
    gridData.CellFormatting.Add <| fun e -> 
        let product = gridData.Rows.[e.RowIndex].DataBoundItem :?> P 
        let column = gridData.Columns.[e.ColumnIndex]        
        let row = gridData.Rows.[e.RowIndex]        
        let cell = row.Cells.[e.ColumnIndex]
        match e.Value with
        | :? Option<Logging.Line> as p  -> 
            match p with
            | None -> ()
            | Some ( date, level, text ) -> 
                cell.Style.BackColor <- Logging.backColor level
                cell.Style.ForeColor <- Logging.foreColor level
                let strDate = date.ToString("dd.MM HH:mm")
                e.Value <- 
                    match level with 
                    | Logging.Error ->  strDate + " ошибка" 
                    | _ -> strDate

        | _ -> ()

    gridData.CellDoubleClick.Add <| fun e ->
        let product = gridData.Rows.[e.RowIndex].DataBoundItem :?> P 
        let column = gridData.Columns.[e.ColumnIndex]        
        let row = gridData.Rows.[e.RowIndex]        
        let cell = row.Cells.[e.ColumnIndex]
        match cell.Value with
        | :? Option<Logging.Line> as p  -> 
            match p with
            | None -> ()
            | Some ( ( date, level, text ) as prod )-> 
                let pan = new Panel(Width = 400, Height = 300 , BorderStyle = BorderStyle.FixedSingle)
                let webb =  
                    new WebBrowser(Parent = pan, BackColor = TabsheetScenary.RightTab.BackColor, 
                                    Dock = DockStyle.Fill, AllowNavigation = true, Url = null,
                                    IsWebBrowserContextMenuEnabled = false, 
                                    AllowWebBrowserDrop = false )
                webb.Navigate "about:blank"
                webb.Document.Write String.Empty
                webb.DocumentText <- 
                    LoggingHtml.create 
                        (sprintf "%s, %s" product.What column.HeaderText) 
                        [prod]

                let _ = new Panel(Parent = pan, Dock = DockStyle.Left, Width = 5)
                let _ = new Panel(Parent = pan, Dock = DockStyle.Right, Width = 5)
                let _ = new Panel(Parent = pan, Dock = DockStyle.Top, Height = 5)
                let _ = new Panel(Parent = pan, Dock = DockStyle.Bottom, Height = 5)
                let popup = new MyWinForms.Popup(pan)                
                
                let r = gridData.GetCellDisplayRectangle(e.ColumnIndex, e.RowIndex, false)
                (r.X + gridData.Left, r.Y + gridData.Top + row.Height)
                |> Point
                |> gridData.PointToScreen
                |> popup.Show
        | _ -> ()
    fun () -> ()