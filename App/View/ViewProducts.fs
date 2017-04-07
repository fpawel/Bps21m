module Bps21.View.Products

open System
open System.Windows.Forms
open System.Drawing
open System.Collections.Generic

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
        let x = %% new TextColumn(DataPropertyName = dataprop, HeaderText = header, Width = width, 
                                    ReadOnly = readonly )    
        
        x
        
    let isChecked = %% new CheckBoxColumn(DataPropertyName = "IsChecked", Width = 50)
    let addr = col "#" "Addr" 50 false
    let kind = col "Тип" "Kind" 50 true
    let serial = col "№" "Serial" 50 false
        
    
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
        [   isChecked; addr; kind; serial
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
    let getProductOfRow rowIndex =
        gridProducts.Rows.[rowIndex].DataBoundItem :?> P
    gridProducts.CellFormatting.Add <| fun e ->
        let product = getProductOfRow e.RowIndex
        let column = gridProducts.Columns.[e.ColumnIndex]        
        let row = gridProducts.Rows.[e.RowIndex]        
        let cell = row.Cells.[e.ColumnIndex]
        let (~%%) = listContainsObj column
        if column === Columns.conn then
            let text, fore, back =
                match e.Value :?> Result<string,string> option with
                | Some (Ok s) -> s, Color.Black, Color.White
                | Some (Err s) -> s, Color.Red, Color.LightGray
                | _ -> "", Color.Black, Color.White
            e.Value <- text            
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


    gridData.CellFormatting.Add <| fun e ->
        if e.ColumnIndex = 0 then () else
        let cellTag = gridData.Columns.[e.ColumnIndex].Tag
        if cellTag = null then () else
        let p = gridData.Columns.[e.ColumnIndex].Tag :?> P
        let row = gridData.Rows.[e.RowIndex]
        let cell = row.Cells.[e.ColumnIndex]
        match MainWindow.ProdPointRow.tryGetProdPointOfRow row with
        | None -> ()
        | Some prodPt ->            
            match p.Product.Production.TryFind prodPt with
            | None -> ()
            | Some ( date, level, text ) -> 
                cell.Style.BackColor <- Logging.backColor level
                cell.Style.ForeColor <- Logging.foreColor level
                let strDate = date.ToString("dd.MM HH:mm")
                e.Value <- 
                    match level with 
                    | Logging.Error ->  strDate + " ошибка" 
                    | _ -> strDate

    gridData.CellDoubleClick.Add <| fun e ->
        if e.ColumnIndex = 0 then () else
        let p = gridData.Columns.[e.ColumnIndex].Tag :?> P
        let row = gridData.Rows.[e.RowIndex]
        let cell = row.Cells.[e.ColumnIndex]
        match MainWindow.ProdPointRow.tryGetProdPointOfRow row with
        | None -> ()
        | Some prodPt ->            
            match Map.tryFind prodPt p.Product.Production with
            | None -> ()
            | Some x ->
                let pan = new Panel(Width = 400, Height = 600 , BorderStyle = BorderStyle.FixedSingle)
                let webb = MainWindow.newLogginWebbrowser pan
                let _ = new Panel(Parent = pan, Dock = DockStyle.Left, Width = 5)
                let _ = new Panel(Parent = pan, Dock = DockStyle.Right, Width = 5)
                let _ = new Panel(Parent = pan, Dock = DockStyle.Top, Height = 5)
                let _ = new Panel(Parent = pan, Dock = DockStyle.Bottom, Height = 5)
                let popup = new MyWinForms.Popup(pan)
                
                LoggingHtml.set webb (sprintf "%s, %s" p.What prodPt.What) [x]

                let r = gridData.GetCellDisplayRectangle(e.ColumnIndex, e.RowIndex, false)
                (r.X + gridData.Left, r.Y + gridData.Top + row.Height)
                |> Point
                |> gridData.PointToScreen
                |> popup.Show

                

    fun () -> ()