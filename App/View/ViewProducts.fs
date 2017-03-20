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
    let private col header dataprop width = 
        %% new TextColumn(DataPropertyName = dataprop, HeaderText = header, Width = width)    

    let isChecked = %% new CheckBoxColumn(DataPropertyName = "IsChecked", Width = 50)
    let addr = col "#" "Addr" 50
    let serial = col "№" "Serial" 50
        
    let private col1 header dataprop width = 
        %% new TextColumn(DataPropertyName = dataprop, HeaderText = header, Width = width, ReadOnly = true)    

    let rele = 
        let col1 header prop =  col1 header prop 50

        [   col1 "СТ." "Status"
            col1 "СП.РЕЖИМ" "SpMode"
            col1 "ОТКАЗ" "Failure"
            col1 "П1" "Porog1"
            col1 "П2" "Porog2"
            col1 "П3" "Porog3"            
        ] 

    let currProd = col1 "Iп" "ProductCurrent" 80
    let blockStatus = col1 "СТАТУС" "ProductStatus" 80
    let currStend = col1 "Iс" "StendCurrent" 80
    let conn = col1 "Связь" "Connection" 80

    let columns = 
        [   isChecked; addr; serial
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
                e.Value <- sprintf "%A %s" date text

    fun () -> ()