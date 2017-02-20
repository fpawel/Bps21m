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

module Columns =
    let private col header dataprop width = 
        %% new TextColumn(DataPropertyName = dataprop, HeaderText = header, Width = width)    

    let isChecked = %% new CheckBoxColumn(DataPropertyName = "IsChecked", Width = 50)
    let addr = col "#" "Addr" 50
    let serial = col "№" "Serial" 50
        
    let private col1 header dataprop width = 
        %% new TextColumn(DataPropertyName = dataprop, HeaderText = header, Width = width, ReadOnly = true)    

    let connection = col1 "Связь" "Connection" 80
    let conc = col1 "C" "Conc" 80
    let curr = col1 "I" "Curr" 80
    let tens = col1 "U" "Tens" 80
    let porog1 = col1 "П1" "Porog1" 50
    let porog2 = col1 "П2" "Porog2" 50
    let porog3 = col1 "П3" "Porog3" 50

    let setVisibilityFromConfig() =
        let isvar var = Set.contains var AppConfig.config.View.DevVars
        conc.Visible <- isvar DevConc 
        curr.Visible <- isvar DevCurr
        tens.Visible <- isvar DevTens 
    
        porog1.Visible <- isvar DevConc 
        porog2.Visible <- isvar DevConc 
        porog3.Visible <- isvar DevConc 
    let init = 
        fun () ->
        gridProducts.Columns.AddColumns
            [   isChecked
                addr
                serial
                connection
                conc
                curr
                tens
                porog1
                porog2
                porog3
            ]
        setVisibilityFromConfig()

    
let initialize = 
    Columns.init()
    gridProducts.DataSource <- party.Products
    gridProducts.Columns.CollectionChanged.Add(fun _ ->
        gridProducts.Columns.SetDisplayIndexByOrder()
        )

    

    gridProducts.CellFormatting.Add <| fun e ->
        let hcolumn = gridProducts.Columns.[e.ColumnIndex].GetHashCode()
        let row = gridProducts.Rows.[e.RowIndex]
        let cell = row.Cells.[e.ColumnIndex]

        if hcolumn = Columns.connection.GetHashCode() then
            let text, fore, back =
                match e.Value :?> Result<string,string> option with
                | Some (Ok s) -> s, Color.Black, Color.White
                | Some (Err s) -> s, Color.Red, Color.LightGray
                | _ -> "", Color.Black, Color.White
            e.Value <- text            
            cell.Style.ForeColor <- fore
            cell.Style.BackColor <- back
        elif 
            hcolumn = Columns.porog1.GetHashCode() || 
            hcolumn = Columns.porog2.GetHashCode() || 
            hcolumn = Columns.porog3.GetHashCode()  then   
                let color, text = 
                    match e.Value :?> bool option with
                    | Some true -> Color.Red, "порог сработал"
                    | Some false -> Color.SkyBlue, "порог не сраюотал"
                    | _ -> Color.White, "нет данных о состоянии порога"
                             
                cell.Style.BackColor <-
                    match e.Value :?> bool option with
                    | Some true -> Color.Red
                    | Some false -> Color.SkyBlue
                    | _ -> Color.White
                cell.ToolTipText <- text
                e.Value <- ""    
            
    
    fun () -> ()