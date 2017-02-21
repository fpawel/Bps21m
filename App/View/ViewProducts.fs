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

    let porogs = 
        [   col1 "П1" "Porog1" 50
            col1 "П2" "Porog2" 50
            col1 "П3" "Porog3" 50 ]

    let status = 
        let col1 header prop =  col1 ( "Ст:" + header) prop 50

        [   col1 "РЕЖИМ" "StatusMode"
            col1 "ОТКАЗ" "StatusFailure"
            col1 "П1" "StatusPorog1"
            col1 "П2" "StatusPorog2"
            col1 "П3" "StatusPorog3"
        ] 

    let conc = col1 "C" "Conc" 80
    let curr = col1 "I" "Curr" 80
    let tens = col1 "U" "Tens" 80
    let conn = col1 "Связь" "Connection" 80

    let columns = 
        [   isChecked; addr; serial
            col1 "П1" "VPorog1" 80
            col1 "П2" "VPorog2" 80
            col1 "П3" "VPorog3" 80
            conn
            conc
            curr
            tens
        ] @ porogs @ status

    
    

    

    let setVisibilityFromConfig() =
        let isvar var = Set.contains var AppConfig.config.View.DevVars
        conc.Visible <- isvar DevConc 
        curr.Visible <- isvar DevCurr
        tens.Visible <- isvar DevTens 
        for col in porogs do
            col.Visible <- conc.Visible    
        for col in status do
            col.Visible <- isvar DevStatus

    let init = 
        fun () ->
            gridProducts.Columns.AddColumns columns
            setVisibilityFromConfig()


let private (===) x y =  obj.ReferenceEquals(x,y)

let private listContainsObj x =
    List.exists( (===) x )

     
    
let initialize = 
    Columns.init()
    gridProducts.DataSource <- party.Products
    gridProducts.Columns.CollectionChanged.Add(fun _ ->
        gridProducts.Columns.SetDisplayIndexByOrder()  )

    let colsOnOff = Columns.porogs @ Columns.status
    gridProducts.CellFormatting.Add <| fun e ->
        let column = gridProducts.Columns.[e.ColumnIndex]
        
        let row = gridProducts.Rows.[e.RowIndex]
        let cell = row.Cells.[e.ColumnIndex]

        if  column === Columns.conn then
            let text, fore, back =
                match e.Value :?> Result<string,string> option with
                | Some (Ok s) -> s, Color.Black, Color.White
                | Some (Err s) -> s, Color.Red, Color.LightGray
                | _ -> "", Color.Black, Color.White
            e.Value <- text            
            cell.Style.ForeColor <- fore
            cell.Style.BackColor <- back
        elif listContainsObj column colsOnOff then   
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

        
            
    
    fun () -> ()