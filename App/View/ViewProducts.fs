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

let initialize = 
    gridProducts.DataSource <- party.Products
    
    fun () -> ()