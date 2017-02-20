module Bps21.View.TabPages

open System
open System.Windows.Forms
open System.Drawing
open System.Collections.Generic

open MyWinForms.Utils
open WinFormsControlUtils

open MainWindow
open Bps21
open Bps21.View.Products

type private P = Bps21.ViewModel.Product

let private onSelect = function
    | TabsheetParty -> 
        gridProducts.Parent <- TabsheetParty.RightTab    
    | _ -> ()
        
let getSelected, setSelected,_ =
    
    radioButtons 
        tabButtonsPlaceholder 
        Tabsheet.values
        Tabsheet.title
        Tabsheet.descr
        (fun tabPage -> 
            setActivePageTitle tabPage.Title
            onSelect tabPage
            tabPage.ShowContent() ) 
