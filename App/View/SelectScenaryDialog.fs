module Bps21.View.SelectScenaryDialog

open System
open System.Windows.Forms
open System.Drawing
open System.Collections.Generic
open System.ComponentModel
open System.ComponentModel.DataAnnotations

open MyWinForms.TopDialog
open MainWindow
open Bps21
open Bps21.View
open Bps21.ViewModel.Operations
open Bps21.PartyWorks

[<AutoOpen>]
module private Helpers =
    
    //let party = AppContent.party
    let popupDialog = MyWinForms.PopupDialog.create
    type Dlg = MyWinForms.PopupDialog.Options


let showSelectScenaryDialog : Button -> unit = 
    let tv = 
        new MyWinForms.Components.TriStateCheckTreeView
                (Font = new Font( "Segue UI", 12.f ),
                    Height = 600)
    let mp = Dictionary<TreeNode,Operation>()
    let ndmp = Dictionary<string,TreeNode>()
    let rec populate (parnd:TreeNode) (x:Operation) = 
        let nd = new TreeNode(Checked = true, Text = x.Name)
        mp.[nd] <- x
        ndmp.[x.FullName] <- nd
        nd 
        |> (if parnd=null then tv.Nodes else parnd.Nodes).Add
        |> ignore
        match x with 
        | Operation.Scenary (_,xs) -> xs |> List.iter (populate nd)                
        | _ -> ()
                        
    populate null bps21
     
    let getOperation() = 
        let node = tv.SelectedNode
        if node=null then None else 
        mp.[node] |> Operation.Choose1 ( fun y -> 
            ndmp.[y.FullName].StateImageIndex > 0 ) 
    
    let dialog,validate  = 
        popupDialog
            { Dlg.def() with 
                Dlg.ButtonAcceptText = "Выбрать" 
                Width = 450
                Dlg.Content = tv 
                Dlg.Title = "Выбрать сценарий"}
            getOperation
            ( fun operation ->
                Thread2.scenary.Set operation )

    tv.StateImageIndexChanged.Add <| fun _ ->
        validate()
    tv.AfterSelect.Add <| fun _ ->
        validate()
    let root = tv.Nodes.[0]
    root.Checked <- true   
    root.Expand()
    for node in root.Nodes do
       node.Expand() 
    tv.Select()
    tv.Focus() |> ignore
    validate()
    dialog.Show