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
    let panTv = new Panel(Height = 400)
    let buttonRun = new Button( Parent = panTv, Height = 25, Text = "Выполнить", 
                                TextAlign = ContentAlignment.MiddleLeft,
                                FlatStyle = FlatStyle.Flat, Dock = DockStyle.Bottom )
    let tv = 
        new MyWinForms.Components.TriStateCheckTreeView
                (Parent = panTv, Font = new Font( "Segue UI", 12.f ), Dock = DockStyle.Fill)
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
                        
    populate null main
     
    let getOperation() = 
        let node = tv.SelectedNode
        if node=null then None else 
        mp.[node] |> Operation.Choose1 ( fun y -> 
            ndmp.[y.FullName].StateImageIndex > 0 ) 
    
    let dialog,validate1  = 
        popupDialog
            { Dlg.def() with 
                Dlg.ButtonAcceptText = "Выбрать" 
                Width = 450
                Dlg.Content = panTv 
                Dlg.Title = "Выбрать сценарий"}
            getOperation
            Thread2.scenary.Set

    let validate() = 
        match getOperation() with
        | Some op ->
            buttonRun.Visible <- true
            buttonRun.Text <- sprintf "Выполнить %A" op.Name
        | _ ->  
            buttonRun.Visible <- false
        validate1()

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

    buttonRun.Click.Add <| fun _ ->
        getOperation()
        |> Option.iter (fun op -> 
            dialog.Close()
            Thread2.run  op 
        )
    dialog.Show