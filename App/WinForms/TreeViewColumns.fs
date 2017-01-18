namespace MyWinForms

open System
open System.Drawing
open System.Windows.Forms
open System.Windows.Forms.VisualStyles
open System.ComponentModel

type TreeViewColumns(getCellText) = 
    inherit UserControl()

    
    let listView1 = new ListView()
    //let columnHeader1 = new ColumnHeader()
    
    let treeView1 = new TreeView()

    let treeView1_DrawNode (e: DrawTreeNodeEventArgs) =
        e.DrawDefault <- true
        let TreeNodeStates0 = enum<TreeNodeStates>(0)
        let mutable rect = e.Bounds
        if (e.State &&& TreeNodeStates.Selected) <> TreeNodeStates0 then
            if (e.State &&& TreeNodeStates.Focused) <> TreeNodeStates0 then
                e.Graphics.FillRectangle(SystemBrushes.Highlight, rect)
            else
                e.Graphics.FillRectangle(SystemBrushes.Control, rect)
        else
            e.Graphics.FillRectangle(Brushes.White, rect)
        e.Graphics.DrawRectangle(SystemPens.Control, rect)


        for intColumn = 1 to listView1.Columns.Count - 1 do
            rect.Offset(listView1.Columns.[intColumn - 1].Width, 0) |> ignore
            rect.Width <- listView1.Columns.[intColumn].Width
            e.Graphics.DrawRectangle(SystemPens.Control, rect)
            let strColumnText = getCellText e.Node intColumn
            let flags = 
                match listView1.Columns.[intColumn].TextAlign with
                | HorizontalAlignment.Center -> TextFormatFlags.HorizontalCenter
                | HorizontalAlignment.Left -> TextFormatFlags.Left
                | HorizontalAlignment.Right -> TextFormatFlags.Right
                | _ -> enum<TextFormatFlags>(0)
                |> (|||) TextFormatFlags.EndEllipsis                
            rect.Y <- rect.Y + 1
            if (e.State &&& TreeNodeStates.Selected) <> TreeNodeStates0 && (e.State &&& TreeNodeStates.Focused) <> TreeNodeStates0 then
                TextRenderer.DrawText(e.Graphics, strColumnText, e.Node.NodeFont, rect, SystemColors.HighlightText, flags);
            else
                TextRenderer.DrawText(e.Graphics, strColumnText, e.Node.NodeFont, rect, e.Node.ForeColor, e.Node.BackColor, flags);
            rect.Y <- rect.Y - 1
    do 
        base.SuspendLayout()        
        listView1.BorderStyle <- BorderStyle.None
        //listView1.Columns.AddRange [|columnHeader1|]
        listView1.Dock <- DockStyle.Top
        listView1.FullRowSelect <- true
        listView1.GridLines <- true
        listView1.Location <- new Point(0, 0)
        listView1.Name <- "listView1"
        listView1.Scrollable <- false
        listView1.Size <- new System.Drawing.Size(438, 20)
        listView1.TabIndex <- 3
        listView1.UseCompatibleStateImageBehavior <- false
        listView1.View <- View.Details

        listView1.ColumnWidthChanged.Add <|  fun _ ->
            treeView1.Focus() |> ignore
            treeView1.Invalidate() 

        listView1.ColumnClick.Add <| fun _ ->
            treeView1.Focus() |> ignore

        listView1.ColumnWidthChanging.Add <| fun _ ->
            treeView1.Focus() |> ignore
            treeView1.Invalidate() 
        
        treeView1.BorderStyle <- BorderStyle.None
        treeView1.CheckBoxes <- true
        treeView1.Dock <- DockStyle.Fill
        treeView1.DrawMode <- TreeViewDrawMode.OwnerDrawAll
        treeView1.HideSelection <- false
        treeView1.Location <- Point(0, 20)
        treeView1.Name <- "treeView1"        
        treeView1.TabIndex <- 2

        treeView1.Click.Add <| fun _ ->
            let p = treeView1.PointToClient(Control.MousePosition)
            let tn = treeView1.GetNodeAt(p)
            if tn <> null then
                treeView1.SelectedNode <- tn
        treeView1.DrawNode.Add treeView1_DrawNode

        //base.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F)
        ///base.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        base.Controls.Add(treeView1)
        base.Controls.Add(listView1)
        //base.Name <- "TreeViewColumns"
        //base.Size = new System.Drawing.Size(438, 184);
        base.BackColor <- VisualStyleInformation.TextControlBorder
        base.Padding <- Padding(1)
        base.ResumeLayout(false)

    [<Description("TreeView associated with the control")>]
    [<Category("Behavior")>]
    member x.TreeView = treeView1

    [<Description("ListView associated with the control")>]
    [<Category("Behavior")>]
    member x.ListView = listView1

    