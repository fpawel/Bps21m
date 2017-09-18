module Bps21.View.Report 

open System
open System.Windows.Forms
open System.Drawing

open Bps21
open Bps21.View

type private Dck = DockStyle

let placeHolder = new Panel(Dock = Dck.Fill)

let webBrowser = 
    let x = 
        new WebBrowser(Parent = placeHolder, Dock = Dck.Fill, Url = null,
                       IsWebBrowserContextMenuEnabled = false, AllowWebBrowserDrop = false)
    x
let toolbar = new Panel(Parent = placeHolder, Dock = Dck.Top, Height = 40)



let private (~%%) k = 
    let x = new Button(Parent = toolbar, Height = 40, Width = 40, 
                       ImageList = Widgets.Icons.instance.imageList1,
                       FlatStyle = FlatStyle.Flat,
                       Dock = Dck.Right, ImageKey = k)
    let _ = new Panel(Parent = toolbar, Dock = Dck.Right, Width = 5)
    x

let btnSave = %% "save"
let btnPrint = %% "print"
let btnClose = %% "close"



let initialize =
    btnClose.Click.Add <| fun _ ->
        placeHolder.Parent <- null
        MainWindow.mainLayer.Visible <- true
        TopBar.placeHolder.Visible <- true


    btnSave.Click.Add <| fun _ ->
        let dlg = 
            new SaveFileDialog
                (Filter = "веб-страница (*.html)|*.html|Все файлы (*.*)|*.*",
                 FilterIndex = 1, RestoreDirectory = true )
        if dlg.ShowDialog() <> DialogResult.OK then () else
        IO.File.WriteAllText
            (dlg.FileName, webBrowser.Document.Body.Parent.OuterHtml, 
                Text.Encoding.GetEncoding(webBrowser.Document.Encoding))

    btnPrint.Click.Add <| fun _ ->
        webBrowser.ShowPrintDialog()

        
    fun () -> ()
