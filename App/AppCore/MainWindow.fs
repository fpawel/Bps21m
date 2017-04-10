module MainWindow

#nowarn "40"

open System
open System.Windows.Forms
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

open MyWinForms.Utils

[<AutoOpen>]
module private Helpers =
    type CheckBoxColumn = MyWinForms.GridViewCheckBoxColumn
    type TextColumn = DataGridViewTextBoxColumn
    let (~%%) x = x :> DataGridViewColumn

    let tooltip = new ToolTip(AutoPopDelay = 5000, InitialDelay = 1000,  ReshowDelay = 500, ShowAlways = true)

let form =     
    let x = new Form(Font = new Font("Consolas", 12.f), WindowState = FormWindowState.Maximized )
    let path = IO.Path.Combine( IO.Path.ofExe, "icon.ico")
    try        
        let customIcon = new Icon( path )
        x.Icon <- customIcon
    with e ->
        Logging.error "fail to set icon.ico from %A : %A" path e
    let mutable isClosed = false    
    x

let setTooltip<'a when 'a :> Control > (x:'a) text = 
    tooltip.SetToolTip(x, text)



let mainLayer = new Panel( Parent = form, Dock = DockStyle.Fill)

let rightTabContentPlaceholder,setActivePageTitle = 
    let par1 = new Panel(Parent = mainLayer, Dock = DockStyle.Fill)
    let rightTabPagePlaceholder = new Panel(Parent = par1, Dock = DockStyle.Fill)

    let p = new Panel(Dock = DockStyle.Top, Height = 30, Parent = par1)
    let _ = new Panel(Parent = p, Dock = DockStyle.Top, Height = 5)
    let x = new Label(Parent = p, Dock = DockStyle.Top, 
                        Height = 20, TextAlign = ContentAlignment.MiddleLeft)
    let _ = new Panel(Parent = p, Dock = DockStyle.Top, Height = 5)
    x.SetInfoStyle()
    rightTabPagePlaceholder,(fun s -> x.Text <- s )

let tabButtonsPlaceholder, leftBottomTabContentPlaceHolder, leftBottomPlaceHolder = 
    let _ = new Panel(Parent = mainLayer, Dock = DockStyle.Left, Width = 3)
    let x = new Panel(Parent = mainLayer, Dock = DockStyle.Left, Width = 135)

    let leftBottomPlaceHolder = new Panel(Parent = x, Dock = DockStyle.Fill)     

    
    
    let _ = new Panel(Parent = x, Dock = DockStyle.Top, Height = 10)
    let leftTabContenPlaceholder = new Panel(Parent = x, Dock = DockStyle.Top)  

    let _ = new Panel(Parent = x, Dock = DockStyle.Top, Height = 10)
    let left_top_TabButtonsPlaceholder = new Panel(Parent = x, Dock = DockStyle.Top)

    let _ = new Panel(Parent = mainLayer, Dock = DockStyle.Left, Width = 3)
    left_top_TabButtonsPlaceholder, leftTabContenPlaceholder, leftBottomPlaceHolder

let bottomLayer = 
    let _ = new Panel(Parent = mainLayer, Dock = DockStyle.Bottom, Height = 3)
    let x = new Panel(Parent = mainLayer, Dock = DockStyle.Bottom, Height = 25)
    let _ = new Panel(Parent = mainLayer, Dock = DockStyle.Bottom, Height = 3)    
    x

let labelPerformingInfo = 
    new Label(Parent = bottomLayer, Dock = DockStyle.Fill, Text = "",
                TextAlign = ContentAlignment.MiddleLeft )

type Tabsheet = 
    | TabsheetParty
    | TabsheetData
    | TabsheetScenary
    member x.Title = Tabsheet.title x
    static member values = FSharpType.unionCasesList<Tabsheet>
    
    static member title = function
        | TabsheetParty ->   "Партия"
        | TabsheetData  ->   "Данные"
        | TabsheetScenary -> "Сценарий"

    static member descr = function
        | TabsheetParty ->   "Партия настраиваемых приборов"
        | TabsheetData  ->   "Данные приборов партии, полученные при настройке"
        | TabsheetScenary -> "Сценарий настройки приборов партии"

module private TabPagesHelp =
    let content = 
        Tabsheet.values 
        |> List.map(fun x -> 
            let p1 = new Panel( Dock = DockStyle.Fill, Parent = rightTabContentPlaceholder, Visible = false, AutoScroll = true)
            let p2 = new Panel( Dock = DockStyle.Fill, Parent = leftBottomTabContentPlaceHolder, Visible = false, AutoScroll = true)
            x, (p1,p2))
        |> Map.ofList

type Tabsheet with
    member x.BottomTab = snd TabPagesHelp.content.[x]
    member x.RightTab = fst TabPagesHelp.content.[x]
    static member content x =
        TabPagesHelp.content.[x]
    member x.ShowContent() =
        Tabsheet.showContent x
    static member showContent tabPage =        
        Tabsheet.values 
        |> List.iter ( fun x -> 
            let v = x=tabPage
            x.BottomTab.Visible <- v
            x.RightTab.Visible <- v)

let newLogginWebbrowser parent = 
    let webb =  
        new WebBrowser(Parent = parent, BackColor = TabsheetScenary.RightTab.BackColor, 
                        Dock = DockStyle.Fill, AllowNavigation = true, Url = null,
                        IsWebBrowserContextMenuEnabled = false, 
                        AllowWebBrowserDrop = false )
    webb.DocumentCompleted.Add <| fun _ ->
        webb.AllowNavigation <- false
        if  webb.Document <> null && webb.Document.Body <> null then 
            webb.Document.Body.ScrollIntoView(false)
    webb

let webbJournal = newLogginWebbrowser TabsheetScenary.RightTab

let newGridView parent name = 
    let x = 
        GridView.newFlexible() 
        |> GridView.withNoSortColumns
    x.Parent <- parent
    x.Name <- name
    x.BackgroundColor <- TabsheetScenary.RightTab.BackColor    
    x

let gridScenary = 
    let splt = new Splitter(Parent = TabsheetScenary.RightTab, Dock = DockStyle.Left, Width = 3, BackColor = Color.LightGray)
    
    let pan = new Panel (Parent = TabsheetScenary.RightTab, Dock = DockStyle.Left,
                            MinimumSize = Size(300,0), MaximumSize = Size(1000,0),
                            Width = AppConfig.config.View.ScnDetailTextSplitterDistance)
    let x = newGridView pan "ScenaryGridView"
//        new DataGridView( Parent = pan, AutoGenerateColumns = false, 
//                            Name = "ScenaryGridView", 
//                            Dock = DockStyle.Fill, 
//                            Width = AppConfig.config.View.ScnDetailTextSplitterDistance,
//                            ColumnHeadersHeight = 40, 
//                            RowHeadersWidth = 30,
//                            
//                            AllowUserToResizeColumns = false,
//                            AllowUserToResizeRows = false,
//                            AllowUserToAddRows = false,
//                            AllowUserToDeleteRows = false,   
//                            
//                            RowHeadersVisible = false  ,
//
//                            AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.AllCells,
//                            AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill,
//
//                            BorderStyle = BorderStyle.None, 
//                            BackgroundColor = TabsheetScenary.RightTab.BackColor  )
//    x.ColumnHeadersHeightSizeMode <- DataGridViewColumnHeadersHeightSizeMode.AutoSize
//    x.DefaultCellStyle.WrapMode <- DataGridViewTriState.True 
//    let autoResizeRows _ =
//        x.AutoResizeRows()
//    x.SizeChanged.Add autoResizeRows
//    x.DataBindingComplete.Add autoResizeRows

    form.FormClosing.Add <| fun _ ->
        AppConfig.config.View.ScnDetailTextSplitterDistance <- pan.Width

    x.Columns.AddRange            
        [|  %% new TextColumn(DataPropertyName = "Name", HeaderText = "Операция", 
                               AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill)
            %% new TextColumn(DataPropertyName = "Delaytime", HeaderText = "Задержка", 
                                AutoSizeMode = DataGridViewAutoSizeColumnMode.AllCells) 
            %% new TextColumn(DataPropertyName = "Status", HeaderText = "Статус", 
                                AutoSizeMode = DataGridViewAutoSizeColumnMode.AllCells) |]
    
    x



let gridProducts = 
    newGridView TabsheetParty.RightTab "PartyDataGrid"
    |> GridView.withDisableSelection
    

let gridData =  
    let x = 
        newGridView TabsheetData.RightTab "GridData"
        |> GridView.withDisableSelection
    x.ReadOnly <- true

    ("Прибор", "What") ::  [   for pt in Bps21.ProductionPoint.values -> pt.What, pt.Property ]
    |> List.map(fun (a,b) -> 
         new TextColumn( HeaderText = a, DataPropertyName = b, AutoSizeMode = DataGridViewAutoSizeColumnMode.AllCells )  )
    |> x.Columns.AddColumns
        
    x

let productsToolsLayer = new Panel(Parent = TabsheetParty.BottomTab, Dock = DockStyle.Left, Width = 40 ) 
    
let errorMessageBox title message = 
    Logging.error "%A, %s" title message
    form.PerformThreadSafeAction <| fun () ->
        MessageBox.Show( message, title, MessageBoxButtons.OK, MessageBoxIcon.Error ) 
        |> ignore

let onExeption (e:Exception) = 
    Logging.error "Исключение %A" e 
    form.PerformThreadSafeAction <| fun () ->
        MessageBox.Show( sprintf "%A" e ,"Исключение", MessageBoxButtons.OK, MessageBoxIcon.Error ) 
        |> ignore
    System.Environment.Exit(1)
    failwith ""

open AppConfig
open AppConfig.View

let initialize =
    let get'grids() = 
        form.enumControls
            (fun x -> 
                if x.GetType()=  typeof<DataGridView>  then                     
                    Some (x :?> DataGridView) 
                else None)
            id
    form.FormClosing.Add <| fun _ -> 
        config.View.Grids <-
            get'grids()
            |> Seq.map( fun g -> 
                g.Name,
                    {   ColWidths = [for c in g.Columns -> c.Width]
                        ColumnHeaderHeight = g.ColumnHeadersHeight } )     
            |> Map.ofSeq

    let rec h = EventHandler( fun _ _ -> 
        form.Activated.RemoveHandler h

        // показать оштбку файла конфигурации если есть
        match AppConfig.error with
        | None -> ()
        | Some error ->
            MessageBox.Show( error, "Ошибка файла конфигурации", MessageBoxButtons.OK, MessageBoxIcon.Warning)
            |> ignore
        
        for g in get'grids() do 
            let dt = config.View.Grids.TryFind g.Name
            
            g.ColumnHeadersHeight <-
                match dt with
                | Some { ColumnHeaderHeight = h} -> h
                | _ -> g.ColumnHeadersHeight
                |> max (let sz = TextRenderer.MeasureText( "X", g.ColumnHeadersDefaultCellStyle.Font )
                        sz.Height + 7 )
            [for c in g.Columns -> c ]  |> List.iteri( fun n c -> 
                let w = 
                    match dt with            
                    | Some { ColWidths = dt } when n < dt.Length ->  dt.[n]
                    | _ -> 
                        let sz = TextRenderer.MeasureText( c.HeaderText, c.HeaderCell.Style.Font )
                        sz.Width + 10
                c.Width <- max 50 w ))

    form.Activated.AddHandler h    
    fun () -> ()