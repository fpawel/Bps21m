module MainWindow

open System
open System.Windows.Forms
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

open MyWinForms.Utils

[<AutoOpen>]
module private Helpers =

    //type C = DataGridViewColumn
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

let tabButtonsPlaceholder, leftBottomTabContentPlaceHolder = 
    let _ = new Panel(Parent = mainLayer, Dock = DockStyle.Left, Width = 3)
    let x = new Panel(Parent = mainLayer, Dock = DockStyle.Left, Width = 135)

    let left_bottom_TabContentPlaceHolder = new Panel(Parent = x, Dock = DockStyle.Fill)        

    let _ = new Panel(Parent = x, Dock = DockStyle.Top, Height = 30)
    let left_top_TabButtonsPlaceholder = new Panel(Parent = x, Dock = DockStyle.Top)

    let _ = new Panel(Parent = mainLayer, Dock = DockStyle.Left, Width = 3)
    left_top_TabButtonsPlaceholder, left_bottom_TabContentPlaceHolder

let bottomLayer = 
    let _ = new Panel(Parent = mainLayer, Dock = DockStyle.Bottom, Height = 3)
    let x = new Panel(Parent = mainLayer, Dock = DockStyle.Bottom, Height = 25)
    let _ = new Panel(Parent = mainLayer, Dock = DockStyle.Bottom, Height = 3)    
    x

let labelPerformingInfo = 
    new Label(Parent = bottomLayer, Dock = DockStyle.Fill, Text = "",
                TextAlign = ContentAlignment.MiddleLeft )

module HardwareInfo = 
    let private (~%%) x = MyWinForms.Components.LeftInfoBlock(bottomLayer, x)
    let oven = %% "Подогрев плат"
    let termo = %% "Термокамера"
    let peumo = %% "ПГС"

    let initialize = 
        [   oven;  termo; peumo 
        ] |> List.iter (fun x -> 
            x.hide() )
        fun() -> ()

type Tabsheet = 
    | TabsheetParty
    | TabsheetScenary
    | TabsheetVars
    | TabsheetErrors
    member x.Title = Tabsheet.title x
    static member values = FSharpType.unionCasesList<Tabsheet>
    
    static member title = function
        | TabsheetParty ->   "Партия"
        | TabsheetScenary -> "Сценарий"
        | TabsheetVars ->    "Данные"
        | TabsheetErrors ->  "Погрешность"   

    static member descr = function
        | TabsheetParty ->   "Партия настраиваемых приборов"
        | TabsheetScenary -> "Сценарий настройки приборов партии"
        | TabsheetVars ->    "Данные приборов партии"
        | TabsheetErrors ->  "Измеренная погрешность концентрации приборов партии"   

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

let webbJournal =    
    let x =  
        new WebBrowser(Parent = TabsheetScenary.RightTab, BackColor = TabsheetScenary.RightTab.BackColor, 
                       Dock = DockStyle.Fill, AllowNavigation = false, Url = null,
                       IsWebBrowserContextMenuEnabled = false, AllowWebBrowserDrop = false )
    x.DocumentCompleted.Add <| fun _ ->
        x.AllowNavigation <- false
        if  x.Document <> null && x.Document.Body <> null then 
            x.Document.Body.ScrollIntoView(false)
    x

let gridScenary = 
    let splt = new Splitter(Parent = TabsheetScenary.RightTab, Dock = DockStyle.Left, Width = 3, BackColor = Color.LightGray)
    let x = 
        new DataGridView( Parent = TabsheetScenary.RightTab, AutoGenerateColumns = false, 
                            Name = "ScenaryGridView", 
                            Dock = DockStyle.Left, 
                            Width = AppConfig.config.View.ScnDetailTextSplitterDistance,
                            MinimumSize = Size(200,0), MaximumSize = Size(1000,0),
                            ColumnHeadersHeight = 40, 
                            //DataSource = Thread2.operations, 
                            RowHeadersWidthSizeMode = DataGridViewRowHeadersWidthSizeMode.DisableResizing,
                            RowHeadersWidth = 30,
                            AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.None,
                            AllowUserToResizeRows = false,
                            BorderStyle = BorderStyle.None, BackgroundColor = TabsheetScenary.RightTab.BackColor  )
    form.FormClosing.Add <| fun _ ->
        AppConfig.config.View.ScnDetailTextSplitterDistance <- x.Width
    x

let gridProducts = 
    new DataGridView (                 
            AutoGenerateColumns = false, 
            Dock = DockStyle.Fill,
            BackgroundColor = TabsheetScenary.RightTab.BackColor,
            Name = "PartyDataGrid",
            ColumnHeadersHeight = 40, //DataSource = party.Products, 
            Parent = TabsheetParty.RightTab,
            RowHeadersWidthSizeMode = DataGridViewRowHeadersWidthSizeMode.DisableResizing,
            RowHeadersWidth = 30,
            AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.None,
            AllowUserToResizeRows = false,
            AllowUserToAddRows = false,
            AllowUserToDeleteRows = false,                                
            BorderStyle = BorderStyle.None  )

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
    HardwareInfo.initialize()
    
    fun () -> ()