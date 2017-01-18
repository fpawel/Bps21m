module MyWinForms.Utils

open System 
open System.Windows.Forms
open System.Drawing
open System.Reflection

let timer interval onTick = 
    let mutable startTime  = DateTime.MinValue
    let timer = new Timer(Enabled = false, Interval = interval)
    //let upd _ = 
        //let elepsed = DateTime.Now - startTime
        //let v = int elepsed.TotalMilliseconds
        //textBlock.Text <- sprintf "%s - %s" (startTime.ToString("HH:mm:ss")) (TimeSpan.toString elepsed) 
      //  onTick()
    timer.Tick.Add <| fun _ ->  
        onTick startTime
    let start () = 
        startTime <- DateTime.Now
        timer.Start()        
    let stop () = 
        timer.Stop()    
    start, stop

let buttonsMenu (font:Font) maxWidth xs =
    
    let width =         
        let s =  xs |> List.map fst |> List.maxBy( String.length ) 
        let sz = TextRenderer.MeasureText( s, font, Size( Int32.MaxValue, Int32.MaxValue))
        match maxWidth with 
        | Some maxWidth -> min (sz.Width + 10) maxWidth 
        | _ -> Int32.MaxValue
        
    let p = new Panel(Font = font, Width = width + 10, Height = 3 ) 
    let mutable popup : MyWinForms.Popup = null  
    xs 
    |> List.map( fun (text,f) -> 
        let h = 
            let sz = TextRenderer.MeasureText( text, font, Size( width, Int32.MaxValue), TextFormatFlags.WordBreak)
            sz.Height
        
        let b = new Button(Parent = p, Width = width, Height = h + 18, Left = 5,
                            FlatStyle = FlatStyle.Flat,
                            Text = text, TextAlign = ContentAlignment.MiddleLeft)        
        b.Click.Add <| fun _ ->  f b popup
        b )
    |> List.iter( fun b -> 
        b.Top <- p.Height
        p.Height <- p.Height + b.Height + 3 )
    popup <- new MyWinForms.Popup(p)
    popup



let path  = 
    let rec loop (x:Control) =seq{
        yield x
        if x.Parent<>null then             
            yield! loop x.Parent }
    loop

let index<'a when 'a :> Control > (c:'a) = 
    let t = c.GetType()
    if c.Parent=null then t.Name else
    sprintf "%s%d" t.Name (c.Parent.Controls.IndexOf c) 


let enum =
    let rec loop (x:Control) chooser mapper = seq{
        match chooser x with
        | None -> ()
        | Some y -> 
            yield mapper y
        for x in x.Controls do
            yield! loop x chooser mapper }
    loop 

let mapIndexes c chooser pathMapper mapper  = enum c chooser (fun x -> 
    mapper x ( Seq.map index (path x) |> pathMapper ) )

module GridView = 
    type private G = DataGridView
    type private C = DataGridViewColumn
    type private Cols = DataGridViewColumnCollection

    let addCol<'a when 'a :> C > (cols:Cols) (col:'a)  =
        cols.Add col |> ignore

    let addCols<'a when 'a :> C > (xs : 'a seq) (cols:Cols) =
        xs |> Seq.iter ( cols.Add >> ignore)

    let updateBinding (g:G) = 
        let d = g.DataSource
        g.DataSource <- null
        g.DataSource <- d


let isPropGridEditing (g:PropertyGrid) = 
    let gridView = 
        g.GetType()
            .GetField("gridView", BindingFlags.Instance ||| BindingFlags.NonPublic)
            .GetValue(g)
        :?> Control
    let edit = 
        gridView.GetType()
            .GetField("edit", BindingFlags.Instance ||| BindingFlags.NonPublic)
            .GetValue(gridView)
        :?> Control;
    let dropDownHolder = 
        gridView.GetType()
            .GetField("dropDownHolder", BindingFlags.Instance ||| BindingFlags.NonPublic)
            .GetValue(gridView)
        :?> Control;
    ((edit <> null) && (edit.Visible && edit.Focused)) || 
    ((dropDownHolder <> null) && (dropDownHolder.Visible))


let popupConfig title selectedObject propertySort = 
    let p = new Panel(Width = 400, Height = 600 )
    let g = new PropertyGrid(Parent = p, SelectedObject = selectedObject, Dock = DockStyle.Fill,
                             ToolbarVisible = false,
                             PropertySort = propertySort)
    let l = new Label(Parent = p,  Dock = DockStyle.Top, Text = title, TextAlign = ContentAlignment.MiddleLeft)
    l.SetInfoStyle()
    let popup = new MyWinForms.Popup(p, Resizable = true, Font = Application.OpenForms.[0].Font)
    popup.Closing.Add <| fun e ->
        if isPropGridEditing g then
            e.Cancel <- true
    popup

let radioButtons<'a when 'a : comparison> 
        parent (items : 'a list) 
        (what : 'a -> string ) 
        (descr : 'a -> string ) 
        (handler : 'a -> unit ) = 
    let mutable activeItem = items.Head
    let tooltip = new ToolTip(AutoPopDelay = 5000, InitialDelay = 1000,  ReshowDelay = 500, ShowAlways = true)
    let buttons = items |> List.rev |> List.mapi ( fun n item -> 
        let b = new RadioButton( Parent = parent, Dock = DockStyle.Top,
                                    TextAlign = ContentAlignment.MiddleLeft,
                                    Text = what item, AutoSize=true, Appearance = Appearance.Button)
        tooltip.SetToolTip(b, descr item)
        let bPanel = new Panel(Parent = parent, Dock = DockStyle.Top, Height = 3)
        b.CheckedChanged.Add <| fun _ ->                
            if b.Checked then 
                handler item
            activeItem <- item
        b.FlatStyle <- FlatStyle.Flat
        b,bPanel )
    let b = fst buttons.Head 
    parent.Height <- b.Top + b.Height + 3
    let get () = activeItem

    let btn x =
        let n = items  |> List.findIndex ( (=) x) 
        buttons.[ items.Length - n - 1]
    let (~%%) = btn

    let set x =
        (fst <| %% x).Checked <- true
        activeItem <- x

    let setVisibility visibleItems =
        let visibleItemsSet = Set.ofList visibleItems
        parent.Height <-
            items |> List.choose ( fun x -> 
                let b,p = %% x
                let visible = visibleItemsSet.Contains x
                b.Visible <- visible
                p.Visible <- visible
                if visible then Some  (b.Height + 3) else None)            
            |> List.fold (+) 0
        if visibleItemsSet.Contains  activeItem then () else
            visibleItems 
            |> List.maybeHead
            |> Option.iter set
    get, set, setVisibility


let colorFromString x = ColorTranslator.FromHtml(x)
let MidleAqua = colorFromString "#569CD6"
let MidleGreen = colorFromString "#84BB96"


