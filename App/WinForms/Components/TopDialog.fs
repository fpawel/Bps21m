module MyWinForms.TopDialog

open System
open System.Text
open System.Drawing
open System.Windows.Forms

let private (<==) (ownr:Control) chld = 
    ownr.Controls.Add chld



type Placment = 
    | Center
    | RightBottom 
    | RightTop




type opts = 
    {   mutable btmCtrl : Control option 
        mutable topCtrl : Control option 
        mutable width : int
        mutable textForeColor : Color option
        mutable placement : Placment
        mutable btnAcpt : string option 
        mutable btnCncl : string option 
        mutable isOpened : bool
        mutable canUpdate : bool
        mutable text : string
        mutable title : string 
        mutable minTextHeight : int}

type TopmostBar(parent) = 
    let mutable opts = 
        {   btmCtrl = None
            topCtrl = None
            width = 300
            textForeColor = None    
            placement = Center     
            btnAcpt = None
            btnCncl = None
            isOpened = false
            canUpdate = false    
            text = ""
            title = "" 
            minTextHeight = 0}

    let p = new Panel(  Visible = false, Parent = parent,
                        BorderStyle = BorderStyle.FixedSingle, 
                        Font = new Font("Consolas", 12.f ) )
    do
        p.Visible <- false
    let textHeight14 = 
        let sz = TextRenderer.MeasureText("X", p.Font)
        sz.Height
    let textBockTitle = 
        new Label(Parent = p, TextAlign = ContentAlignment.MiddleLeft,
                  ForeColor = Color.White, BackColor = Color.Navy)
    let textPanel = new Panel(Parent = p, Location = Point(5,5), Width = p.Width, AutoScroll = true)
    let textBlock = new Label(Parent = textPanel)
    let buttonAccept = new Button( Parent = p, Height = textHeight14 + 10, FlatStyle = FlatStyle.Flat )    
    let buttonCancel = new Button( Parent = p, Height = textHeight14 + 10, FlatStyle = FlatStyle.Flat )

    let isVisible() =  p.PerformThreadSafeAction <| fun () -> p.Visible

    let validateButtons() = 
        match opts.btnAcpt, opts.btnCncl with
        | Some buttonAcceptText, None ->
            let sz = TextRenderer.MeasureText(buttonAcceptText, p.Font)
            buttonAccept.Text <- buttonAcceptText 
            buttonAccept.Width <- sz.Width + 10
            buttonAccept.Left <- opts.width / 2 - sz.Width / 2             
        | Some buttonAcceptText, Some buttonCancelText ->
            let sz =
                [buttonAcceptText; buttonCancelText]
                |> List.map( fun s -> TextRenderer.MeasureText(s, p.Font))
                |> List.maxBy( fun x -> opts.width )
            let w = sz.Width
            let h = sz.Height

            buttonAccept.Text <- buttonAcceptText 
            buttonAccept.Width <- w
            buttonAccept.Left <- opts.width / 4 - w / 2
            
            buttonCancel.Text <- buttonCancelText 
            buttonCancel.Width <- w
            buttonCancel.Left <- 3 * opts.width / 4 - w / 2            
        | _ -> ()

    let validateLocation() = 
        p.Location <- 
            match opts.placement with
            | Center -> 
                new Point ( p.Parent.ClientSize.Width / 2 - p.Width / 2, 
                            p.Parent.ClientSize.Height / 2 - p.Height / 2 )
            | RightBottom ->
                new Point ( p.Parent.ClientSize.Width - p.Size.Width - p.Margin.Right, 
                            p.Parent.ClientSize.Height - p.Size.Height - p.Margin.Bottom )
            | RightTop ->
                new Point ( p.Parent.ClientSize.Width - p.Size.Width - p.Margin.Right, 0 )

    let validateContent () = 
        p.Width <- opts.width

        textBockTitle.Width <- opts.width
        textBockTitle.Text <- opts.title
        
        textBockTitle.Height <-
            let sz = TextRenderer.MeasureText(opts.title, p.Font, Size(opts.width-3, Int32.MaxValue), TextFormatFlags.WordBreak ) 
            sz.Height + 3

        let mutable y = textBockTitle.Bottom + 5

        match opts.topCtrl with
        | None -> ()
        | Some topCtrl ->
            topCtrl.Parent <- p            
            topCtrl.Top <- y
            y <- topCtrl.Bottom + 5
            topCtrl.Width <- opts.width


        textBlock.Text <- opts.text
        textBlock.Height <- 
            let textHeight = 
                let sz = TextRenderer.MeasureText(opts.text, p.Font, Size(textBlock.Width, Int32.MaxValue), TextFormatFlags.WordBreak)
                max sz.Height 5
            max opts.minTextHeight textHeight

        if textBlock.Height > parent.ClientSize.Height - 100 then
            textPanel.Height <- parent.ClientSize.Height - 150
            textBlock.Width <- opts.width - 70
            textPanel.Width <- opts.width - 10
        else
            textPanel.Height <- textBlock.Height
            textBlock.Width <- opts.width 
            textPanel.Width <- opts.width 


        textPanel.Top <- y
        textBlock.ForeColor <- 
            match opts.textForeColor with 
            | Some textForeColor -> textForeColor
            | _ -> textPanel.ForeColor
        y <- textPanel.Bottom + 5
        
        match opts.btmCtrl with
        | None -> ()
        | Some btmCtrl ->
            btmCtrl.Parent <- p
            btmCtrl.Top <- y
            y <- btmCtrl.Bottom + 5
            btmCtrl.Width <- opts.width

        buttonAccept.Visible <- opts.btnAcpt.IsSome
        buttonCancel.Visible <- opts.btnCncl.IsSome
        
        if opts.btnAcpt.IsSome then            
            buttonAccept.Top <- y + 5 
            y <- buttonAccept.Bottom + 5
        if opts.btnCncl.IsSome then            
            buttonCancel.Top <- buttonAccept.Top
        validateButtons()

        p.Height <- y
        p.Visible <- true

        p.BringToFront()
        p.Parent.PerformLayout()
        validateLocation()

    let parSizeChangedHandler = new EventHandler(fun _ _ -> 
        if p.Visible then
            validateContent() )

    do
        p.VisibleChanged.Add <| fun _ ->
            opts.isOpened <- p.Visible
            opts.canUpdate <- p.Visible

        buttonAccept.Click.Add <| fun _ -> 
            p.Visible <- false
            opts.isOpened <- false

        buttonCancel.Click.Add <| fun _ -> 
            opts.isOpened <- false
            p.Visible <- false

        parent.SizeChanged.AddHandler parSizeChangedHandler
        parent.Resize.AddHandler parSizeChangedHandler

    let update() = 
        if opts.canUpdate then
            p.PerformThreadSafeAction validateContent
    
    member x.Placement
        with get() = opts.placement
        and set v = 
            if v<>opts.placement then
                opts.placement <- v
                update()

    member x.MinTextHeight
        with get() = opts.minTextHeight
        and set v = 
            if v<>opts.minTextHeight then
                opts.minTextHeight <- v
                update()

    member x.TextForeColor
        with get() = opts.textForeColor
        and set v = 
            if v<>opts.textForeColor then
                opts.textForeColor <- v
                update()

    member x.TopControl
        with get() = opts.topCtrl
        and set v = 
            if v<>opts.topCtrl then
                match v, opts.topCtrl with
                | None, Some ctrl -> ctrl.Parent <- null
                | _ -> ()
                opts.topCtrl <- v
                update()

    member x.BottomControl
        with get() = opts.btmCtrl
        and set v = 
            if v<>opts.btmCtrl then
                match v, opts.btmCtrl with
                | None, Some ctrl -> ctrl.Parent <- null
                | _ -> ()
                opts.btmCtrl <- v
                update()

    member x.Text
        with get() = opts.text
        and set v = 
            if v<>opts.text then
                opts.text <- v
                update()

    member x.Title
        with get() = opts.title
        and set v = 
            if v<>opts.title then
                opts.title <- v
                update()

    member x.Width
        with get() = opts.width
        and set v = 
            if v<>opts.width then
                opts.width <- v
                update()

    member x.Visible 
        with get() = opts.isOpened
        and set v = 
            if v<>opts.isOpened then
                opts.isOpened <- v
                p.PerformThreadSafeAction <| fun () ->
                    p.Visible <- v
                    if p.Visible then
                        validateContent()

    member x.ButtonAccept 
        with get() =  opts.btnAcpt 
        and set v = 
            if v<> opts.btnAcpt then
                opts.btnAcpt <- v
                update()

    member x.ButtonCancel 
        with get() =  opts.btnCncl 
        and set v = 
            if v<> opts.btnCncl then
                opts.btnCncl <- v                
                update()
    
    member x.Font 
        with get() =  p.Font
        and set v = 
            p.PerformThreadSafeAction <| fun () ->
                p.Font <- v
                if opts.isOpened then
                    validateContent()

    member x.DoUpdate f = 
        if opts.isOpened then
            opts.canUpdate <- false
            p.PerformThreadSafeAction <| fun () ->
                f()
                validateContent()
            opts.canUpdate <- true
        else
            f()
            x.Visible <- true