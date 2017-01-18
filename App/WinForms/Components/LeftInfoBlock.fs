namespace MyWinForms.Components

open System
open System.Drawing
open System.Windows.Forms

type LeftInfoBlock(par, caption) = 
    let p = new Panel(Parent = par, Dock = DockStyle.Left, Width = 1, Visible = false)
    let b = new Label(Parent = p, Dock = DockStyle.Fill)

    let _ = new Panel(Parent = p, Dock = DockStyle.Left, Width = 3)
    let p_caption = new Label(Parent = p, Dock = DockStyle.Left, Text = caption)    
    let _ = new Panel(Parent = p, Dock = DockStyle.Left, Width = 3)

    let tooltip = new ToolTip(AutoPopDelay = 5000, InitialDelay = 1000,  ReshowDelay = 500, ShowAlways = true)
    let setTooltip text = 
        tooltip.SetToolTip(b, text)
        tooltip.SetToolTip(p_caption, text)
    
    do
        let sz = TextRenderer.MeasureText(caption, p.Font ) 
        p_caption.Width <- sz.Width + 3
    
    member __.setText level (text,tooltipText) =
        b.Text <- text
        let sz = TextRenderer.MeasureText(text, p.Font )             
        p.Width <- p_caption.Width + 6 + sz.Width
        b.ForeColor <- if level = Logging.Error then Color.Red else Color.Navy
        match tooltipText with 
        | None -> tooltip.RemoveAll()
        | Some tooltipText -> setTooltip tooltipText
        if not p.Visible then
            p.Visible <- true

    member __.hide() =
        p.Visible <- false

    member x.setTextSafe level text =
        par.PerformThreadSafeAction <| fun () ->
            x.setText level text