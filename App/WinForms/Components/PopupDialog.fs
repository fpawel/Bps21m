module MyWinForms.PopupDialog

open System
open System.Text
open System.Drawing
open System.Windows.Forms

let private (<==) (ownr:Control) chld = 
    
    ownr.Controls.Add chld

type Options = 
    {   Width : int 
        Font : Font
        Text : string option
        Title : string 
        ButtonAcceptText : string 
        Content : Control }
    static member def() = 
        {   Width = 300
            Font = new Font("Consolas", 12.f)
            Text = None
            Title = ""
            ButtonAcceptText = "Применить"
            
            Content = null }

type Dialog = 
    {   Validate : unit -> unit
        Show : Control -> unit }
        

let create<'a> i (getValue : unit -> 'a option) (accept : 'a -> unit) = 
    let p = new Panel( Width = i.Width, Font = i.Font, BorderStyle = BorderStyle.FixedSingle )       
    let textBlockTitle = new Label(Parent = p, TextAlign = ContentAlignment.MiddleLeft,
                                  Text = i.Title, Width = i.Width,
                                  ForeColor = Color.White, BackColor = Color.Navy)
    let mutable y = textBlockTitle.Bottom + 5

    match i.Text with
    | None -> ()
    | Some text ->
        let sz = TextRenderer.MeasureText(text, i.Font, Size(i.Width - 5, Int32.MaxValue), TextFormatFlags.WordBreak)
        let x =  new Label ( Parent = p, Text = text, Width = i.Width - 5, Top = y, Left = 5, Height = sz.Height )
        y <- x.Bottom + 5

    let content =         
        if i.Content<>null then
            i.Content.Parent <- p
            i.Content.Left <- 10
            i.Content.Width <- i.Width - 20
            i.Content.Top <- y
            y <- i.Content.Bottom + 3

    let buttonAccept = new Button( Parent = p, Left = 10, Width = i.Width - 20, 
                                   Top = y, Height = 40, Text=i.ButtonAcceptText, 
                                   FlatStyle = FlatStyle.Flat,
                                   Enabled = getValue().IsSome )
    p.Height <- buttonAccept.Bottom + 5
    let popup = new Popup(p) 
    buttonAccept.Click.Add <| fun _ ->
        popup.Hide()
        match getValue() with
        | None -> ()
        | Some x -> accept x
        
                
    popup, ( fun () -> buttonAccept.Enabled <- getValue().IsSome )



     