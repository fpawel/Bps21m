namespace MyWinForms

open System
open System.Text
open System.Drawing
open System.Windows.Forms

type GrowLabel() = 
    inherit Label(AutoSize = false) 
    let mutable isGrowing = false

    
    member this.doResize() =
        if isGrowing then () else
        try 
            isGrowing <- true
            let sz = TextRenderer.MeasureText(this.Text, this.Font, Size(this.Width, Int32.MaxValue), TextFormatFlags.WordBreak)
            this.Height <- sz.Height        
        finally 
            isGrowing <- false

    override this.OnTextChanged(e) =
        base.OnTextChanged(e)
        this.doResize()

    override this.OnFontChanged(e) =
        base.OnFontChanged(e)
        this.doResize()

    override this.OnSizeChanged(e) =
        base.OnSizeChanged(e)
        this.doResize()