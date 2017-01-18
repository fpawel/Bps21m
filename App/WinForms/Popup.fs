namespace MyWinForms

open System
open System.Text
open System.Drawing
open System.Windows.Forms

open System.Runtime.InteropServices


open WindowStyles

[<AutoOpen>]
module private Helpers = 
    [<DllImport("user32.dll", CharSet=CharSet.Auto, ExactSpelling = true)>]
    extern IntPtr SetActiveWindow(HandleRef hWnd)
    [<Literal>] 
    let WM_ACTIVATE = 0x0006
    [<Literal>] 
    let WM_MOUSEACTIVATE = 0x0021
    let MA_NOACTIVATE = 3








type Popup(ownerControl : Control) = 
    inherit Control()
    let WM_ACTIVATE = 0x0006
    let WM_MOUSEACTIVATE = 0x0021
    do
        base.SetTopLevel(true)

    override x.CreateParams =
        let createParams = base.CreateParams

        createParams.Style <-   WS_POPUP |||
                                WS_VISIBLE |||
                                WS_CLIPSIBLINGS |||
                                WS_CLIPCHILDREN |||
                                WS_MAXIMIZEBOX |||
                                WS_BORDER;
        createParams.ExStyle <- WS_EX_LEFT |||
                                WS_EX_LTRREADING |||
                                WS_EX_RIGHTSCROLLBAR ||| 
                                WS_EX_TOPMOST

        createParams.Parent <- if ownerControl <> null then ownerControl.Handle else IntPtr.Zero
        createParams

    override x.OnPaint (e) =   
        
        e.Graphics.FillRectangle(SystemBrushes.Info, 0, 0, x.Width, x.Height)
        base.OnPaint(e)

    override x.WndProc(m) =
        match m.Msg with 
        | WM_ACTIVATE when int m.WParam = 1 ->
            if ownerControl <> null then
                SetActiveWindow(new HandleRef(x, ownerControl.FindForm().Handle))
                |> ignore
        | WM_MOUSEACTIVATE ->
            m.Result <- new IntPtr(MA_NOACTIVATE)
        | _ -> ()
        base.WndProc(ref m)
(*
class PopupWindow : Control
{
    private const int WM_ACTIVATE = 0x0006;
    private const int WM_MOUSEACTIVATE = 0x0021;

    private Control ownerControl;

    public PopupWindow(Control ownerControl)
        :base()
    {
        this.ownerControl = ownerControl;
        base.SetTopLevel(true);
    }

    public Control OwnerControl
    {
        get
        {
            return (this.ownerControl as Control);
        }
        set
        {
            this.ownerControl = value;
        }
    }

    protected override CreateParams CreateParams
    {
        get
        {
            CreateParams createParams = base.CreateParams;

            createParams.Style = WindowStyles.WS_POPUP |
                                 WindowStyles.WS_VISIBLE |
                                 WindowStyles.WS_CLIPSIBLINGS |
                                 WindowStyles.WS_CLIPCHILDREN |
                                 WindowStyles.WS_MAXIMIZEBOX |
                                 WindowStyles.WS_BORDER;
            createParams.ExStyle = WindowsExtendedStyles.WS_EX_LEFT |
                                   WindowsExtendedStyles.WS_EX_LTRREADING |
                                   WindowsExtendedStyles.WS_EX_RIGHTSCROLLBAR | 
                                   WindowsExtendedStyles.WS_EX_TOPMOST;

            createParams.Parent = (this.ownerControl != null) ? this.ownerControl.Handle : IntPtr.Zero;
            return createParams;
        }
    }

    [DllImport("user32.dll", CharSet = CharSet.Auto, ExactSpelling = true)]
    public static extern IntPtr SetActiveWindow(HandleRef hWnd);

    protected override void WndProc(ref Message m)
    {
        switch (m.Msg)
        {
            case WM_ACTIVATE:
                {
                    if ((int)m.WParam == 1)
                    {
                        //window is being activated
                        if (ownerControl != null)
                        {
                            SetActiveWindow(new HandleRef(this, ownerControl.FindForm().Handle));
                        }
                    }
                    break;
                }
            case WM_MOUSEACTIVATE:
                {
                    m.Result = new IntPtr(MouseActivate.MA_NOACTIVATE);
                    return;
                    //break;
                }
        }
        base.WndProc(ref m);
    }

    protected override void OnPaint(PaintEventArgs e)
    {
        base.OnPaint(e);
        e.Graphics.FillRectangle(SystemBrushes.Info, 0, 0, Width, Height);
        e.Graphics.DrawString((ownerControl as VerticalDateScrollBar).FirstVisibleDate.ToLongDateString(), this.Font, SystemBrushes.InfoText, 2, 2);
    }
}

*)