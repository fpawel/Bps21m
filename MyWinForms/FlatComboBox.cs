using System;
using System.Windows.Forms;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Runtime.InteropServices;

namespace MyWinForms
{
	[ToolboxBitmap(typeof(System.Windows.Forms.ComboBox))]
	public class FlatComboBox: ComboBox
	{	
		#region ComboInfoHelper
		internal class ComboInfoHelper
		{
			[DllImport("user32")] 
			private static extern bool GetComboBoxInfo(IntPtr hwndCombo, ref ComboBoxInfo info);

			#region RECT struct
			[StructLayout(LayoutKind.Sequential)]
				private struct RECT 
			{
				public int Left;
				public int Top;
				public int Right;
				public int Bottom;
			}
			#endregion

			#region ComboBoxInfo Struct
			[StructLayout(LayoutKind.Sequential)]
				private struct ComboBoxInfo 
			{
				public int cbSize;
				public RECT rcItem;
				public RECT rcButton;
				public IntPtr stateButton;
				public IntPtr hwndCombo;
				public IntPtr hwndEdit;
				public IntPtr hwndList;
			}
			#endregion

			public static int GetComboDropDownWidth()
			{
				ComboBox cb = new ComboBox();
				int width = GetComboDropDownWidth(cb.Handle);
				cb.Dispose();
				return width;
			}
			public static int GetComboDropDownWidth(IntPtr handle)
			{
				ComboBoxInfo cbi = new ComboBoxInfo();
				cbi.cbSize = Marshal.SizeOf(cbi);
				GetComboBoxInfo(handle, ref cbi);
				int width = cbi.rcButton.Right - cbi.rcButton.Left;
				return width;
			}
		}
		#endregion

		public const int WM_ERASEBKGND = 0x14;
		public const int WM_PAINT = 0xF;
		public const int WM_NC_PAINT = 0x85;
		public const int WM_PRINTCLIENT = 0x318;
		private static int DropDownButtonWidth = 17;

		[DllImport("user32.dll", EntryPoint="SendMessageA")]
		public static extern int SendMessage (IntPtr hwnd, int wMsg, IntPtr wParam, object lParam);

		[DllImport("user32")]
		public static extern IntPtr GetWindowDC (IntPtr hWnd );

		[DllImport("user32")]
		public static extern int ReleaseDC(IntPtr hWnd, IntPtr hDC );

		static FlatComboBox()
		{
			DropDownButtonWidth = ComboInfoHelper.GetComboDropDownWidth() + 2;
		}

        public FlatComboBox()
			: base()
		{
			this.SetStyle(ControlStyles.DoubleBuffer, true);
		}

		protected override void OnSelectedValueChanged(EventArgs e)
		{
			base.OnSelectedValueChanged (e);
			this.Invalidate();
		}

		protected override void WndProc(ref Message m)
		{
			if (this.DropDownStyle == ComboBoxStyle.Simple)
			{
				base.WndProc(ref m);
				return;
			}

			IntPtr hDC = IntPtr.Zero;
			Graphics gdc = null;
			switch (m.Msg)
			{
				case WM_NC_PAINT:	
					hDC = GetWindowDC(this.Handle);
					gdc = Graphics.FromHdc(hDC);
					SendMessage(this.Handle, WM_ERASEBKGND, hDC, 0);
					SendPrintClientMsg();	// send to draw client area
					PaintFlatControlBorder(this, gdc);
					m.Result = (IntPtr) 1;	// indicate msg has been processed			
					ReleaseDC(m.HWnd, hDC);
					gdc.Dispose();	

					break;
				case WM_PAINT:	
					base.WndProc(ref m);
					// flatten the border area again
					hDC = GetWindowDC(this.Handle);
					gdc = Graphics.FromHdc(hDC);
					Pen p = new Pen((this.Enabled? BackColor:SystemColors.Control), 2);	
					gdc.DrawRectangle(p, new Rectangle(2, 2, this.Width-3, this.Height-3));
					PaintFlatDropDown(this, gdc);
					PaintFlatControlBorder(this, gdc);
					ReleaseDC(m.HWnd, hDC);
					gdc.Dispose();	

					break;
				default:
					base.WndProc(ref m);
					break;
			}
		}
		private void SendPrintClientMsg()
		{
			// We send this message for the control to redraw the client area
			Graphics gClient = this.CreateGraphics();
			IntPtr ptrClientDC = gClient.GetHdc();
			SendMessage(this.Handle, WM_PRINTCLIENT, ptrClientDC, 0);
			gClient.ReleaseHdc(ptrClientDC);
			gClient.Dispose();
		}

		private void PaintFlatControlBorder(Control ctrl, Graphics g)
		{
			Rectangle rect = new Rectangle(0, 0, ctrl.Width, ctrl.Height);
			if (ctrl.Focused == false || ctrl.Enabled == false )
				ControlPaint.DrawBorder(g, rect, SystemColors.ControlDark, ButtonBorderStyle.Solid);
			else
				ControlPaint.DrawBorder(g, rect, Color.Black, ButtonBorderStyle.Solid);
		}
		public static void PaintFlatDropDown(Control ctrl, Graphics g)
		{
			Rectangle rect = new Rectangle(ctrl.Width-DropDownButtonWidth, 0, DropDownButtonWidth, ctrl.Height);
			ControlPaint.DrawComboButton(g, rect, ButtonState.Flat);
		}

		protected override void OnLostFocus(System.EventArgs e)
		{
			base.OnLostFocus(e);
			this.Invalidate();
		}

		protected override void OnGotFocus(System.EventArgs e)
		{
			base.OnGotFocus(e);
			this.Invalidate();
		}		
		protected override void OnResize(EventArgs e)
		{
			base.OnResize (e);
			this.Invalidate();
		}

	}
}
