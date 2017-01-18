namespace Widgets
{
    partial class Icons
    {
        /// <summary> 
        /// Обязательная переменная конструктора.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Освободить все используемые ресурсы.
        /// </summary>
        /// <param name="disposing">истинно, если управляемый ресурс должен быть удален; иначе ложно.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Код, автоматически созданный конструктором компонентов

        /// <summary> 
        /// Требуемый метод для поддержки конструктора — не изменяйте 
        /// содержимое этого метода с помощью редактора кода.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Icons));
            this.imageList1 = new System.Windows.Forms.ImageList(this.components);
            this.SuspendLayout();
            // 
            // imageList1
            // 
            this.imageList1.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList1.ImageStream")));
            this.imageList1.TransparentColor = System.Drawing.Color.Transparent;
            this.imageList1.Images.SetKeyName(0, "skip");
            this.imageList1.Images.SetKeyName(1, "cancel");
            this.imageList1.Images.SetKeyName(2, "down");
            this.imageList1.Images.SetKeyName(3, "print");
            this.imageList1.Images.SetKeyName(4, "close");
            this.imageList1.Images.SetKeyName(5, "settings");
            this.imageList1.Images.SetKeyName(6, "tools1");
            this.imageList1.Images.SetKeyName(7, "data1");
            this.imageList1.Images.SetKeyName(8, "additem");
            this.imageList1.Images.SetKeyName(9, "removeitem");
            this.imageList1.Images.SetKeyName(10, "doc");
            this.imageList1.Images.SetKeyName(11, "folder");
            this.imageList1.Images.SetKeyName(12, "three_lines");
            this.imageList1.Images.SetKeyName(13, "upload");
            this.imageList1.Images.SetKeyName(14, "installing_updates");
            this.imageList1.Images.SetKeyName(15, "serial");
            this.imageList1.Images.SetKeyName(16, "pressure");
            this.imageList1.Images.SetKeyName(17, "save");
            this.imageList1.Images.SetKeyName(18, "open");
            this.imageList1.Images.SetKeyName(19, "clear");
            this.imageList1.Images.SetKeyName(20, "add");
            this.imageList1.Images.SetKeyName(21, "tools");
            this.imageList1.Images.SetKeyName(22, "about");
            this.imageList1.Images.SetKeyName(23, "todo");
            this.imageList1.Images.SetKeyName(24, "termochamber");
            this.imageList1.Images.SetKeyName(25, "pneumo");
            this.imageList1.Images.SetKeyName(26, "loop");
            this.imageList1.Images.SetKeyName(27, "run");
            this.imageList1.Images.SetKeyName(28, "script");
            this.imageList1.Images.SetKeyName(29, "network");
            this.imageList1.Images.SetKeyName(30, "zoom-out");
            this.imageList1.Images.SetKeyName(31, "clean");
            this.imageList1.Images.SetKeyName(32, "list");
            this.imageList1.Images.SetKeyName(33, "testconn");
            // 
            // Icons
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Name = "Icons";
            this.ResumeLayout(false);

        }

        #endregion

        public System.Windows.Forms.ImageList imageList1;
    }
}
