using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace Widgets
{
    public partial class Icons : UserControl
    {
        public Icons()
        {
            InitializeComponent();
        }
        public static Icons instance = new Icons();
    }
}
