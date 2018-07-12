using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace Widgets
{
    public partial class AboutForm : Form
    {
        public AboutForm()
        {
            InitializeComponent();
        }

        private void AboutForm_Load(object sender, EventArgs e)
        {
            
        }

        private void linkLabel1_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            try
            {
                System.Diagnostics.Process.Start(linkLabel1.Text);
            } catch
            {
                Clipboard.SetText(linkLabel1.Text);
                MessageBox.Show(
                    "Неудалось открыть ссылку в браузере.\n\n" + 
                    "Текст ссылки скопрированн в буфер обмена.\n\n" + 
                    "Чтобы открыть страницу загрузки последнеей версии ПО, " + 
                    "вставьте содержимое буера обмена в адресную строку браузера.", 
                    "", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }              
           
        }

        private void linkLabel2_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            System.Diagnostics.Process.Start("Руководство оператора.doc");
        }
    }
}
