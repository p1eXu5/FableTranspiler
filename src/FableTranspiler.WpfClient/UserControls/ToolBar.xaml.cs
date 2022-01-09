using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace FableTranspiler.WpfClient.UserControls
{
    /// <summary>
    /// Interaction logic for ToolBar.xaml
    /// </summary>
    public partial class ToolBar : UserControl
    {
        public ToolBar()
        {
            InitializeComponent();
            ShowTsContent = true;
            ShowFelizContent = true;
        }

        public static readonly DependencyProperty ShowTsContentProperty =
            DependencyProperty.Register("ShowTsContent", typeof(bool), typeof(ToolBar), new UIPropertyMetadata(null));

        public bool ShowTsContent
        {
            get { return (bool)GetValue(ShowTsContentProperty); }
            set { SetValue(ShowTsContentProperty, value); }
        }


        public static readonly DependencyProperty ShowFelizContentProperty =
            DependencyProperty.Register("ShowFelizContent", typeof(bool), typeof(ToolBar), new UIPropertyMetadata(null));

        public bool ShowFelizContent
        {
            get { return (bool)GetValue(ShowFelizContentProperty); }
            set { SetValue(ShowFelizContentProperty, value); }
        }

        public static readonly DependencyProperty ShowFableContentProperty =
            DependencyProperty.Register("ShowFableContent", typeof(bool), typeof(ToolBar), new UIPropertyMetadata(null));

        public bool ShowFableContent
        {
            get { return (bool)GetValue(ShowFableContentProperty); }
            set { SetValue(ShowFableContentProperty, value); }
        }
    }
}
