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

namespace FableTranspiler.WpfClient
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            ShowTsContent = true;
            ShowFableContent = true;
            ShowFelizContent = false;
        }

        private void CommandBinding_OnExecuted( object sender, ExecutedRoutedEventArgs e )
        {
            if (sender is RichTextBox rtb) {
                rtb.Paste();
            }
            RaiseEvent( new RoutedEventArgs(PastEvent) );
            
            e.Handled = true;
        }

        public static readonly RoutedEvent PastEvent = EventManager.RegisterRoutedEvent(
            "Past", RoutingStrategy.Direct, typeof(RoutedEventHandler), typeof(MainWindow));

        // Provide CLR accessors for the event
        public event RoutedEventHandler Past
        {
            add => AddHandler(PastEvent, value);
            remove => RemoveHandler(PastEvent, value);
        }

        public static readonly DependencyProperty ShowTsContentProperty =
            DependencyProperty.Register("ShowTsContent", typeof(bool), typeof(MainWindow), new UIPropertyMetadata(null));

        public bool ShowTsContent
        {
            get { return (bool)GetValue(ShowTsContentProperty); }
            set { SetValue(ShowTsContentProperty, value); }
        }


        public static readonly DependencyProperty ShowFelizContentProperty =
            DependencyProperty.Register("ShowFelizContent", typeof(bool), typeof(MainWindow), new UIPropertyMetadata(null));

        public bool ShowFelizContent
        {
            get { return (bool)GetValue(ShowFelizContentProperty); }
            set { SetValue(ShowFelizContentProperty, value); }
        }

        public static readonly DependencyProperty ShowFableContentProperty =
            DependencyProperty.Register("ShowFableContent", typeof(bool), typeof(MainWindow), new UIPropertyMetadata(null));

        public bool ShowFableContent
        {
            get { return (bool)GetValue(ShowFableContentProperty); }
            set { SetValue(ShowFableContentProperty, value); }
        }

    }
}
