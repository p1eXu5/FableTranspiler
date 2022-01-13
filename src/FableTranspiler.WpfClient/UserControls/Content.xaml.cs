using FableTranspiler.WpfClient.Adorners;
using System;
using System.Collections.Generic;
using System.Diagnostics;
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
    /// Interaction logic for Content.xaml
    /// </summary>
    public partial class Content : UserControl
    {
        private FsCodeStyleAdorner? _adorner;

        public Content()
        {
            InitializeComponent();
        }


        public FsCodeStyleAdorner FsCodeStyleAdorner
        {
            get {
                if ( _adorner is not null ) {
                    return _adorner;
                }

                var adornerLayer = AdornerLayer.GetAdornerLayer(m_FelizContent);
                _adorner = new FsCodeStyleAdorner(m_FelizContent);
                adornerLayer.Add(_adorner);
                return _adorner;
            }
        }


        #region SectionMouseEnterEvent

        public static readonly RoutedEvent SectionMouseEnterEvent =
            EventManager.RegisterRoutedEvent("SectionMouseEnter", RoutingStrategy.Bubble, typeof(RoutedEventHandler), typeof(Content));

        public static void AddSectionMouseEnterHandler(DependencyObject d, RoutedEventHandler handler)
        {
            UIElement? uie = d as UIElement;
            if (uie != null) {
                uie.AddHandler(SectionMouseEnterEvent, handler);
            }
        }
        public static void RemoveSectionMouseEnterHandler(DependencyObject d, RoutedEventHandler handler)
        {
            UIElement? uie = d as UIElement;
            if (uie != null) {
                uie.RemoveHandler(SectionMouseEnterEvent, handler);
            }
        }

        #endregion ───────────────────────────────────────────────────── SectionMouseEnterEvent ─┘


        #region SectionMouseLeaveEvent

        public static readonly RoutedEvent SectionMouseLeaveEvent =
            EventManager.RegisterRoutedEvent("SectionMouseLeave", RoutingStrategy.Bubble, typeof(RoutedEventHandler), typeof(Content));

        public static void AddSectionMouseLeaveHandler(DependencyObject d, RoutedEventHandler handler)
        {
            UIElement? uie = d as UIElement;
            if (uie != null) {
                uie.AddHandler(SectionMouseLeaveEvent, handler);
            }
        }
        public static void RemoveSectionMouseLeaveHandler(DependencyObject d, RoutedEventHandler handler)
        {
            UIElement? uie = d as UIElement;
            if (uie != null) {
                uie.RemoveHandler(SectionMouseLeaveEvent, handler);
            }
        }

        #endregion ───────────────────────────────────────────────────── SectionMouseLeaveEvent ─┘


        private void RichTextBox_MouseEnter(object sender, RoutedEventArgs e)
        {
            var adorner = FsCodeStyleAdorner;
            adorner.SetPosition((Block)e.Source);

            // see ((System.Windows.Documents.Section)e.Source).ContentStart.GetCharacterRect(LogicalDirection.Backward) - 5, 362.8766..., 0, 15.60333... - 'I' (0 - width, 15 - height)
            e.Handled = true;
            Debug.WriteLine("Mouse enter");
        }

        private void RichTextBox_MouseLeave(object sender, RoutedEventArgs e)
        {
            var adorner = FsCodeStyleAdorner;
            adorner.Hide();
            Debug.WriteLine("Mouse leave");
        }
    }
}
