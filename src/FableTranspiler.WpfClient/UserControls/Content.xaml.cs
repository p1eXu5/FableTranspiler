using FableTranspiler.WpfClient.Adorners;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
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

                var adornerLayer = AdornerLayer.GetAdornerLayer(m_FableContent);
                _adorner = new FsCodeStyleAdorner(m_FableContent);
                adornerLayer.Add(_adorner);
                return _adorner;
            }
        }


        


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

        private void _MouseEnter(object sender, MouseEventArgs e)
        {
            var adorner = FsCodeStyleAdorner;
            adorner.Hide();
            e.Handled= true;
            Debug.WriteLine("Mouse enter {0}", sender);
        }

        private async void SaveDocument(object sender, RoutedEventArgs e)
        {
            var flowDocument = m_FableContent.Document;
            var fileName = System.IO.Path.Combine((string)Tag, m_FsModuleFileName.Text);

            if (File.Exists(fileName)) {
                var view = new OverwriteFileDialog();
                var result = await MaterialDesignThemes.Wpf.DialogHost.Show(view);

                if ( !(result is not null && result is bool b && b ) ) return;
            }

            using (FileStream fs = new FileStream(fileName, FileMode.OpenOrCreate, FileAccess.Write))
            {
                TextRange textRange = new TextRange(flowDocument.ContentStart, flowDocument.ContentEnd);
                textRange.Save(fs, DataFormats.Text);
            }
        }
    }
}
