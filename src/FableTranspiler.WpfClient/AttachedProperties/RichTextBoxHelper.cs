using FableTranspiler.WpfClient.Converters;
using FableTranspiler.VmAdapters;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;

namespace FableTranspiler.WpfClient.AttachedProperties
{
    public class RichTextBoxHelper
    {
        public static List<DocumentSegmentViewModel> GetDtsDocument(DependencyObject obj)
        {
            return (List<DocumentSegmentViewModel>)obj.GetValue(DtsDocumentProperty);
        }

        public static void SetDtsDocument(DependencyObject obj, List<DocumentSegmentViewModel> value)
        {
            obj.SetValue(DtsDocumentProperty, value);
        }

        /// <summary>
        /// See <see href="https://stackoverflow.com/questions/343468/richtextbox-wpf-binding"/>
        /// </summary>
        public static readonly DependencyProperty DtsDocumentProperty =
            DependencyProperty.RegisterAttached(
                "DtsDocument",
                typeof(List<DocumentSegmentViewModel>),
                typeof(RichTextBoxHelper),
                new FrameworkPropertyMetadata {
                    BindsTwoWayByDefault = false,
                    PropertyChangedCallback = DtsDocumentChangedCallback
                });

        private static FlowDocument EmptyDocument = new FlowDocument();

        private static void DtsDocumentChangedCallback(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var richTextBox = (RichTextBox)d;

            var statements = GetDtsDocument(richTextBox);

            if (statements is null) {
                // TODO: set error to Document
                return;
            }

            var conv = new StatementListToFlowDocumentConverter();
            
            richTextBox.Document =
                conv.Convert(statements, typeof(FlowDocument), null!, CultureInfo.CurrentUICulture) as FlowDocument;
        }



        public static string GetDocumentError(DependencyObject obj)
        {
            return (string)obj.GetValue(DocumentErrorProperty);
        }

        public static void SetDocumentError(DependencyObject obj, string value)
        {
            obj.SetValue(DocumentErrorProperty, value);
        }

        /// <summary>
        /// See <see href="https://stackoverflow.com/questions/343468/richtextbox-wpf-binding"/>
        /// </summary>
        public static readonly DependencyProperty DocumentErrorProperty =
            DependencyProperty.RegisterAttached(
                "DocumentError",
                typeof(string),
                typeof(RichTextBoxHelper),
                new FrameworkPropertyMetadata {
                    BindsTwoWayByDefault = false,
                    PropertyChangedCallback = DocumentErrorChangedCallback
                });


        private static void DocumentErrorChangedCallback(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var richTextBox = (RichTextBox)d;

            var error = GetDocumentError(richTextBox);

            if (error is null) {
                // TODO: set error to Document
                return;
            }

            var fd = new FlowDocument();
            foreach (var item in error.Split('\n', '\r').Where(s => s != null)) {
                fd.Blocks.Add(new Paragraph(new Run(item)));
            }

            richTextBox.Document =fd;
        }
    }
}
