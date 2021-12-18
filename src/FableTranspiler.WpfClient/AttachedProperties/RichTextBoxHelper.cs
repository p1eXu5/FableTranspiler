using FableTranspiler.WpfClient.Converters;
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

        public static void SetDtsDocument(DependencyObject obj, string value)
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
                    PropertyChangedCallback = PropertyChangedCallback
                });

        private static void PropertyChangedCallback(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var richTextBox = (RichTextBox)d;

            var statements = GetDtsDocument(richTextBox);
            var conv = new StatementListToFlowDocumentConverter();
            
            richTextBox.Document =
                conv.Convert(statements, typeof(FlowDocument), null!, CultureInfo.CurrentUICulture) as FlowDocument;
        }
    }
}
