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
using Microsoft.FSharp.Collections;

namespace FableTranspiler.WpfClient.AttachedProperties
{
    public class RichTextBoxHelper
    {
        private static FlowDocument EmptyDocument = new FlowDocument();


        #region DtsDocumentProperty

        /// <summary>
        /// See <see href="https://stackoverflow.com/questions/343468/richtextbox-wpf-binding"/>
        /// </summary>
        public static readonly DependencyProperty DtsDocumentProperty =
            DependencyProperty.RegisterAttached(
                "DtsDocument",
                typeof(FSharpList<DtsStatementViewModel>),
                typeof(RichTextBoxHelper),
                new FrameworkPropertyMetadata {
                    BindsTwoWayByDefault = false,
                    PropertyChangedCallback = DtsDocumentChangedCallback
                });


        private static void DtsDocumentChangedCallback(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var richTextBox = (RichTextBox)d;

            FSharpList<DtsStatementViewModel>? statements = GetDtsDocument(richTextBox);

            if (statements is null) {
                // TODO: set error to Document
                return;
            }

            var conv = new DtsStatementListToFlowDocumentConverter();
            
            richTextBox.Document =
                conv.Convert(statements, typeof(FlowDocument), null!, CultureInfo.CurrentUICulture) as FlowDocument;
        }


        public static FSharpList<DtsStatementViewModel> GetDtsDocument(DependencyObject obj)
        {
            return (FSharpList<DtsStatementViewModel>)obj.GetValue(DtsDocumentProperty);
        }

        public static void SetDtsDocument(DependencyObject obj, FSharpList<DtsStatementViewModel> value)
        {
            obj.SetValue(DtsDocumentProperty, value);
        }

        #endregion ───────────────────────────────────────────────────── DtsDocumentProperty ─┘


















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
