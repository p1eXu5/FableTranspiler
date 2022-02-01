using FableTranspiler.WpfClient.Converters;
using FableTranspiler.VmAdapters;
using System.Globalization;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using Microsoft.FSharp.Collections;
using FableTranspiler.VmAdapters.Types;
using FableTranspiler.Components;

namespace FableTranspiler.WpfClient.AttachedProperties
{
    public class DtsFlowDocument
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
                typeof(DtsFlowDocument),
                new FrameworkPropertyMetadata {
                    BindsTwoWayByDefault = false,
                    PropertyChangedCallback = DtsDocumentChangedCallback<DtsStatementViewModel>
                });

        public static FSharpList<DtsStatementViewModel> GetDtsDocument(DependencyObject obj)
        {
            return (FSharpList<DtsStatementViewModel>)obj.GetValue(DtsDocumentProperty);
        }

        public static void SetDtsDocument(DependencyObject obj, FSharpList<DtsStatementViewModel> value)
        {
            obj.SetValue(DtsDocumentProperty, value);
        }

        #endregion ───────────────────────────────────────────────────── DtsDocumentProperty ─┘


        #region DtsDocumentErrorProperty

        /// <summary>
        /// See <see href="https://stackoverflow.com/questions/343468/richtextbox-wpf-binding"/>
        /// </summary>
        public static readonly DependencyProperty DtsDocumentErrorProperty =
            DependencyProperty.RegisterAttached(
                "DtsDocumentError",
                typeof(FSharpList<CodeItem>),
                typeof(DtsFlowDocument),
                new FrameworkPropertyMetadata {
                    BindsTwoWayByDefault = false,
                    PropertyChangedCallback = DtsDocumentChangedCallback<CodeItem>
                });


        public static FSharpList<CodeItem> GetDtsDocumentError(DependencyObject obj)
        {
            return (FSharpList<CodeItem>)obj.GetValue(DtsDocumentErrorProperty);
        }

        public static void SetDtsDocumentError(DependencyObject obj, FSharpList<CodeItem> value)
        {
            obj.SetValue(DtsDocumentErrorProperty, value);
        }

        #endregion ───────────────────────────────────────────────────── DtsDocumentErrorProperty ─┘


        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="TViewModel"></typeparam>
        /// <param name="d"></param>
        /// <param name="e"></param>
        private static void DtsDocumentChangedCallback<TViewModel>(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var richTextBox = (RichTextBox)d;

            object? statements = typeof(TViewModel).Name switch {
                nameof(DtsStatementViewModel) => GetDtsDocument(richTextBox),
                nameof(CodeItem) => GetDtsDocumentError(richTextBox),
                _ => null
            };

            if (statements is null) {
                // TODO: set error to Document
                return;
            }

            var conv = new DtsStatementListToFlowDocumentConverter();

            richTextBox.Document =
                conv.Convert(statements, typeof(FlowDocument), null!, CultureInfo.CurrentUICulture) as FlowDocument;
        }

    }
}
