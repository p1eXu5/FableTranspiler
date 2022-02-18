using FableTranspiler.WpfClient.Converters;
using FableTranspiler.Interpreters;
using System.Globalization;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using Microsoft.FSharp.Collections;
using FableTranspiler.Adapters.WpfClient.Components;

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
                typeof(FSharpList< DtsInterpreter.DtsStatement >),
                typeof(DtsFlowDocument),
                new FrameworkPropertyMetadata {
                    BindsTwoWayByDefault = false,
                    PropertyChangedCallback = DtsDocumentChangedCallback< DtsInterpreter.DtsStatement >
                });

        public static FSharpList< DtsInterpreter.DtsStatement > GetDtsDocument(DependencyObject obj)
        {
            return (FSharpList< DtsInterpreter.DtsStatement >)obj.GetValue(DtsDocumentProperty);
        }

        public static void SetDtsDocument(DependencyObject obj, FSharpList< DtsInterpreter.DtsStatement > value)
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
                nameof(DtsInterpreter.DtsStatement) => GetDtsDocument(richTextBox),
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
