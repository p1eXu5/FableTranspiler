using FableTranspiler.VmAdapters;
using FableTranspiler.WpfClient.Converters;
using Microsoft.FSharp.Collections;
using System.Globalization;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;

namespace FableTranspiler.WpfClient.AttachedProperties
{
    public class FsFlowDocument
    {
        private static FlowDocument EmptyDocument = new FlowDocument();


        #region FsDocumentProperty

        /// <summary>
        /// See <see href="https://stackoverflow.com/questions/343468/richtextbox-wpf-binding"/>
        /// </summary>
        public static readonly DependencyProperty FsDocumentProperty =
            DependencyProperty.RegisterAttached(
                "FsDocument",
                typeof(FSharpList<FsStatementViewModel>),
                typeof(FsFlowDocument),
                new FrameworkPropertyMetadata {
                    BindsTwoWayByDefault = false,
                    PropertyChangedCallback = FsDocumentChangedCallback<FsStatementViewModel>
                });


        public static FSharpList<FsStatementViewModel> GetFsDocument(DependencyObject obj)
        {
            return (FSharpList<FsStatementViewModel>)obj.GetValue(FsDocumentProperty);
        }

        public static void SetFsDocument(DependencyObject obj, FSharpList<FsStatementViewModel> value)
        {
            obj.SetValue(FsDocumentProperty, value);
        }

        #endregion ───────────────────────────────────────────────────── FsDocumentProperty ─┘


        #region FsDocumentErrorProperty

        /// <summary>
        /// See <see href="https://stackoverflow.com/questions/343468/richtextbox-wpf-binding"/>
        /// </summary>
        public static readonly DependencyProperty FsDocumentErrorProperty =
            DependencyProperty.RegisterAttached(
                "FsDocumentError",
                typeof(FSharpList<CodeItemViewModel>),
                typeof(FsFlowDocument),
                new FrameworkPropertyMetadata {
                    BindsTwoWayByDefault = false,
                    PropertyChangedCallback = FsDocumentChangedCallback<CodeItemViewModel>
                });


        public static FSharpList<CodeItemViewModel> GetFsDocumentError(DependencyObject obj)
        {
            return (FSharpList<CodeItemViewModel>)obj.GetValue(FsDocumentErrorProperty);
        }

        public static void SetFsDocumentError(DependencyObject obj, FSharpList<CodeItemViewModel> value)
        {
            obj.SetValue(FsDocumentErrorProperty, value);
        }

        #endregion ───────────────────────────────────────────────────── FsDocumentErrorProperty ─┘


        #region SectionMouseEnterEvent

        public static readonly RoutedEvent SectionMouseEnterEvent =
            EventManager.RegisterRoutedEvent("SectionMouseEnter", RoutingStrategy.Bubble, typeof(RoutedEventHandler), typeof(FsFlowDocument));

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
            EventManager.RegisterRoutedEvent("SectionMouseLeave", RoutingStrategy.Bubble, typeof(RoutedEventHandler), typeof(FsFlowDocument));

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


        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="TViewModel"></typeparam>
        /// <param name="d"></param>
        /// <param name="e"></param>
        private static void FsDocumentChangedCallback<TViewModel>(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var richTextBox = (RichTextBox)d;

            object? statements = typeof(TViewModel).Name switch {
                nameof(FsStatementViewModel) => GetFsDocument(richTextBox),
                nameof(CodeItemViewModel) => GetFsDocumentError(richTextBox),
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
