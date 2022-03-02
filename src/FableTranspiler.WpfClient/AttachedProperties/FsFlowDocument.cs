using FableTranspiler.Adapters.WpfClient.Components;
using FableTranspiler.Interpreters;
using FableTranspiler.WpfClient.Converters;
using Microsoft.FSharp.Collections;
using System.Globalization;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using FableTranspiler.Interpreters.FsInterpreter;

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
                typeof(FSharpList< FsStatementV2 >),
                typeof(FsFlowDocument),
                new FrameworkPropertyMetadata {
                    BindsTwoWayByDefault = false,
                    PropertyChangedCallback = FsDocumentChangedCallback<FsStatementV2>
                });


        /// <summary>
        /// Obtains <see cref="FsDocumentProperty"/> value.
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public static FSharpList<FsStatementV2> GetFsDocument(DependencyObject obj)
        {
            return (FSharpList<FsStatementV2>)obj.GetValue(FsDocumentProperty);
        }

        public static void SetFsDocument(DependencyObject obj, FSharpList<FsStatementV2> value)
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
                typeof(FSharpList<CodeItem>),
                typeof(FsFlowDocument),
                new FrameworkPropertyMetadata {
                    BindsTwoWayByDefault = false,
                    PropertyChangedCallback = FsDocumentChangedCallback<CodeItem>
                });


        public static FSharpList<CodeItem> GetFsDocumentError(DependencyObject obj)
        {
            return (FSharpList<CodeItem>)obj.GetValue(FsDocumentErrorProperty);
        }

        public static void SetFsDocumentError(DependencyObject obj, FSharpList<CodeItem> value)
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
                nameof(FsStatementV2) => GetFsDocument(richTextBox),
                nameof(CodeItem) => GetFsDocumentError(richTextBox),
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
