using System;
using System.Globalization;
using System.Windows;
using System.Windows.Data;
using System.Windows.Documents;
using FableTranspiler.Adapters.WpfClient.Components;
using FableTranspiler.Interpreters;
using FableTranspiler.Interpreters.FsInterpreter;
using FableTranspiler.WpfClient.AttachedProperties;
using Microsoft.FSharp.Collections;

namespace FableTranspiler.WpfClient.Converters
{
    public class DtsStatementListToFlowDocumentConverter : IValueConverter
    {
        private static readonly string _offset = "        ";

        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if (value is not null) {
                return Convert((dynamic)value);
            }

            return DependencyProperty.UnsetValue;
        }


        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }

        private object Convert(object value)
        {
            return DependencyProperty.UnsetValue;
        }


        internal object Convert(FSharpList<DtsInterpreter.DtsStatement> vmList)
        {
            var fd = new FlowDocument();

            foreach (var vm in vmList) 
            {
                var section = BuildSection(vm.Presentation);
                fd.Blocks.Add(section);
            }

            return fd;
        }


        internal object Convert(FSharpList<TopLevelFsStatement> vmList)
        {
            var fd = new FlowDocument();

            foreach (TopLevelFsStatement vm in vmList) {
                Section section = BuildSection(TopLevelFsStatement.CollectCodeItems(vm), true);
                //if (vm.FsCodeStyle() != FsCodeStyle.Universal) {
                //    section.MouseEnter += SectionEnter;
                //}
                //else {
                //    section.MouseEnter += SectionLeave;
                //}

                fd.Blocks.Add(section);
            }

            return fd;
        }


        internal object Convert(FSharpList<CodeItem> vmList)
        {
            var fd = new FlowDocument();
            Section section = BuildSection(vmList);
            fd.Blocks.Add(section);

            return fd;
        }


        private Section BuildSection(FSharpList<CodeItem> vmList, bool extraLineBreake = false)
        {
            var section = new Section();
            var paragraph = new Paragraph() { Margin = new Thickness(0) };
            paragraph.Inlines.Add(new Run(_offset));

            foreach (var item in vmList) {
                if (item.Tag == Tag.EndOfLine) 
                {
                    if (item.Content is not null) {
                        //if (!paragraph.Inlines.Any()) {
                        //    paragraph.Inlines.Add(_offset);
                        //}
                        paragraph.Inlines.Add(new Run(item.Content)); // end of line
                    }
                    section.Blocks.Add(paragraph);
                    paragraph = new Paragraph() { Margin = new Thickness(0) };
                    paragraph.Inlines.Add(new Run(_offset));
                    continue;
                }

                Style? style = item.Tag switch {
                    Tag.Modifier => Application.Current.FindResource("st_Modifier") as Style,
                    Tag.Keyword => Application.Current.FindResource("st_Keyword") as Style,
                    Tag.Type => Application.Current.FindResource("st_Type") as Style,
                    Tag.Comment => Application.Current.FindResource("st_Comment") as Style,
                    Tag.Parentheses => Application.Current.FindResource("st_Parentheses") as Style,
                    _ => null
                };

                paragraph.Inlines.Add(new Run(item.Content) { Style = style });
            }

            if (extraLineBreake) {
                section.Blocks.Add(paragraph);
            }

            return section;
        }


        private void SectionEnter(object sender, System.Windows.Input.MouseEventArgs e)
        {
            var section = sender as Section;
            if (section != null) {
                var routedEventArgs = new RoutedEventArgs(FsFlowDocument.SectionMouseEnterEvent);
                section.RaiseEvent(routedEventArgs);
            }
        }

        private void SectionLeave(object sender, System.Windows.Input.MouseEventArgs e)
        {
            var section = sender as Section;
            if (section != null) {
                var routedEventArgs = new RoutedEventArgs(FsFlowDocument.SectionMouseLeaveEvent);
                section.RaiseEvent(routedEventArgs);
            }
        }
    }
}
