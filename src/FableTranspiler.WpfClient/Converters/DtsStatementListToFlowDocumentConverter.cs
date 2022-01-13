using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Data;
using System.Windows.Documents;
using FableTranspiler.VmAdapters;
using FableTranspiler.WpfClient.Adorners;
using Microsoft.FSharp.Collections;

namespace FableTranspiler.WpfClient.Converters
{
    public class DtsStatementListToFlowDocumentConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if (value is not null && value is FSharpList<DtsStatementViewModel> vmList) 
            {
                var fd = new FlowDocument();

                foreach (var vm in vmList) 
                {
                    var section = new Section();
                    var paragraph = new Paragraph() {  Margin = new Thickness(0) };

                    foreach (var item in vm.DtsDocumentSection) {
                        if (item.Tag == Tag.EndOfLine) {
                            if (item.Content is not null) {
                                paragraph.Inlines.Add(new Run(item.Content)); // end of line
                            }
                            section.Blocks.Add(paragraph);
                            paragraph = new Paragraph() { Margin = new Thickness(0) };
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

                    fd.Blocks.Add(section);
                }

                return fd;
            }

            return DependencyProperty.UnsetValue;
        }



        private void Section_MouseEnter(object sender, System.Windows.Input.MouseEventArgs e)
        {
            var section = sender as Section;
            if (section != null) {
                var routedEventArgs = new RoutedEventArgs(FableTranspiler.WpfClient.UserControls.Content.SectionMouseEnterEvent);
                section.RaiseEvent(routedEventArgs);
            }
        }

        private void Section_MouseLeave(object sender, System.Windows.Input.MouseEventArgs e)
        {
            var section = sender as Section;
            if (section != null) {
                var routedEventArgs = new RoutedEventArgs(FableTranspiler.WpfClient.UserControls.Content.SectionMouseLeaveEvent);
                section.RaiseEvent(routedEventArgs);
            }
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }
}
