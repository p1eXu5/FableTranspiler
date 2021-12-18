using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Data;
using System.Windows.Documents;
using FableTranspiler.Parsers;

namespace FableTranspiler.WpfClient.Converters
{
    public class StatementListToFlowDocumentConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if (value is not null && value is List<DocumentSegmentViewModel> vm) {
                var fd = new FlowDocument();
                var i = 0;
                while (vm[i].Tag != Tag.EndOfDocument) 
                {
                    var paragraph = new Paragraph() {  Margin = new Thickness(0) };
                    while (vm[i].Tag != Tag.EndOfLine) {
                        switch (vm[i].Tag) {
                            case Tag.Keyword:
                                paragraph.Inlines.Add(
                                    new Run(vm[i].Content) {
                                        Style = Application.Current.FindResource("st_Keyword") as Style
                                    }
                                );
                                break;

                            case Tag.Text:
                                paragraph.Inlines.Add(new Run(vm[i].Content));
                                break;
                        }
                        ++i;
                    }

                    paragraph.Inlines.Add(new Run(vm[i].Content));
                    fd.Blocks.Add(paragraph);

                    ++i;
                }

                return fd;
            }

            return DependencyProperty.UnsetValue;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }
}
