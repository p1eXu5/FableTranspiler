﻿using System;
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
                    
                    while (vm[i].Tag != Tag.EndOfLine) 
                    {
                        // TODO: if last keyword is import then not insert line break
                        Style? style = vm[i].Tag switch {
                            Tag.Keyword => Application.Current.FindResource("st_Keyword") as Style,
                            Tag.Type => Application.Current.FindResource("st_Type") as Style,
                            Tag.Comment => Application.Current.FindResource("st_Comment") as Style,
                            _ => null
                        };

                        paragraph.Inlines.Add(new Run(vm[i].Content) { Style = style });

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
