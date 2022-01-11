using System;
using System.Globalization;
using System.Windows;
using System.Windows.Data;

namespace FableTranspiler.WpfClient.Converters
{
    public class ParentWidthConverter : IMultiValueConverter
    {
        public object Convert(object[] values, Type targetType, object parameter, CultureInfo culture)
        {
            if (
                values[0] is double width
                && values[1] is bool showTsContent
                && values[2] is bool showFelizContent
                && values[3] is bool showFableContent
                && values[4] is Thickness margin) 
            { 
                var factor =
                    (showTsContent ? 1 : 0)
                    + (showFelizContent ? 1 : 0)
                    + (showFableContent ? 1 : 0);

                object rv =
                    width / (factor == 0 ? 1 : factor) - (margin.Left + margin.Right);

                return rv;
            }

            return DependencyProperty.UnsetValue;
        }

        public object[] ConvertBack(object value, Type[] targetTypes, object parameter, CultureInfo culture)
        {
            return new object[] { 0, false, false, false, new Thickness() };
        }
    }
}
