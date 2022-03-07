using System;
using System.Windows.Data;

namespace FableTranspiler.WpfClient.Converters
{
    public class MultiBooleanConverter : IMultiValueConverter
    {
        public object Convert( object[] values, Type targetType, object parameter, System.Globalization.CultureInfo culture)
        {
            bool result = true;
            
            foreach (object value in values) 
            {
                if (value is bool bValue && ! bValue) {
                    result = false;
                    break;
                }
            }

            return result;
        }

        public object[] ConvertBack( object value, Type[] targetTypes, object parameter, System.Globalization.CultureInfo culture )
        {
            throw new NotImplementedException();
        }
    }
}
