using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Globalization;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using Elmish.WPF;

namespace FableTranspiler.WpfClient
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : System.Windows.Application
    {
        public App()
        {
            CultureInfo ci = CultureInfo.CreateSpecificCulture(CultureInfo.CurrentCulture.Name);
            ci.DateTimeFormat.ShortDatePattern = "dd.MM.yyyy";
            Thread.CurrentThread.CurrentCulture = ci;

            this.Activated += StartElmish;
        }

        private void StartElmish( object? sender, EventArgs e )
        {
            this.Activated -= StartElmish;
            FableTranspiler.Adapters.WpfClient.App.main(MainWindow);
        }
    }
}
