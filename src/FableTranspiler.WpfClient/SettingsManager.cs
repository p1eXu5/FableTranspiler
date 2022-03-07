using FableTranspiler.WpfClient.Properties;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FableTranspiler.Adapters.WpfClient.Types;

namespace FableTranspiler.WpfClient
{
    public class SettingsManager : ISettingsManager
    {
        static private SettingsManager? _settingsManager;
        static public ISettingsManager Instance => _settingsManager ??= new SettingsManager(); 

        private SettingsManager()
        {
        }

        public object Load(string key)
        {
            return Settings.Default[key];
        }

        public void Save(string key, object value)
        {
            Settings.Default[key] = value;
            Settings.Default.Save();
            Settings.Default.Reload();
        }
    }
}
