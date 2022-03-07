module FableTranspiler.Adapters.WpfClient.Types

type ISettingsManager =
    interface
        abstract Load : key: string -> obj
        abstract Save : key: string -> value: obj -> unit
    end