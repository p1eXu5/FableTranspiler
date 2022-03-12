        namespace ReactScroll.Modules.Components.Button
        
        open Fable.Core
        
        // outer lib is not processed yet - import * as React from 'react';
        
        /// see also React.HTMLProps<HTMLButtonElement>
        type ButtonProps =
            | To of string
            | ContainerId of string
            | ActiveClass of string
            | Spy of bool
            | HashSpy of bool
            | Smooth of U2<bool, string>
            | Offset of float
            | Delay of float
            | IsDynamic of bool
            | OnClick of unit -> unit
            | Duration of U3<float, string, (float -> float)>
            | Absolute of bool
            | OnSetActive of string -> unit
            | OnSetInactive of unit -> unit
            | IgnoreCancelEvents of bool
            | SaveHashHistory of bool
            interface IHTMLProp
        
        module Button =
        
            open Fable.React
            open Fable.Core.JsInterop
        
            let inline Button props children =
                domEl (importDefault @"react-scroll\modules\components\Button") props children
        
