namespace FableTranspiler.Tests.Domain.UseCases

open NUnit.Framework

[<Category("UseCases.Test")>]
module ImplementationTests =
    
    open FsUnit
    open FsToolkit.ErrorHandling
    open FableTranspiler.Tests.Common
    open FableTranspiler.Tests.Common.FsUnit


    [<Test>]
    let ``WHEN parsing dts module with declared module and namespace THEN producing tree with two levels`` () =
        result {
            let statement =
                """
                declare module "material-ui" {
                    export import AppBar = __MaterialUI.AppBar;
                    export import AppBarProps = __MaterialUI.AppBarProps;
                }

                declare namespace __MaterialUI {
                    // ReactLink is from "react/addons"

                    export interface AppBarProps {
                            className?: string | undefined;
                            iconClassNameLeft?: string | undefined;
                            iconClassNameRight?: string | undefined;
                            iconElementLeft?: React.ReactElement | undefined;
                            iconElementRight?: React.ReactElement | undefined;
                            iconStyleRight?: React.CSSProperties | undefined;
                            iconStyleLeft?: React.CSSProperties | undefined;
                            onLeftIconButtonClick?: React.MouseEventHandler<{}> | undefined;
                            onRightIconButtonClick?: React.MouseEventHandler<{}> | undefined;
                            onTitleClick?: React.MouseEventHandler<{}> | undefined;
                            showMenuIconButton?: boolean | undefined;
                            style?: React.CSSProperties | undefined;
                            title?: React.ReactNode | undefined;
                            titleStyle?: React.CSSProperties | undefined;
                            zDepth?: number | undefined;
                        }
                        export class AppBar extends React.Component<AppBarProps> {
                        }
                }
                """
            ()
        }
        |> Result.runTest