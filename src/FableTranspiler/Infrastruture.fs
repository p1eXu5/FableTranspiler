module Infrastruture

open System;
open System.IO;
open System.Windows;
open Microsoft.Win32;


let openFile () =
    let fd = OpenFileDialog()
    match fd.ShowDialog() |> Option.ofNullable with
    | Some _ -> fd.FileName
    | None -> ""


