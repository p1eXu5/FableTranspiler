namespace FableTranspiler.Adapters.WpfClient.Tests

open NUnit.Framework


module ModuleTreeListTests =

    open FableTranspiler.Tests.Common.SimpleTypesFactories
    open Bogus
    open FableTranspiler.SimpleTypes
    open FableTranspiler.Domain.UseCases
    open FableTranspiler.Adapters.WpfClient.Components
    open FsToolkit.ErrorHandling
    open FsUnit
    open System.IO

    module ModuleTree = FableTranspiler.Adapters.WpfClient.Components.ModuleTreeList.ModuleTree

    let faker = Faker "en"

    //let dirPath = faker.System.DirectoryPath()
    //let f1 = faker.System.FileName()
    let rootedFilePath () = 
        Path.GetFullPath(Path.Combine("z:\\node_modules\\@types", faker.System.DirectoryPath()[3..], faker.System.CommonFileName("d.ts")))

    let fullPath () =
        rootedFilePath() |> FullPath.createUnsafe
        

    let emptyFullPathNode () =
        let fullPath = fullPath ()
        (fullPath, []) |> FullPathTree.Node

    let fullPathTree () =
        let fullPath = fullPath ()
        let importing =
            List.init 3 (fun _ -> emptyFullPathNode ())
        (fullPath, importing) |> FullPathTree.Node


    [<Test>]
    let ``update [AppendNewModuleTree]: produces new module tree`` () =
        let tree = fullPathTree ()
        tree |> FullPathTree.allNodes |> should haveLength 4

        let (initModel, _) = ModuleTreeList.init ()
        let (model, _) = ModuleTreeList.update (ModuleTreeList.Msg.AppendNewModuleTree tree) initModel

        model |> should haveLength 1
        let moduleTree = model.Head
        moduleTree.Key |> should haveLength 1
        moduleTree.SubModules |> List.map (fun mt -> mt.Key.Length) |> List.distinct |> should equivalent [2]

        let allModules = moduleTree |> ModuleTree.allModules
        allModules |> should haveLength (tree |> FullPathTree.allNodes |> List.length |> (+) 1 (*lib name on top*))
        allModules |> List.map (fun mt -> mt.IsSelected) |> should not' (contain true)