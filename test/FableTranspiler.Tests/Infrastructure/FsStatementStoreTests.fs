namespace FableTranspiler.Tests.Infrastructure

open FableTranspiler
open FableTranspiler.SimpleTypes
open FableTranspiler.Interpreters.FsInterpreter
open NUnit.Framework
open FableTranspiler.Tests.Common
open FableTranspiler.Tests.Common.SimpleTypesFactories
open FableTranspiler.Tests.Factories
open FsUnit

module FsStatementStoreTests =

    let private store = 
        (InterpretConfigFactory.build (MockLoggerFactory.GetMockedLoggerFactory()) (fun _ -> None) FsCodeStyle.Fable)
            .Store
            
    let typedFsStatmenet name =
        let identifier = Identifier "foo"
        let statement =
            FsStatement.Typed (identifier, [], [])
        (identifier, statement)


    [<Test>]
    let ``Store can returns statement by its identifier when it's added to the store`` () =
        let (identifier, statement) = typedFsStatmenet "foo"
        let modulePath = @"z:\bar\baz.d.ts" |> ModulePath.createUnsafe

        do
            store.Add modulePath statement

        store.Get modulePath identifier |> should be (ofCase <@ Some statement @>)


    [<Test>]
    let ``Statement can be obtained through reader by identifier when reader is initialized with statment module path`` () =
        let (identifier, statement) = typedFsStatmenet "foo"
        let modulePath = ModulePath @"z:\bar\baz.d.ts"

        do
            store.Add modulePath statement

        let reader = store.InitReader modulePath

        reader.Get [identifier] |> should be (ofCase <@ Some statement @>)


    [<Test>]
    let ``Statement can be obtained through reader by qualifiers when statment module is imported`` () =
        let (identifier, statement) = typedFsStatmenet "foo"
        let modulePath = ModulePath @"z:\bar\baz.d.ts"

        do
            store.Add modulePath statement

        let moduleAlias = Identifier "Baz"
        let reader = 
            store.InitReader (ModulePath "some-else")
            |> store.ImportAll moduleAlias modulePath

        reader.Get [moduleAlias; identifier] |> should be (ofCase <@ Some statement @>)


    [<Test>]
    let ``Statement can not be obtained through reader by qualifiers when statment module is not imported`` () =
        let (identifier, statement) = typedFsStatmenet "foo"
        let modulePath = ModulePath @"z:\bar\baz.d.ts"

        do
            store.Add modulePath statement

        let moduleAlias = Identifier "Baz"
        let reader = 
            store.InitReader (ModulePath "some-else")

        reader.Get [moduleAlias; identifier] |> should be (ofCase <@ None @>)