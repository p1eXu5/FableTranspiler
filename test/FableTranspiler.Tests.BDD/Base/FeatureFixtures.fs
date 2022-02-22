namespace FableTranspiler.Tests.BDD.Base

open TickSpec
open NUnit.Framework

open System
open System.Reflection
open System.Runtime.ExceptionServices

[<AbstractClass>]
/// Class containing all BDD tests in current assembly as NUnit unit tests
type FeatureFixture () =

    /// Test method for all BDD tests in current assembly as NUnit unit tests
    abstract member Bdd : Scenario -> unit
    default __.Bdd (scenario:Scenario) =
        if scenario.Tags |> Seq.exists ((=) "ignore") then
            raise (new IgnoreException("Ignored: " + scenario.ToString()))
        try
            TestContext.WriteLine("Steps:")
            TestContext.WriteLine(scenario.Description)
            scenario.Action.Invoke()
        with
        | :? TargetInvocationException as ex -> ExceptionDispatchInfo.Capture(ex.InnerException).Throw()

    /// All test scenarios from feature files in current assembly
    static member GenerateTestCaseData(fuetureFileName: string, [<ParamArray>] steps: string[]) =
        let createFeatureData (feature:Feature) =
            let createTestCaseData (feature:Feature) (scenario:Scenario) =
                let enhanceScenarioName parameters scenarioName =
                    let replaceParameterInScenarioName (scenarioName:string) parameter =
                        scenarioName.Replace("<" + fst parameter + ">", snd parameter)

                    parameters
                    |> Seq.fold replaceParameterInScenarioName scenarioName

                (new TestCaseData(scenario))
                    .SetName(enhanceScenarioName scenario.Parameters scenario.Name)
                    .SetProperty("Feature", feature.Name)
                |> Seq.foldBack (fun (tag:string) data -> data.SetProperty("Tag", tag)) scenario.Tags

            feature.Scenarios
            |> Seq.map (createTestCaseData feature)
        
        let assembly = Assembly.GetExecutingAssembly()
        let definitions = 
            if steps |> Array.isEmpty then
                new StepDefinitions(assembly.GetTypes())
            else
                new StepDefinitions(assembly.GetTypes() |> Array.filter (fun t -> steps |> Array.exists ((=) t.Name)))

        assembly.GetManifestResourceNames()
        |> Seq.filter (fun (n:string) -> n.EndsWith($"%s{fuetureFileName}.feature", System.StringComparison.Ordinal) )
        |> Seq.collect (fun n ->
            definitions.GenerateFeature(n, assembly.GetManifestResourceStream(n))
            |> createFeatureData)