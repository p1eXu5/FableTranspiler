namespace rec FableTranspiler.Tests.BDD

open NUnit.Framework
open FsUnit
open TickSpec
open FableTranspiler.Tests.BDD.Base


[<TestFixture(TestName="Feature: DTS to Fable interpretation")>]
type FableInterpretationFeatureFixture() = 
    inherit FeatureFixture()

    [<TestCaseSource("Scenarios")>]
    override _.Bdd(scenario: Scenario) = base.Bdd(scenario)
    static member Scenarios = 
        FeatureFixture.GenerateTestCaseData(
            "FableInterpretation",
            nameof ParseFileSteps,
            nameof FableInterpretationSteps
        )


module FableInterpretationSteps =

    let dummy = ()
