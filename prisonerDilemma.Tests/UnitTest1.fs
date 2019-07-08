namespace PrisonerDilemmaTests

open NUnit.Framework

open prisonerDilemma.Program


[<TestClass>]
type TestClass () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.Test1 () =
        Assert.Pass()
    
    [<Test>]
    member this.OneTickGameBetweenCooperators() =
        let cooperatorStrategy = {Name="cooperator";Strategy=cooperatorPlay}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategy}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategy}
        let game = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        let oneTickGame = tick game
        let resultingMoves = oneTickGame.JointmoveHistory 
        Assert.That(List.length resultingMoves = 1)
        let frameOfGame = List.head resultingMoves
        Assert.That(frameOfGame.Player1Move = Cooperate)
        Assert.That(frameOfGame.Player2Move = Cooperate)
        
//    [<Test>]
//    member this.OutcomesTestCooperator() =
//        let cooperatorStrategy = {Name="cooperator";Strategy=cooperatorPlay}
//        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategy}
//        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategy}
//        let game = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
//        let oneTickGame = tick game
//        let outcomes = cumulateOutcomes oneTickGame.JointmoveHistory
//        Assert.That(outcomes.Player1Outcome = 4)


    [<Test>]
    member this.OutcomesOfATwoTickGameBetweenCooperators() =
        let cooperatorStrategy = {Name="cooperator";Strategy=cooperatorPlay}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategy}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategy}
        let game = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        let twoTicksGame =  game |> tick |> tick
        Assert.That((List.length twoTicksGame.JointmoveHistory)=2)
        let outcomes = gameOutcomes twoTicksGame 
        Assert.That(outcomes.Player1Outcome = 8)

    [<Test>]
    member this.NTicksGameTest() =
        let cooperatorStrategy = {Name="cooperator";Strategy=cooperatorPlay}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategy}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategy}
        let game = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        let twoTicksGame =  nTicks game 2
        Assert.That((List.length twoTicksGame.JointmoveHistory)=2)
        let outcomes = gameOutcomes twoTicksGame
        Assert.That(outcomes.Player1Outcome = 8)
        Assert.That(true)

    [<Test>]
    member this.GamesOutcome() =
        let cooperatorStrategy = {Name="cooperator";Strategy=cooperatorPlay}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategy}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategy}
        let game1 = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        let game2 = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        let twoTicksGame1 =  nTicks game1 2
        let twoTicksGame2 =  nTicks game2 2
        let games = [twoTicksGame1;twoTicksGame2]
        let playedGame = gamesOutcomes games
        Assert.That(List.length playedGame = 2)
        let (_,_,outcome) = List.head playedGame
        Assert.That(outcome.Player1Outcome = 8)



        


