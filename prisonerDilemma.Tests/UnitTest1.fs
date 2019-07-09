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
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let game = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        let oneTickGame = tick game
        let resultingMoves = oneTickGame.JointmoveHistory 
        Assert.That(List.length resultingMoves = 1)
        let frameOfGame = List.head resultingMoves
        Assert.That(frameOfGame.Player1Move = Cooperate)
        Assert.That(frameOfGame.Player2Move = Cooperate)
        

    [<Test>]
    member this.OutcomesOfATwoTickGameBetweenCooperators() =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let game = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        let twoTicksGame =  game |> tick |> tick
        Assert.That((List.length twoTicksGame.JointmoveHistory)=2)
        let outcomes = gameScores twoTicksGame 
        Assert.That(outcomes.Player1Score = 8)

    [<Test>]
    member this.NTicksGameTest() =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let game = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        let twoTicksGame =  nTicks game 2
        Assert.That((List.length twoTicksGame.JointmoveHistory)=2)
        let outcomes = gameScores twoTicksGame
        Assert.That(outcomes.Player1Score = 8)
        Assert.That(true)

    [<Test>]
    member this.GamesOutcome() =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let game1 = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        let game2 = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        let twoTicksGame1 =  nTicks game1 2
        let twoTicksGame2 =  nTicks game2 2
        let games = [twoTicksGame1;twoTicksGame2]
        let playedGame = gamesScores games
        Assert.That(List.length playedGame = 2)
        let (_,_,outcome) = List.head playedGame
        Assert.That(outcome.Player1Score=8)

    
    [<Test>]
    member this.makeTournmentTest1()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator]
        let tournment = makeTournment players 1
        Assert.That(List.length (tournment.Games) = 2)

    [<Test>]
    member this.makeTournmentTest2()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let thirdCooperator = {Name="cooperator3";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator;thirdCooperator]
        let tournment = makeTournment players 1
        Assert.That(List.length (tournment.Games) = 6)
        

    [<Test>]
    member this.playTournmentTest()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let thirdCooperator = {Name="cooperator3";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator;thirdCooperator]
        let tournment = makeTournment players 2
        let playedTournment = playTournment tournment
        let firstGame = List.head playedTournment
        Assert.That(List.length firstGame.JointmoveHistory = 2)

    [<Test>]
    member this.tournmentScores()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator]
        let tournment = makeTournment players 2
        let playedTournment = playTournment tournment
        let outcomes = gamesScores playedTournment
        let (_,_,firstGameOutcome) = List.head outcomes
        Assert.That(firstGameOutcome.Player1Score = 8)
    
    [<Test>]
    member this.playersScoreInTournment()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator]
        let tournment = makeTournment players 2
        let playedTournment = playTournment tournment
        let outcomes = gamesScores playedTournment
        let (player1,_,firstGameOutcome) = List.head outcomes
        let (player2,_,secondGameOutcome) = List.head (List.tail outcomes)
        let player1outcomes = scoreForPlayer playedTournment "cooperator1"
        Assert.That(firstGameOutcome.Player1Score = 8)
        Assert.That(secondGameOutcome.Player1Score = 8)
        Assert.That(16 = player1outcomes)

