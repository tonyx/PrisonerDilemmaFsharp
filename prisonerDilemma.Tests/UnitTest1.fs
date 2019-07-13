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
    member this.makeGamesTest1()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator]
        let games = makeGames players 
        Assert.That(List.length games = 2)

    [<Test>]
    member this.makeGamesTest2()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let thirdCooperator = {Name="cooperator3";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator;thirdCooperator]
        let games = makeGames players 
        Assert.That(List.length games = 6)
        

    [<Test>]
    member this.playGamesTest3()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let thirdCooperator = {Name="cooperator3";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator;thirdCooperator]
        let games = makeGames players
        let playedGames = playGamesNTimes games 2
        let firstGame = List.head playedGames
        Assert.That(List.length firstGame.JointmoveHistory = 2)

    [<Test>]
    member this.gamesScores()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator]
        let games = makeGames players
        let playedGames = playGamesNTimes games 2
        let outcomes = gamesScores playedGames
        let (_,_,firstGameOutcome) = List.head outcomes
        Assert.That(firstGameOutcome.Player1Score = 8)
    
    [<Test>]
    member this.playersScoreInTournment()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator]
        let games = makeGames players
        let playedGames = playGamesNTimes games 2
        let outcomes = gamesScores playedGames
        let (_,_,firstGameOutcome) = List.head outcomes
        let (_,_,secondGameOutcome) = List.head (List.tail outcomes)
        let player1outcomes = scoreForPlayer playedGames firstCooperator
        Assert.That(firstGameOutcome.Player1Score = 8)
        Assert.That(secondGameOutcome.Player1Score = 8)
        Assert.That(16 = player1outcomes)


    [<Test>]
    member this.makeNCooperators()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let fiveCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 5
        Assert.That(5 = List.length fiveCooperators)


    [<Test>]
    member this.makeGames5Players()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let fiveCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 5
        let games = makeGames fiveCooperators 
        Assert.That(20 = List.length games)

    [<Test>]
    member this.makeTournment2Players()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let twoCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 2
        let games = makeGames twoCooperators
        Assert.That(2 = List.length games)

    [<Test>]
    member this.makeTournment3Players()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let threeCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 3
        let games = makeGames threeCooperators
        Assert.That(6 = List.length games)

    [<Test>]
    member this.playedTournmentReturnsGamePlayedNTimes()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let threeCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 3
        let games = makeGames threeCooperators
        let playedGames = playGamesNTimes games 10
        Assert.That(10 = List.length ((List.head playedGames).JointmoveHistory))

    [<Test>]
    member this.scoresOf10PlaysOfCooperatorsIs80()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let twoCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 2
        let games = makeGames twoCooperators
        let playedGames = playGamesNTimes games 10
        let scores = sortedPlayersScore playedGames
        let (_,firstScore) = List.head scores
        Assert.That(80 = firstScore)

    [<Test>]
    member this.scoresOf10PlaysOfDefectorsIs60()  =
        let defectorStrategyInfo = {Name="defector";Strategy=defectorStrategy}
        let twoDefectors = makeNPlayersByStrategyInfo defectorStrategyInfo 2
        let games = makeGames twoDefectors
        let playedTournament = playGamesNTimes games 10
        let scores = sortedPlayersScore playedTournament
        let (_,firstScore) = List.head scores
        Assert.That(60 = firstScore)


    [<Test>]
    member this.inPlayingAllDefectorsMutationEndsUnInDefectors()  =
        let defectorStrategyInfo = {Name="defector";Strategy=defectorStrategy}
        let twoDefectors = makeNPlayersByStrategyInfo defectorStrategyInfo 2
        let games = makeGames twoDefectors

        let playedTournament = playGamesNTimes games 10
        let scores = sortedPlayersScore playedTournament
        let transProb = mutationProbabilities scores
        let mutatedPlayers = mutateSomePlayerByRandomFactor 0.5 twoDefectors transProb
        let firstPlayer = List.head mutatedPlayers
        let secondPlayer = List.head (List.tail mutatedPlayers)
        Assert.That (firstPlayer.StrategyInfo.Name = "defector")
        Assert.That (secondPlayer.StrategyInfo.Name = "defector")
        
    [<Test>]
    member this.twoGenerationsOfCooperators() =
        let cooperatorStrategyInfo = {Name="cooperatorStrategy";Strategy=cooperatorStrategy}
        let twoCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 2
        let labeledGenerations = logNGenerationPlayers twoCooperators 1 1 1.0
        Assert.That (List.length labeledGenerations = 2)

    // [<Test>]
    // member this.aggregateTimeLabeledGenerations() =
    //     let cooperatorStrategyInfo = {Name="cooperatorStrategy";Strategy=cooperatorStrategy}
    //     let twoCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 2
    //     let labeledGenerations = logNGenerationPlayers twoCooperators 1 1 1.0
    //     let serie = applyStrategyStatToLabeledSeries labeledGenerations
    //     Assert.That (List.length serie = 2)











