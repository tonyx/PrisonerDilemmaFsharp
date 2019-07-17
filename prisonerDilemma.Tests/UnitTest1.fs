namespace PrisonerDilemmaTests

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

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
        Assert.AreEqual(Cooperate,frameOfGame.Player1Move)
        Assert.AreEqual(Cooperate,frameOfGame.Player2Move)

        

    [<Test>]
    member this.OneTickGameBetweenCooperatorAndDefector() =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let defectorStrategyInfo = {Name="defector";Strategy=defectorStrategy}
        let cooperator = {Name="cooperator";StrategyInfo=cooperatorStrategyInfo}
        let defector = {Name="defector";StrategyInfo=defectorStrategyInfo}
        let game = {Player1=cooperator;Player2=defector; JointmoveHistory=[]}
        let oneTickGame = tick game
        let resultingMoves = oneTickGame.JointmoveHistory 
        Assert.That(List.length resultingMoves = 1)
        let frameOfGame = List.head resultingMoves
        Assert.AreEqual(Cooperate,frameOfGame.Player1Move)
        Assert.AreEqual(Defect,frameOfGame.Player2Move)

    [<Test>]
    member this.TwoTickGameBetweenTitForTatAndDefector() =
        let titForTatStrategyInfo = {Name="titForTat";Strategy=titForTatStrategy}
        let defectorStrategyInfo = {Name="defector";Strategy=defectorStrategy}
        let titForTatRef = {Name="titForTat";StrategyInfo=titForTatStrategyInfo}
        let defectorRef = {Name="defector";StrategyInfo=defectorStrategyInfo}
        let game = {Player1=titForTatRef;Player2=defectorRef; JointmoveHistory=[]}
        let oneTickGame = game |> tick |> tick
        let resultingMoves = oneTickGame.JointmoveHistory 
        Assert.That(List.length resultingMoves = 2)
        let frameOfGame = List.head resultingMoves
        let secondFrameOfGame = List.head (List.tail resultingMoves)
        Assert.AreEqual(Defect,frameOfGame.Player1Move)
        Assert.AreEqual(Defect,frameOfGame.Player2Move)
        Assert.AreEqual(Cooperate,secondFrameOfGame.Player1Move)
        Assert.AreEqual(Defect,secondFrameOfGame.Player2Move)

    [<Test>]
    member this.TwoTickGameBetweenTitForTatAndDefector1() =
        let titForTatStrategyInfoRef = {Name="titForTat";Strategy=titForTatStrategy}
        let defectorStrategyInfoRef = {Name="defector";Strategy=defectorStrategy}
        let titForTatRef = {Name="titForTat";StrategyInfo=titForTatStrategyInfoRef}
        let defectorRef = {Name="defector";StrategyInfo=defectorStrategyInfoRef}
        let game = {Player1=defectorRef;Player2=titForTatRef; JointmoveHistory=[]}
        let oneTickGame = game |> tick |> tick
        let resultingMoves = oneTickGame.JointmoveHistory 
        Assert.That(List.length resultingMoves = 2)
        let frameOfGame = List.head resultingMoves
        let secondFrameOfGame = List.head (List.tail resultingMoves)
        Assert.AreEqual(Defect,frameOfGame.Player1Move)
        Assert.AreEqual(Defect,frameOfGame.Player2Move)
        Assert.AreEqual(Defect,secondFrameOfGame.Player1Move)
        Assert.AreEqual(Cooperate,secondFrameOfGame.Player2Move)

    [<Test>]
    member this.TwoTickGameBetweenDefectorAndTitForTat() =
        let titForTatStrategyInfoRef = {Name="titForTat";Strategy=titForTatStrategy}
        let defectorStrategyInfoRef = {Name="defector";Strategy=defectorStrategy}
        let titForTatRef = {Name="titForTat";StrategyInfo=titForTatStrategyInfoRef}
        let defectorRef = {Name="defector";StrategyInfo=defectorStrategyInfoRef}
        let game = {Player1=titForTatRef;Player2=defectorRef; JointmoveHistory=[]}
        let oneTickGame = game |> tick |> tick
        let resultingMoves = oneTickGame.JointmoveHistory 
        Assert.That(List.length resultingMoves = 2)
        let frameOfGame = List.head resultingMoves
        let secondFrameOfGame = List.head (List.tail resultingMoves)
        Assert.AreEqual(Defect,frameOfGame.Player1Move)
        Assert.AreEqual(Defect,frameOfGame.Player2Move)
        Assert.AreEqual(Cooperate,secondFrameOfGame.Player1Move)
        Assert.AreEqual(Defect,secondFrameOfGame.Player2Move)

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
        let scores = gamesScores playedGames
        let player1outcomes = scoreForPlayer  firstCooperator scores
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

    // started refactoring tests
    [<Test>]
    member this.testTwoDefectorsOneTickGame() =
        let defectorStrategyInfo = {Name="defectorStrategyInfo";Strategy=defectorStrategy}
        let firstDefector = {Name = "firstDefector";StrategyInfo = defectorStrategyInfo}
        let secondDefector = {Name= "secondDefector";StrategyInfo = defectorStrategyInfo}
        let game = {Player1=firstDefector;Player2=secondDefector;JointmoveHistory=[]}
        let playedgame = tick game
        let playedMoves = playedgame.JointmoveHistory
        Assert.AreEqual(1,List.length playedMoves)
        let actualJointmove = List.head playedMoves
        let expectedJointMoves = {Player1Move = Defect;Player2Move=Defect}
        Assert.AreEqual(expectedJointMoves,actualJointmove)


    [<Test>]
    member this.testTwoDefectorstwoTicksGame() =
        let defectorStrategyInfo = {Name="defectorStrategyInfo";Strategy=defectorStrategy}
        let firstDefector = {Name = "firstDefector";StrategyInfo = defectorStrategyInfo}
        let secondDefector = {Name= "secondDefector";StrategyInfo = defectorStrategyInfo}

        let game = {Player1=firstDefector;Player2=secondDefector;JointmoveHistory=[]}
        let playedgame = nTicks game 2
        let playedMoves = playedgame.JointmoveHistory
        Assert.AreEqual(2,List.length playedMoves)
        let latestJointPlayedMove = List.head playedMoves
        let firstJointPlayedMove = List.head (List.tail playedMoves)
        let expectedJointMoves = {Player1Move = Defect;Player2Move=Defect}
        Assert.AreEqual(expectedJointMoves,latestJointPlayedMove)
        Assert.AreEqual(expectedJointMoves,firstJointPlayedMove)

    [<Test>]
    member this.titForTatBetraitsDefectorAtSecondMove() =
        let defectorStrategyInfo =  {Name="defectorStrategyInfo";Strategy=defectorStrategy}
        let titForTatStrategyInfo = {Name="titForTatStrategyInfo";Strategy=titForTatStrategy}
        let defector = {Name= "defector";StrategyInfo=defectorStrategyInfo}
        let titForTat = {Name = "titForTat";StrategyInfo = titForTatStrategyInfo}
        let game = {Player1=titForTat;Player2=defector;JointmoveHistory=[]}
        let playedGame = nTicks game 2
        let playedMoves = playedGame.JointmoveHistory
        Assert.AreEqual(2,List.length playedMoves)
        let latestJointPlayedMoves = List.head playedMoves
        let firstJointPlayedMoves = List.head (List.tail playedMoves)
        let expectedFirstJointmoves = {Player1Move=Cooperate;Player2Move=Defect}
        Assert.AreEqual(expectedFirstJointmoves,firstJointPlayedMoves)
        let expectedLatestJointMoves = {Player1Move=Defect;Player2Move=Defect}
        Assert.AreEqual(expectedLatestJointMoves,latestJointPlayedMoves)

    [<Test>]
    member this.titForTatVsDefectorTwoGamesScores() =
        let defectorStrategyInfo =  {Name="defectorStrategyInfo";Strategy=defectorStrategy}
        let titForTatStrategyInfo = {Name="titForTatStrategyInfo";Strategy=titForTatStrategy}
        let defector = {Name= "defector";StrategyInfo=defectorStrategyInfo}
        let titForTat = {Name= "titForTat";StrategyInfo = titForTatStrategyInfo}
        let game = {Player1=titForTat;Player2=defector;JointmoveHistory=[]}
        let playedGame = nTicks game 2

        let scoresForThisGame = gameScores playedGame
        let expectedScores = {Player1Score=2+3;Player2Score=5+3}
        Assert.AreEqual(expectedScores,scoresForThisGame )

    [<Test>]
    member this.tournmentTestBetweenDefectorAndTifForTAt() =
        let defectorStrategyInfo =  {Name="defectorStrategyInfo";Strategy=defectorStrategy}
        let titForTatStrategyInfo = {Name="titForTatStrategyInfo";Strategy=titForTatStrategy}
        let defector = {Name= "defector";StrategyInfo=defectorStrategyInfo}
        let titForTat = {Name = "titForTat";StrategyInfo = titForTatStrategyInfo}

        let players = [defector;titForTat]
        let games = makeGames players
        Assert.AreEqual(2,List.length games)
        let playedGames = playGamesNTimes games 1 
        let playedGamesScores = gamesScores playedGames
        let firstPlayedGameScores = List.head playedGamesScores
        let secondPlayedGameScores = List.head (List.tail playedGamesScores)
        let expectedFirstPlayedGameScore = (defector,titForTat,{Player1Score=5;Player2Score=2})
        let expectedSecondPlayedGameScore = (titForTat,defector,{Player1Score=2;Player2Score=5})
        Assert.AreEqual(expectedFirstPlayedGameScore,firstPlayedGameScores)
        Assert.AreEqual(expectedSecondPlayedGameScore,secondPlayedGameScores)


    [<Test>]
    member this.scoreOfDefectorVsTitForTatInAOneTickGameTournment() =
        let defectorStrategyInfo =  {Name="defectorStrategyInfo";Strategy=defectorStrategy}
        let titForTatStrategyInfo = {Name="titForTatStrategyInfo";Strategy=titForTatStrategy}
        let defector = {Name= "defector";StrategyInfo=defectorStrategyInfo}
        let titForTat = {Name= "titForTat";StrategyInfo = titForTatStrategyInfo}
        let players = [defector;titForTat]
        let games = makeGames players
        Assert.AreEqual(2,List.length games)
        let playedGames = playGamesNTimes games 1 
        let expectedScoreForTitForTat = 2 + 2
        let scores = gamesScores playedGames
        let titForTatScore = scoreForPlayer  titForTat scores
        let defectorScore = scoreForPlayer  defector scores
        Assert.AreEqual(4,titForTatScore)
        Assert.AreEqual(10,defectorScore)


    [<Test>]
    member this.sameScoreSameMutationProbability() =
        let defectorStrategyInfo =  {Name="defectorStrategyInfo";Strategy=defectorStrategy}
        let defector1 = {Name= "defector1";StrategyInfo=defectorStrategyInfo}
        let defector2 = {Name= "defector2";StrategyInfo=defectorStrategyInfo}
        let scores = [(defector1,6);(defector2,6)]
        let cumulativeProbabilities = mutationProbabilities scores
        let firstInterval = List.head cumulativeProbabilities
        let secondInterval = List.head (List.tail cumulativeProbabilities)
        let expectedFirstInterval = (defector1,0.5)
        let expectedSecondInterval = (defector2,1.0)
        Assert.AreEqual(expectedFirstInterval,firstInterval)
        Assert.AreEqual(expectedSecondInterval,secondInterval)

    [<Test>]
    member this.twoThirdAndOneThirdProbabilityonCumulativeProbinfo() =
        let defectorStrategyInfo =  {Name="defectorStrategyInfo";Strategy=defectorStrategy}
        let defector1 = {Name= "defector1";StrategyInfo=defectorStrategyInfo}
        let defector2 = {Name= "defector2";StrategyInfo=defectorStrategyInfo}
        let scores = [(defector1,6);(defector2,4)]
        let cumulativeProbabilities = mutationProbabilities scores
        let firstInterval = List.head cumulativeProbabilities
        let secondInterval = List.head (List.tail cumulativeProbabilities)
        let expectedFirstInterval = (defector1,(double)6/(double)10)
        let expectedSecondInterval = (defector2,1.0)
        Assert.AreEqual(expectedFirstInterval,firstInterval)
        Assert.AreEqual(expectedSecondInterval,secondInterval)

    [<Test>]
    member this.pickUpPlayerOnCumulativeProbabilityList() =
        let defectorStrategyInfo =  {Name="defectorStrategyInfo";Strategy=defectorStrategy}
        let defector1 = {Name= "defector1";StrategyInfo=defectorStrategyInfo}
        let defector2 = {Name= "defector2";StrategyInfo=defectorStrategyInfo}
        let scores = [(defector1,6);(defector2,4)]
        let pickUpFirstInterval = (double)6/(double)10 - 0.001
        let pickUpSecondInterval = (double)6/(double)10 + 0.001
        let cumulativeProbabilitiesList = mutationProbabilities scores
        let firstIntervalPlayer = pickUpAPlayerBasedOnMutationProbabilities cumulativeProbabilitiesList pickUpFirstInterval
        Assert.AreEqual(defector1,firstIntervalPlayer)
        let secondIntervalPlayer = pickUpAPlayerBasedOnMutationProbabilities cumulativeProbabilitiesList pickUpSecondInterval
        Assert.AreEqual(defector1,secondIntervalPlayer)

    [<Test>]
    member this.DefetorVsCooperatorinA10IterationGame() =
        let defectorStrategyInfo =  {Name="defectorStrategyInfo";Strategy=defectorStrategy}
        let cooperatorStrategyInfo =  {Name="cooperatorStrategyInfo";Strategy=cooperatorStrategy}
        let defector = {Name= "defector";StrategyInfo=defectorStrategyInfo}
        let cooperator = {Name= "cooperator";StrategyInfo=cooperatorStrategyInfo}
        let players = [defector;cooperator]
        let games = makeGames  players 
        let playedGames = playGamesNTimes games 10
        let scores = gamesScores playedGames
        let expectedCooperatorScore = 2*2*10;
        let expectedDefectorScore = 2*5*10;
        let actualCooperatorScore = scoreForPlayer cooperator scores
        let actualDefectorScore = scoreForPlayer defector scores
        Assert.AreEqual(expectedCooperatorScore,actualCooperatorScore)
        Assert.AreEqual(expectedDefectorScore,actualDefectorScore)


    [<Test>]
    member this.defectorVsTitForTat10Iterations() =

        let defectorStrategyInfo =  {Name="defectorStrategyInfo";Strategy=defectorStrategy}
        let titForTatStrategyInfo =  {Name="titForTatStrategyInfo";Strategy=titForTatStrategy}
        let defector = {Name= "defector";StrategyInfo=defectorStrategyInfo}
        let titForTat = {Name= "titForTat";StrategyInfo=titForTatStrategyInfo}
        let players = [defector;titForTat]
        let games = makeGames  players 
        let playedGames = playGamesNTimes games 10
        let scores = gamesScores playedGames

        let expectedTitForTatScore =   2*2 + 2*3*9;
        let expectedDefectorScore =    2*5 + 2*3*9;
        let actualTitForTatScore = scoreForPlayer titForTat scores
        let actualDefectorScore = scoreForPlayer defector scores
        Assert.AreEqual(expectedTitForTatScore,actualTitForTatScore)
        Assert.AreEqual(expectedDefectorScore,actualDefectorScore)


    // [<Test>]
    // member this.evalQuotedExpression() =
    //     let qExp = <@ fun (x: JointMove history) -> Moveh Cooperate @>
    //     Assert.AreEqual(2, evalQ qExp)
        

        










        // let actualJointmove = List.head playedMoves
        // let expectedJointMoves = {Player1Move = Defect;Player2Move=Defect}
        // Assert.AreEqual(expectedJointMoves,actualJointmove)




    // [<Test>]
    // member this.aggregateTimeLabeledGenerations() =
    //     let cooperatorStrategyInfo = {Name="cooperatorStrategy";Strategy=cooperatorStrategy}
    //     let twoCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 2
    //     let labeledGenerations = logNGenerationPlayers twoCooperators 1 1 1.0
    //     let serie = applyStrategyStatToLabeledSeries labeledGenerations
    //     Assert.That (List.length serie = 2)













