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
    member this.``in a one tick game between two cooperators both cooperates as first move``()=
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let game = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        // when
        let oneTickGame = tick game
        let resultingMoves = oneTickGame.JointmoveHistory 
        // then
        Assert.That(List.length resultingMoves = 1)
        let frameOfGame = List.head resultingMoves
        Assert.AreEqual(Cooperate,frameOfGame.Player1Move)
        Assert.AreEqual(Cooperate,frameOfGame.Player2Move)


    [<Test>]
    member this.``in a one tick game the cooperator cooperates and the defector defects`` ()= 
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let defectorStrategyInfo = {Name="defector";Strategy=defectorStrategy}
        let cooperator = {Name="cooperator";StrategyInfo=cooperatorStrategyInfo}
        let defector = {Name="defector";StrategyInfo=defectorStrategyInfo}
        let game = {Player1=cooperator;Player2=defector; JointmoveHistory=[]}
        // when
        let oneTickGame = tick game
        let resultingMoves = oneTickGame.JointmoveHistory 
        // then
        Assert.That(List.length resultingMoves = 1)
        let frameOfGame = List.head resultingMoves
        Assert.AreEqual(Cooperate,frameOfGame.Player1Move)
        Assert.AreEqual(Defect,frameOfGame.Player2Move)

    [<Test>]
    member this.``in a two ticks game the tit for tat cooperates and then copies the opponent`` () =
        // given
        let titForTatStrategyInfo = {Name="titForTat";Strategy=titForTatStrategy}
        let defectorStrategyInfo = {Name="defector";Strategy=defectorStrategy}
        let titForTatRef = {Name="titForTat";StrategyInfo=titForTatStrategyInfo}
        let defectorRef = {Name="defector";StrategyInfo=defectorStrategyInfo}
        let game = {Player1=titForTatRef;Player2=defectorRef; JointmoveHistory=[]}
        // when
        let twoTicksGame = game |> tick |> tick
        let resultingMoves = twoTicksGame.JointmoveHistory 
        Assert.That(List.length resultingMoves = 2)
        // then
        let lastJointMoves = List.head resultingMoves
        let firstJointMoves = List.head (List.tail resultingMoves)
        Assert.AreEqual(Defect,lastJointMoves.Player1Move)
        Assert.AreEqual(Defect,lastJointMoves.Player2Move)
        Assert.AreEqual(Cooperate,firstJointMoves.Player1Move)
        Assert.AreEqual(Defect,firstJointMoves.Player2Move)



    [<Test>]
    member this.``a cooperator in a two round match with another cooperator earn 8 points`` ()=
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let game = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        // when
        let twoTicksGame =  game |> tick |> tick
        // then
        Assert.That((List.length twoTicksGame.JointmoveHistory)=2)
        let outcomes = gameScores twoTicksGame 
        Assert.That(outcomes.Player1Score = 8)
        Assert.That(outcomes.Player2Score = 8)

    [<Test>]
    member this.``two cooperators match using nTicks function`` ()=
        // given 
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let game = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        // when
        let twoTicksGame =  nTicks game 2
        // then
        Assert.That((List.length twoTicksGame.JointmoveHistory)=2)
        let outcomes = gameScores twoTicksGame
        Assert.That(outcomes.Player1Score = 8)
        Assert.That(true)

    [<Test>]
    member this.``scores of a list of two games``() =
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let game1 = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        let game2 = {Player1=firstCooperator;Player2=secondCooperator; JointmoveHistory=[]}
        // when
        let twoTicksGame1 =  nTicks game1 2
        let twoTicksGame2 =  nTicks game2 2
        let games = [twoTicksGame1;twoTicksGame2]
        let plaidGame = gamesScores games
        // then
        Assert.That(List.length plaidGame = 2)
        let (_,_,outcome) = List.head plaidGame
        Assert.That(outcome.Player1Score=8)
        let (_,_,outcome2) = List.head (List.tail plaidGame)
        Assert.That(outcome2.Player1Score=8)


    
    [<Test>]
    member this.``generating games between two player end up in a list of two games``()  =
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator]
        // when
        let games = makeGames players 
        // then
        let game1 = {Player1=firstCooperator;Player2=secondCooperator;JointmoveHistory=[]}
        let game2 = {Player2=firstCooperator;Player1=secondCooperator;JointmoveHistory=[]}
        Assert.That(List.length games = 2)
        Assert.AreEqual(game1,List.head games )
        Assert.AreEqual(game2,List.head (List.tail games) )

    [<Test>]
    member this.``generating games between three players gets six games``() = 
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let thirdCooperator = {Name="cooperator3";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator;thirdCooperator]
        // when 
        let games = makeGames players 
        // then

        Assert.AreEqual(6,List.length games)
        
    [<Test>]
    member this.``games between three players ``() = 
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let thirdCooperator = {Name="cooperator3";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator;thirdCooperator]
        // when 
        let games = makeGames players 
        // then
        let game1 = {Player1=firstCooperator;Player2=secondCooperator;JointmoveHistory=[]}
        let game2 = {Player2=firstCooperator;Player1=secondCooperator;JointmoveHistory=[]}
        let setOfGames = Set.ofList games

        Assert.AreEqual(6,List.length games)
        Assert.IsTrue(Set.contains game1 setOfGames)
        Assert.IsTrue(Set.contains game2 setOfGames)
 
    [<Test>]
    member this.``able to play a list of games n times``()  =
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let thirdCooperator = {Name="cooperator3";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator;thirdCooperator]
        let games = makeGames players
        // when
        let playedGames = playGamesNTimes games 2
        let firstGame = List.head playedGames
        // then
        Assert.That(List.length firstGame.JointmoveHistory = 2)

    [<Test>]
    member this.``playing games between cooperators two times gets score of 8 in the first game for the first player``()  =
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator]
        let games = makeGames players
        let playedGames = playGamesNTimes games 2
        let outcomes = gamesScores playedGames
        // when
        let (_,_,firstGameOutcome) = List.head outcomes
        // then
        Assert.AreEqual(8,firstGameOutcome.Player1Score)
    
    [<Test>]
    member this.``two cooperators in a two games tournment gets 8 points each one``()  =
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let firstCooperator = {Name="cooperator1";StrategyInfo=cooperatorStrategyInfo}
        let secondCooperator = {Name="cooperator2";StrategyInfo=cooperatorStrategyInfo}
        let players = [firstCooperator;secondCooperator]
        let games = makeGames players
        // when
        let playedGames = playGamesNTimes games 2
        let outcomes = gamesScores playedGames
        let (_,_,firstGameOutcome) = List.head outcomes
        let (_,_,secondGameOutcome) = List.head (List.tail outcomes)
        let scores = gamesScores playedGames
        let player1outcomes = scoreForPlayer  firstCooperator scores
        // then
        Assert.That(firstGameOutcome.Player1Score = 8)
        Assert.That(secondGameOutcome.Player1Score = 8)
        Assert.That(16 = player1outcomes)


    [<Test>]
    member this.makeNCooperators()  =
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let fiveCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 5
        Assert.That(5 = List.length fiveCooperators)


    [<Test>]
    member this.``a tournment of five players is 20 games``()  =
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let fiveCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 5
        // when
        let games = makeGames fiveCooperators 
        // then
        let expectedNumOfGames = (4 + 3 + 2 + 1) * 2 // = 20
        Assert.AreEqual(20, List.length games)
        
    [<Test>]
    member this.``in a tournment of two players there are two games``()  =
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let twoCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 2
        // when
        let games = makeGames twoCooperators
        // then
        Assert.AreEqual(2, List.length games)

    [<Test>]
    member this.``in a tournment of three player there are three games``()  =
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let threeCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 3
        // when
        let games = makeGames threeCooperators
        // then
        let expectedNumberOfGames = (3+2)* 2 // = 6
        Assert.AreEqual(6, List.length games)

    [<Test>]
    member this.``in a set of games plaid 10 times, the history of moves contains 10 elements``()  =
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let threeCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 3
        let games = makeGames threeCooperators
        // when
        let playedGames = playGamesNTimes games 10
        // then
        Assert.That(10 = List.length ((List.head playedGames).JointmoveHistory))

    [<Test>]
    member this.scoresOf10PlaysOfCooperatorsIs80()  =
        // given
        let cooperatorStrategyInfo = {Name="cooperator";Strategy=cooperatorStrategy}
        let twoCooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 2
        let games = makeGames twoCooperators
        // when
        let playedGames = playGamesNTimes games 10
        let scores = sortedPlayersScore playedGames
        let (_,firstScore) = List.head scores
        // then
        Assert.AreEqual(80,firstScore)

    [<Test>]
    member this.listIndexTest() =
        let myList = [0.5;1.0]
        let theIndex = myList |> List.findIndex(fun x -> x> 0.1)
        Assert.AreEqual(0,theIndex)
        let theSecondIndex = myList |> List.findIndex(fun x -> x> 0.6)
        Assert.AreEqual(1,theSecondIndex)


    [<Test>]
    member this.scoresOf10PlaysOfDefectorsIs60()  =
        // given
        let defectorStrategyInfo = {Name="defector";Strategy=defectorStrategy}
        let twoDefectors = makeNPlayersByStrategyInfo defectorStrategyInfo 2
        let games = makeGames twoDefectors
        // when
        let playedTournament = playGamesNTimes games 10
        // then
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
    member this.threePlayersProbabilitiesMatrix()=
        let defectorStrategyInfo =  {Name="defectorStrategyInfo";Strategy=defectorStrategy}
        let defector1 = {Name= "defector1";StrategyInfo=defectorStrategyInfo}
        let defector2 = {Name= "defector2";StrategyInfo=defectorStrategyInfo}
        let defector3 = {Name= "defector3";StrategyInfo=defectorStrategyInfo}
        let scores = [(defector1,6);(defector2,4);(defector3,2)]
        let cumulativeProbabilities = mutationProbabilities scores
        let firstInterval = List.head cumulativeProbabilities
        let secondInterval = List.head (List.tail cumulativeProbabilities)
        let thirdInterval = List.head (List.tail (List.tail cumulativeProbabilities))
        let actualProb = snd firstInterval
        let actualProb2 = snd secondInterval
        let actualProb3 = snd thirdInterval

        Assert.AreEqual(6.0/12.0,actualProb)
        Assert.AreEqual((6.0+4.0)/12.0,actualProb2,0.000001)
        Assert.AreEqual(1.0,actualProb3,0.000001)
        





    [<Test>]
    member this.pickUpPlayerOnCumulativeProbabilityList() =
        let defectorStrategyInfo =  {Name="defectorStrategyInfo";Strategy=defectorStrategy}
        let defector1 = {Name= "defector1";StrategyInfo=defectorStrategyInfo}
        let defector2 = {Name= "defector2";StrategyInfo=defectorStrategyInfo}
        let scores = [(defector1,6);(defector2,4)]
        let pickUpFirstInterval = (double)6/(double)10 - 0.001
        let pickUpSecondInterval = (double)6/(double)10 + 0.1
        let cumulativeProbabilitiesList = mutationProbabilities scores
        let firstTreshold  = snd (List.head cumulativeProbabilitiesList)
        let secondTreshold = snd (List.head (List.tail cumulativeProbabilitiesList))
        Assert.AreEqual(1.0,secondTreshold)

        let firstIntervalPlayer = pickUpAPlayerBasedOnMutationProbabilities cumulativeProbabilitiesList pickUpFirstInterval
        Assert.AreEqual(defector1,firstIntervalPlayer)
        let secondIntervalPlayer = pickUpAPlayerBasedOnMutationProbabilities cumulativeProbabilitiesList pickUpSecondInterval
        Assert.AreEqual(defector2,secondIntervalPlayer)


    [<Test>]
    member this.pickUpAPlayerBasedOnProbabilitiesList2()=
        let defectorStrategyInfo =  {Name="defectorStrategyInfo";Strategy=defectorStrategy}
        let defector1 = {Name= "defector1";StrategyInfo=defectorStrategyInfo}
        let defector2 = {Name= "defector2";StrategyInfo=defectorStrategyInfo}
        let defector3 = {Name= "defector3";StrategyInfo=defectorStrategyInfo}
        let scores = [(defector1,6);(defector2,4);(defector3,2)]
        let cumulativeProbabilities = mutationProbabilities scores
        // let pickUpFirstInterval = (double)6/(double)12 - 0.001
        let pickUpFirstInterval = 0.0
        // let pickUpSecondInterval = (double)10/(double)12 - 0.01
        let pickUpSecondInterval = (double)6/(double)12 + 0.01
        let pickUpThirdInterval = (double)10/(double)12 

        // let pickUpThirdInterval = 1.0-0.1


        let firstIntervalPlayer = pickUpAPlayerBasedOnMutationProbabilities cumulativeProbabilities pickUpFirstInterval
        Assert.AreEqual(defector1,firstIntervalPlayer)
        let secondIntervalPlayer = pickUpAPlayerBasedOnMutationProbabilities cumulativeProbabilities pickUpSecondInterval
        Assert.AreEqual(defector2,secondIntervalPlayer)
        let thirdIntervalPlayer = pickUpAPlayerBasedOnMutationProbabilities cumulativeProbabilities pickUpThirdInterval
        Assert.AreEqual(defector3,thirdIntervalPlayer)



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
        

    [<Test>]
    member this.ListPickUp() =
        let myList= [0.4;1.0]
        let actualIndex = myList |> List.findIndex (fun x -> x>=0.2)
        Assert.AreEqual(0,actualIndex)










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













