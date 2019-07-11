namespace prisonerDilemma

module Program =

    open System
    type Move = Cooperate | Defect
    type JointMove = {Player1Move: Move; Player2Move: Move}
    type JointScore = {Player1Score: int; Player2Score: int}

    let jointScore jointMove  =
        let R = 4 // my outcome if I cooperate and the other cooperates
        let S = 2 // my outcome if I cooperate and the other defects
        let T = 5 // my outcome if I defect ant the other cooperates
        let P = 3 // my outcome if I defect ant the other defects
        let checkPDConditions = (T > R) && (R > P) && (P > S)
        let _ = if ( not checkPDConditions) then failwith "this is not a pd game: violated  pd conditions: T > R > P > S"
         
        match jointMove with
            | {Player1Move= Cooperate ; Player2Move=Cooperate} -> {Player1Score=R; Player2Score=R }
            | {Player1Move= Cooperate ; Player2Move=Defect} ->    {Player1Score=S; Player2Score=T }
            | {Player1Move= Defect ; Player2Move=Cooperate} ->    {Player1Score=T; Player2Score=S} 
            | {Player1Move= Defect ; Player2Move=Defect} ->       {Player1Score=P; Player2Score=P}   
    
    type Strategy = JointMove list -> Move

    [<CustomEquality;CustomComparison>]
    type StrategyInfo = 
        {Name: string; Strategy:Strategy}
        override this.Equals(y) =
            match y with
            | :? StrategyInfo as other -> this.Name = other.Name
            | _ -> false
        override this.GetHashCode() = hash this.Name
        interface System.IComparable with
          member this.CompareTo y = String.Compare (this.Name, (y:?> StrategyInfo).Name)


    type Player = 
        {Name: string; StrategyInfo: StrategyInfo}

    type Game = { Player1:Player; Player2:Player; JointmoveHistory: JointMove list }
        
    let tick (game:Game) = 
        let playerOneMove = game.Player1.StrategyInfo.Strategy game.JointmoveHistory 
        let playerTwoMove = game.Player2.StrategyInfo.Strategy game.JointmoveHistory 
        {game with JointmoveHistory = {Player1Move=playerOneMove;Player2Move=playerTwoMove}::game.JointmoveHistory } 

    let nTicks (game: Game) n =
        [1 .. n ] |> List.fold (fun game _ -> game |> tick ) game

    let rand = new Random(System.DateTime.Now.Millisecond)

    let makeGames players =
        (players |> List.map (fun x -> ((players |> List.filter (fun z -> z.Name <> x.Name)) |> 
        (List.map (fun y ->  {Player1=x;Player2=y;JointmoveHistory=[]}  ))))) |> List.fold (@) [] 


    let playGamesNTimes games n =
        games |> List.map (fun x -> nTicks x n)

    let gameScores game =  
        game.JointmoveHistory |> List.fold (fun acc x -> 
            let jo = jointScore x;
            {Player1Score=jo.Player1Score+acc.Player1Score;
             Player2Score=jo.Player2Score+acc.Player2Score
            })
            {Player1Score=0;Player2Score=0} 

    let gamesScores games =
        games |> List.map (fun (x:Game) -> (x.Player1,x.Player2,gameScores x))

    let scoreForPlayer games player =
        let ot= gamesScores games 
        let gamesWherePlayerIsFirst =  ot |> List.filter (fun (player1,_,_) -> player1.Name = player.Name)
        let gamesWherePlayerIsSecond =  ot |> List.filter (fun (_,player2,_) -> player2.Name = player.Name)
        let scoresOfPlayerAsFirst =  gamesWherePlayerIsFirst |> List.sumBy (fun (_,_,x) -> x.Player1Score)
        let scoresOfPlayerAsSecond = gamesWherePlayerIsSecond |> List.sumBy (fun (_,_,x) -> x.Player2Score)
        scoresOfPlayerAsFirst+scoresOfPlayerAsSecond

    let makeNPlayersByStrategyInfo (strategyInfo:StrategyInfo) n =
        [0 .. n-1] |> List.map (fun x -> {Name=Guid.NewGuid().ToString();StrategyInfo=strategyInfo})

    let playersOfGames (games:Game list) =
        games |> List.map (fun x -> [x.Player1;x.Player2]) |> List.fold (@) [] |> Set.ofList |> Set.toList

    let allPlayersScore games =
        let players = playersOfGames games
        players |> List.map (fun x -> (x,scoreForPlayer games x)) 

    let sortedPlayersScore games =
        allPlayersScore games |> List.sortBy (fun (_,x) -> x)

    let (randomStrategy:Strategy) = 
        fun (_:JointMove list) ->
            match rand.Next(0,2) with
                | 0 -> Defect
                | _ -> Cooperate

    let (defectorStrategy:Strategy) =
        fun _ ->
            Defect

    let (cooperatorStrategy:Strategy) =
        fun _ ->
            Cooperate

    let (titForTatStrategy:Strategy) =
        fun (history: JointMove list) ->
            match history with
            | [] -> Cooperate
            | H::_ -> H.Player2Move

    let mutationProbabilities  (scores: (Player*int) list) =
        let totalOfScores = (double)(scores |> List.sumBy (fun (_,x) -> x))
        let probabilitiesOfMutationTo = scores |> List.map (fun (p,s) -> (p,(double)s/(double)totalOfScores))
        let thresholdProbabilities = probabilitiesOfMutationTo |> 
            List.map (fun (p,s) -> (p,
            (probabilitiesOfMutationTo |> List.takeWhile (fun (pl,_) -> pl <> p)  |> List.sumBy (fun (_,x) -> x))+s))
        thresholdProbabilities

    let pickUpAPlayerBasedOnMutationProbabilities (mutationProb: (Player*double) list) = 
        let dRand = rand.NextDouble() 
        let indexOfItemAfterTheOneToFind = mutationProb |> List.tryFindIndex (fun (_,x)-> x>dRand) 
        let actualIndex =
            match indexOfItemAfterTheOneToFind with
            | None -> List.length mutationProb - 1
            | Some 0 -> 0
            | Some X -> X - 1
        let (player,_) = mutationProb |> List.item  actualIndex
        player

    let mutateSomePlayerByRandomFactor 
        (probToChange:double) 
        (players: Player list) 
        (probList: (Player*double) list ) =

        players |> List.map (fun x -> 
            match rand.NextDouble() with 
            | X  when X <= probToChange ->  {x with StrategyInfo = (pickUpAPlayerBasedOnMutationProbabilities probList).StrategyInfo} 
            | _ -> x)


    let nextGenerationPlayers players numIterations randomFactor =
        let games = makeGames players
        let playedGames = playGamesNTimes games numIterations
        let scores = sortedPlayersScore playedGames
        let tranProb = mutationProbabilities scores
        let mutatedPlayers = mutateSomePlayerByRandomFactor randomFactor players tranProb
        mutatedPlayers

    let playersStrategyStats (players: Player list) =
        let strategies = players |> List.map (fun x -> x.StrategyInfo) |> Set.ofList |> Set.toList
        strategies |> List.map (fun x -> (x, players |> List.filter (fun y -> y.StrategyInfo = x) |> List.length))
        

    let cooperatorStrategyInfo = {Name="cooperatorStrategy";Strategy=cooperatorStrategy}
    let defectorStrategyInfo = {Name="defectorStrategy"; Strategy=defectorStrategy}
    let randomStrategyInfo = {Name="randomStrategy";Strategy=randomStrategy}
    let titForTatStrategyInfo = {Name="titForTatStrategy";Strategy=titForTatStrategy}

    let cooperatorPlayer = {Name="cooperator";StrategyInfo=cooperatorStrategyInfo}
    let defectorPlayer= {Name="defector";StrategyInfo=defectorStrategyInfo}
    let randomPlayer= {Name="random";StrategyInfo=randomStrategyInfo}
    let titForTatPlayer= {Name="titForTat";StrategyInfo=titForTatStrategyInfo}





// create 5 cooperators:
    let cooperators = makeNPlayersByStrategyInfo cooperatorStrategyInfo 5;
// create 5 defectors:
    let defectors = makeNPlayersByStrategyInfo defectorStrategyInfo 5;
// create 5 randoms:
    let randoms = makeNPlayersByStrategyInfo randomStrategyInfo 5;
// create 5 titForTats 
    let titForTats= makeNPlayersByStrategyInfo titForTatStrategyInfo 5;

// make them allPlayers:
    let players = cooperators@defectors@randoms@titForTats;

// if we make stats of the player we get what we already know: theet there are five players  
// players for each strategy

// we already know that there are 5 kind of player for each type, but we just double check:

//  playersStrategyStats players:
//   val it : (StrategyInfo * int) list =
//   [({Name = "cooperatorStrategy";
//      Strategy = <fun:cooperatorStrategy@363-1>;}, 5);
//    ({Name = "defectorStrategy";
//      Strategy = <fun:defectorStrategy@359-1>;}, 5);
//    ({Name = "randomStrategy";
//      Strategy = <fun:randomStrategy@353-1>;}, 5);
//    ({Name = "titForTatStrategy";
//      Strategy = <fun:titForTatStrategy@367-1>;}, 5)]

// now we want to make them play 20 iterations with each other, and 
// then make each player mutates according to the "replicatior dynamic"
//  rule: 
//  let players_generation_1 = nextGenerationPlayers players 20 1.0
//  see the stat by playersStrategyStats players_generation_1
//  get a new generation by
//  let players_generation_2 = nextGenerationPlayers players_generation_1 20 1.0
//  you can see the stat again to see how many players for each strategy are present.




    let games = makeGames players

    let playedGames = playGamesNTimes games 10

// scores:
    let scores = sortedPlayersScore playedGames
// compute transition probabilities:
    let tranProb = mutationProbabilities scores
// 
    let mutatedPlayers = mutateSomePlayerByRandomFactor 0.9 players tranProb



