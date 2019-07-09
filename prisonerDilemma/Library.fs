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
    type StrategyInfo = {Name: string; Strategy:Strategy}

    [<CustomEquality;CustomComparison>]
    type Player = 
        {Name: string; StrategyInfo: StrategyInfo}
        override this.Equals(y) =
            match y with
            | :? Player as other  -> this.Name = other.Name
            | _ -> false
        override x.GetHashCode() = hash x.Name
        interface System.IComparable with
          //member x.CompareTo y = compare x (y :?> Player)
          //member x.CompareTo y = String.Compare ((((Player)x).Name) , (((Player)y).Name))
          member x.CompareTo y = String.Compare (x.Name, (y:?> Player).Name)

    type Game = { Player1:Player; Player2:Player; JointmoveHistory: JointMove list }
        
    let tick (game:Game) = 
        let playerOneMove = game.Player1.StrategyInfo.Strategy game.JointmoveHistory 
        let playerTwoMove = game.Player2.StrategyInfo.Strategy game.JointmoveHistory 
        {game with JointmoveHistory = {Player1Move=playerOneMove;Player2Move=playerTwoMove}::game.JointmoveHistory } 

    let nTicks (game: Game) n =
        [1 .. n ] |> List.fold (fun game _ -> game |> tick ) game




    type Tournment = {Games:Game list; TicksForGame: int}

    let rand = new Random(System.DateTime.Now.Millisecond)

    let makeTournment players ticksForGame = 
        let games = (players |> List.map (fun x -> ((players |> List.filter (fun z -> z.Name <> x.Name)) |> 
        (List.map (fun y ->  {Player1=x;Player2=y;JointmoveHistory=[]}  ))))) |> List.fold (@) [] 
        {Games = games;TicksForGame=ticksForGame}

    let playTournment (tournment:Tournment) = 
        tournment.Games |> List.map (fun x -> nTicks x tournment.TicksForGame)

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
        [0 .. n-1] |> List.map (fun x -> {Name=(strategyInfo.Name + "_"+(string)x);StrategyInfo=strategyInfo})

    let playersOfGames (games:Game list) =
        games |> List.map (fun x -> [x.Player1;x.Player2]) |> List.fold (@) [] |> Set.ofList |> Set.toList

    let allPlayersScore games =
        let playerNames = playersOfGames games
        playerNames |> List.map (fun x -> (x,scoreForPlayer games x)) 

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

    let cooperatorStrategyInfo = {Name="cooperatorStrategy";Strategy=cooperatorStrategy}
    let defectorStrategyInfo = {Name="defectorStrategy"; Strategy=defectorStrategy}
    let randomStrategyInfo = {Name="randomStrategy";Strategy=randomStrategy}
    let titForTatStrategyInfo = {Name="titForTatStrategy";Strategy=titForTatStrategy}

    let cooperatorPlayer = {Name="cooperator";StrategyInfo=cooperatorStrategyInfo}
    let defectorPlayer= {Name="defector";StrategyInfo=defectorStrategyInfo}
    let randomPlayer= {Name="random";StrategyInfo=randomStrategyInfo}
    let titForTatPlayer= {Name="titForTat";StrategyInfo=titForTatStrategyInfo}

