namespace prisonerDilemma

module Program =

    open System

    type Move = Cooperate | Defect

    type JointMove = {Player1Move: Move; Player2Move: Move}

    type JointOutcome = {Player1Outcome: int; Player2Outcome: int}

    let jointOutcome jointMove  =
        let R = 4 // my outcome if I cooperate and the other cooperates
        let S = 2 // my outcome if I cooperate and the other defects
        let T = 5 // my outcome if I defect ant the other cooperates
        let P = 3 // my outcome if I defect ant the other defects
        let checkPDConditions = (T > R) && (R > P) && (P > S)
        let _ = if ( not checkPDConditions) then failwith "this is not a pd game: violated  pd conditions: T > R > P > S"
         
        match jointMove with
            | {Player1Move= Cooperate ; Player2Move=Cooperate} -> {Player1Outcome=R; Player2Outcome=R }
            | {Player1Move= Cooperate ; Player2Move=Defect} ->    {Player1Outcome=S; Player2Outcome=T }
            | {Player1Move= Defect ; Player2Move=Cooperate} ->    {Player1Outcome=T; Player2Outcome=S} 
            | {Player1Move= Defect ; Player2Move=Defect} ->       {Player1Outcome=P; Player2Outcome=P}   

    
    type Strategy = JointMove list -> Move

    type StrategyInfo = {Name: string; Strategy:Strategy}

    type Player = {Name: string; StrategyInfo: StrategyInfo}

    type Game = { Player1:Player; Player2:Player; JointmoveHistory: JointMove list }

            
//    let cumulateOutcomes jointMovesList =
//        List.fold (fun acc x -> let outComes = jointOutcome x; 
//            { Player1Outcome=outComes.Player1Outcome+acc.Player1Outcome; 
//              Player2Outcome=outComes.Player2Outcome+acc.Player2Outcome } 
//            ) 
//        {Player1Outcome=0;Player2Outcome=0} 
        
    let tick (game:Game) = 
        let playerOneMove = game.Player1.StrategyInfo.Strategy game.JointmoveHistory 
        let playerTwoMove = game.Player2.StrategyInfo.Strategy game.JointmoveHistory 
        {game with JointmoveHistory = {Player1Move=playerOneMove;Player2Move=playerTwoMove}::game.JointmoveHistory } 
       // {Player1Outcome=0;Player2Outcome=0} 
        
    let nTicks (game:Game) n =
        [1 .. n ] |> List.fold (fun acc _ -> tick acc ) game

    type Tournment = {Games:Game list; IterationsPerGame: int}

    let rand = new Random(System.DateTime.Now.Millisecond)

    let makeTournment players iterationsPerGame = 
        let games = (players |> List.map (fun x -> ((players |> List.filter (fun z -> z.Name <> x.Name)) |> 
        (List.map (fun y ->  {Player1=x;Player2=y;JointmoveHistory=[]}  ))))) |> List.fold (@) []
        {Games = games;IterationsPerGame=iterationsPerGame}

    let playTournment (tournment:Tournment) = 
        tournment.Games |> List.map (fun x -> nTicks x tournment.IterationsPerGame)


    let gameOutcomes game =  
        game.JointmoveHistory |> List.fold (fun acc x -> 
            let jo = jointOutcome x;
            {Player1Outcome=jo.Player1Outcome+acc.Player1Outcome;
             Player2Outcome=jo.Player2Outcome+acc.Player2Outcome
            })
            {Player1Outcome=0;Player2Outcome=0} 

    let gamesOutcomes games =
        games |> List.map (fun (x:Game) -> (x.Player1,x.Player2,gameOutcomes x))

    let (randomPlay:Strategy) = 
        fun (_:JointMove list) ->
            match rand.Next(0,2) with
                | 0 -> Defect
                | _ -> Cooperate

    let (defectorPlay:Strategy) =
        fun _ ->
            Defect

    let (cooperatorPlay:Strategy) =
        fun _ ->
            Cooperate


    let (titForTat:Strategy) =
        fun (history: JointMove list) ->
            match history with
            | [] -> Cooperate
            | H::_ -> H.Player2Move








    let hello name =
        printfn "Hello %s" name
    let prisoner x = raise(System.NotImplementedException("not implemented"))
            
