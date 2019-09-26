open prisonerDilemma.Program
module Program = let [<EntryPoint>] main _ = 
    printf "hello world\n"
    logNGenerationPlayers players 50 50 0.1

    // lineStatOfEvolution strategyInfos players 30 20 0.1
    0
