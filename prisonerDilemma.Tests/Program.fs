open prisonerDilemma.Program
module Program = let [<EntryPoint>] main _ = 
    printf "hello world\n"
    // logNGenerationPlayers players 50 50 0.1

    lineStatOfEvolutionRef strategyInfos players 300 50 0.005
    0
