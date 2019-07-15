# PrisonerDilemmaFsharp
https://tonyxzt.blogspot.com/2019/07/fsharp-code-kata-iterated-prinsoner.html



in the Library.fs file there are few notes about the project.
It is based on dotnet core  2.2, and tested on windows 8



this code contains in part code that I rewiewed and unit tested, and in part
code that needs some refactoryng: stay tuned for future releases

simple notes:
how to run interactively:
the solution has two subprojects/directories: prisonerDilemma and prisonerDilemma.Tests
in the prisonerDilemma directory, install the Chart.Library by the command: nuget install Chart.Library
run the repl: (fsi in windows, or fsharpi on mono)
from the repl:

\#I "packages/FSharp.Charting.2.1.0"   // note the version 2.1.0 may change
\#load "FSharp.Charting.fsx"
\#load "Library.fs" // load the prisoner dilemma library
open prinsonerDilemma.Program

now you can run: lineStatOfEvolution strategyInfos players 30 20 0.1
it whows a line chart with the number of individual for each strategy in 20 generations where each game consists in 20 iterations

lineStatOfEvolution strategyInfos players 30 20 0.1

the colors don't show the name of the strategy, but in text output you can read the values of the last generations

note: by default a fraction of individuals change strategy  by choosing a strategy from any player where higher scores players have more probability to be copied.

What I've seen is that on the long run any strategy may win.


If I change the mutation rule from "random" to "weakest" (I'll go deeper later about how easily change such behaviors) 
then the result is much more regular and shows that 
in a competition between defector, titForTat, random, cooperator, the winner is almost always the defector.





