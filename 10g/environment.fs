module Environment
open AnimalFactory
open Board

type Environment(boardSize : int*int, wolfRepTime:int, wolfFeedTime:int, startWolfCount:int, mooseRepTime:int, startMooseCount:int) =
    member startSimulation() =
        let board = Board()
        //0. Create board & animals

        //WHILE LOOP:
        //1. Get random seq of all animals

        //2. Tick each animal

        //3. Print board (override "toString")
        printfn "%A" this