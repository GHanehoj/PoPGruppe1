module Environment
open AnimalFactory
open Board

type Environment(boardSize : int*int, wolfRepTime:int, wolfFeedTime:int, startWolfCount:int, mooseRepTime:int, startMooseCount:int) =
    let board = Board()
    let aFac = AnimalFactory(wolfRepTime, wolfFeedTime, mooseRepTime, board)
    do aFac.createRandom(startWolfCount, startMooseCount)
    member startSimulation() =
        //1. Get random seq of all animals

        //2. Tick each animal

        //3. Print board
        printfn "%A" board