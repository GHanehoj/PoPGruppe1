module Environment
open AnimalFactory
open Board

type Environment(boardSize : int*int, wolfRepTime:int, wolfFeedTime:int, startWolfCount:int, mooseRepTime:int, startMooseCount:int) =
    let board = Board()
    let aFac = AnimalFactory(wolfRepTime, wolfFeedTime, mooseRepTime, board)
    aFac.createRandom(startWolfCount, startMooseCount)