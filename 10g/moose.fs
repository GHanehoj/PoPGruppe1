module Moose
type AnimalFactory = int
type position = int*int

type action = Move of position
             |Reproduce of position
             |Eat of position

[<AbstractClass>]
type Animal(pos:position, repTime:int, aFac:AnimalFactory) =
    let mutable _pos = pos
    member this.pos with get() = _pos
    member this.pos with set(newPos) = _pos <- newPos
    member this.takeTurn() =
    member this.tick





type Moose(pos:position, repTime:int, aFac:AnimalFactory) =
    inherit Animal(pos,repTime, aFac)

    override this.actionList = []
