module Animal

type position = int*int

type action = Move of position
             |Reproduce of position
             |Eat of position

[<AbstractClass>]
type Animal(pos:position, repTime:int) =
    let mutable _pos = pos
    member this.pos with get() = _pos
    member this.pos with set(newPos) = _pos <- newPos
    abstract member actionList : action list
