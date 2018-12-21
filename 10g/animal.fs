module Animal
open AnimalFactory
open Board
open Microsoft.FSharp.Reflection
open System.Linq.Expressions
type position = int*int

type action = Move of position
             |Reproduce of position
             |Eat of position

[<AbstractClass>]
type Animal(pos:position, repTime:int, brd:Board) =
    let mutable _pos = pos
    let generateMoves ((x,y) : position) =
        let actionTypes = FSharpType.GetUnionCases typeof<action>
        seq {
            for actionType in actionTypes do
                for i=(-1) to 1 do
                    for j=(-1) to 1 do
                        // We intentionally include actions to the same position
                        // and will filter the invalid ones later.
                        yield match actionType.Name with
                              |"Move" -> Move(x+i, y+j)
                              |"Reproduce" -> Reproduce(x+i, y+j)
                              |_ -> Eat(x+i, y+j)
        }  

    let filterInvalidActions (actSeq: action seq) =
        seq {
            for act in actSeq do
                if brd.findAtCoordinate(x,y) = None then
                    match act with
                    |Move(x,y) -> yield Move(x,y)
                    |Reproduce(x,y) -> yield Reproduce(x,y)
                    | _ -> ()
                if brd.findAtCoordinate(x,y) = Some Animal then
                    match act with
                    |Eat(x,y)  -> yield Eat(x,y)
                    | _ -> ()
        }


    member this.pos 
        with get() = _pos
        and set(newPos) = _pos <- newPos


    abstract member prioritize : action seq -> action

    // For mooses just decrease reproduction time by 1
    // For wolves decrease both repTime and hunger by 1
    abstract member tick : unit -> unit
    member this.takeTurn() =
        let availableActions = generateMoves(this.pos)
        let legalActions = filterInvalidActions(availableActions)
        let prioritizedActions = this.prioritize(legalActions)
        // Execute (prioritizedActions.nth 1)
        this.tick()
        ()

