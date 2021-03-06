module Animal
open Board
open Microsoft.FSharp.Reflection
open System.Linq.Expressions
type position = int*int

type action = Move of position
             |Reproduce of position
             |Eat of position


[<AbstractClass>]
type Animal(startPos:position, repTimeDefault:int, brd:Board<Animal>) =
    // Variables
    member val repTime = repTimeDefault with get,set
    member this.resetRepTime () = this.repTime <- repTimeDefault 
    member val pos = startPos with get,set
    member val alive = true with get,set
    override this.Equals(other) =
        match other with 
        | :? Animal as a-> this.pos = a.pos
        | _ -> false
    override this.GetHashCode() =
        hash this.pos

    // Generates a sequence of all moves, even those which are impossible.
    member this.generateMoves() =
        let actionTypes = FSharpType.GetUnionCases typeof<action>
        let (x,y) = this.pos
        seq {
            for actionType in actionTypes do
                for i=(-1) to 1 do
                    for j=(-1) to 1 do
                        yield match actionType.Name with
                              |"Move" -> Move(x+i, y+j)
                              |"Reproduce" -> Reproduce(x+i, y+j)
                              |_ -> Eat(x+i, y+j)
        }
    // Filters the impossible moves from the rest
    member this.filterInvalidActions (actSeq: action seq) =
        seq {
            for act in actSeq do
                let a = this.animalAt(this.getCords act)
                let (x,y) = this.getCords act
                if x >= 0 && x < brd.size && y >= 0 && y < brd.size then
                    match act with
                    | Move(x,y) when a = None || a = Some this -> yield Move(x,y)
                    | Reproduce(x,y) when a = None -> yield Reproduce(x,y)
                    | Eat(x,y) when a <> None -> yield Eat(x,y)
                    | _ -> () 
        }

    abstract member prioritize : action seq -> action
    abstract member represent : string 
    abstract member tick : unit -> unit
    abstract member execute : action -> unit
    member this.takeTurn() =
        if this.alive then
            let availableActions = this.generateMoves()
            let legalActions = this.filterInvalidActions(availableActions)
            let prioritizedAction = this.prioritize(legalActions)
            this.execute(prioritizedAction)
            this.tick()
    member this.die () =
        brd.delete(this)
        this.alive <- false


    // Below are some helperfunctions
    // Returns the animal (if any) at a given position
    member this.animalAt((x,y) : position) : Animal option=
        let lst = brd.getContent
        match List.filter (fun (a:Animal) -> a.pos = (x,y)) lst with
        | a::t  -> Some a
        | _     -> None
    // Returns the coordinates that a given action targets
    member this.getCords(act:action) = 
        match act with
        | Eat(x,y) -> (x,y)
        | Move(x,y) -> (x,y)
        | Reproduce(x,y) -> (x,y)
    // Finds the number of moves required to go from position (x1,y1) to (x2,y2)
    member this.distanceTo ((x1,y1) : position) ((x2,y2) : position) = 
        max (abs (x2-x1)) (abs (y2-y1))
    // Returns a list consisting of the action specified with actionName
    member this.actSeqOf (actionName : string) (actSeq : action seq) =
        Seq.choose (fun elem -> 
            match elem.GetType().Name with 
            | x when x = actionName -> Some (elem)
            | _ -> None) actSeq 
    // Chooses a random action from the sequence
    member this.chooseRandom (actSeq : action seq) =
        let rnd = System.Random()
        Seq.item (rnd.Next(Seq.length actSeq)) actSeq 

    
