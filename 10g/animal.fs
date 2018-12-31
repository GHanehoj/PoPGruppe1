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

    let _animalAt x y : Animal option=
        let lst = brd.getContent
        match List.filter (fun (a:Animal) -> a.pos = (x,y)) lst with
        | a::t  -> Some a
        | _     -> None

    let filterInvalidActions (actSeq: action seq) =
        seq {
            for act in actSeq do
                match act with
                | Move(x,y) when _animalAt x y = None -> yield Move(x,y)
                | Reproduce(x,y) when _animalAt x y = None -> yield Reproduce(x,y)
                | Eat(x,y) when _animalAt x y <> None -> yield Eat(x,y)
                | _ -> () 
        }
    member this.animalAt = _animalAt
    member val repTime = repTimeDefault with get,set
    member this.resetRepTime () = this.repTime <- repTimeDefault 
    member val pos = startPos with get,set  
    override this.Equals(other) =
        match other with 
        | :? Animal as a-> this.pos = a.pos
        | _ -> false
    override this.GetHashCode() =
        hash this.pos

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
    
    member this.die () =
        brd.delete(this)    
    
    
