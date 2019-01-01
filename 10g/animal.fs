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


    // Finds the number of moves required to go from position
    // Move(x1,y1) to (x2,y2)
    member this.distanceTo (Move(x1,y1) : action) ((x2,y2) : position) = 
        let d1,d2 = (abs (x2-x1) , abs (y2-y1)) 
        if d1 > d2 then d1 else d2 

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
    abstract member represent : string 
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
    
    
