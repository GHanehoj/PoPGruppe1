module Moose
open Animal
open Board
type Moose(startPos:position, repTimeDefault:int,brd:Board<Animal>) =
    inherit Animal(startPos,repTimeDefault,brd)

    let viewLength = 1
    
    // Returns sequence of position of wolves that are within view of the 
    // given position. 
    // (Skal muligvis boxe brd.findAtCoordinate(x+i, y+i) for ikke at få error) 
    // Kan denne elegant gøres til en abstrakt klasse? 
    member this.nearbyWolves () =
        let (x,y) = this.pos 
        seq {
            for i = (-viewLength) to viewLength do 
                for j = (-viewLength) to viewLength do 
                    match this.animalAt((x+i),(y+j)) with
                    | Some(a) -> match a with
                                 | :? Moose -> ()
                                 | _ -> yield (x+i, y+j) 
                    | _ -> ()
        }

    
    // Finds the nearest wolf to the position (x,y) from all nearbyWolves to that position.
    // Returns a tuple consisting of the number of moves to the nearest
    // wolf, and the position given as an argument. 
    member this.nearestWolf ((x,y) : position) = 
        let wolves = this.nearbyWolves()
        let distList = Seq.map (fun elem -> (this.distanceTo (x,y) elem, (x,y))) wolves 
        let sortedList = Seq.sortBy (fun elem -> fst elem) distList
        Seq.head sortedList

    // Selects the Move action that puts the animal most furthest away from the wolves.
    member this.flee (actSeq : action seq) : action = 
        let moves = this.actSeqOf "Move" actSeq  
        let distList = Seq.map (fun elem -> this.nearestWolf (this.getCords elem)) moves
        let sortedList = Seq.sortBy (fun elem -> fst elem) distList
        Move(snd (Seq.last sortedList))
        
    // Selects action
    override this.prioritize (actSeq : action seq) =
        let moveSeq = this.actSeqOf "Move" actSeq
        let reproduceSeq = this.actSeqOf "Reproduce" actSeq
        if not (Seq.isEmpty (this.nearbyWolves())) && not (Seq.isEmpty moveSeq) then
            this.flee actSeq
        elif this.repTime <= 0 && not (Seq.isEmpty reproduceSeq) then 
            this.chooseRandom reproduceSeq
        else
            this.chooseRandom moveSeq

    override this.execute act =
        printfn "%s%A did %A" this.represent this.pos act 
        match act with
        | Move(p) -> this.pos <- p
        | Reproduce(p) -> 
            brd.insert(Moose(p,repTimeDefault,brd))
            this.resetRepTime()
        | Eat(p) -> failwith "Moose tried to eat something"
    override this.represent = "M"
    override this.tick () =
        this.repTime <- this.repTime - 1
// prioritizing:

// 1: flygt fra ulv
// 2: formering hvis muligt
// 3: Flyt tilfældigt
