module Moose
open Animal
open Board
type Moose(pos:position, repTime:int,brd:Board<Animal>) as this =
    inherit Animal(pos,repTime,brd)

    let viewLength = 3
    
    // Returns sequence of position of wolves that are within view of the 
    // given position. 
    // (Skal muligvis boxe brd.findAtCoordinate(x+i, y+i) for ikke at få error) 
    // Kan denne elegant gøres til en abstrakt klasse? 
    member this.nearbyWolves ((x,y) : position) = 
        seq {
            for i = (-viewLength) to viewLength do 
                for j = (-viewLength) to viewLength do 
                    match this.animalAt (x+i)  (y+j) with
                    | Some(a) -> match a with
                                 | :? Moose -> ()
                                 | _ -> yield (x+i, y+j) 
                    | _ -> ()
        }

    
    // Finds the nearest wolf to the position (x,y) from all nearbyWolves to that position.
    // Returns a tuple consisting of the number of moves to the nearest
    // wolf, and the position given as an argument. 
    member this.nearestWolf ((x,y) : position) = 
        let wolves = this.nearbyWolves (x,y)
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
        if not (Seq.isEmpty (this.nearbyWolves this.pos)) then
            this.flee actSeq
        elif this.repTime = 0 then 
            this.chooseRandom (this.actSeqOf "Reproduce" actSeq)
        else
            this.chooseRandom (this.actSeqOf "Move" actSeq)

    override this.execute act = 
        match act with
        | Move(p) -> this.pos <- p
        | Reproduce(p) -> 
            brd.insert(Moose(p,repTime,brd))
            this.resetRepTime()
        | Eat(p) -> failwith "Moose tried to eat something"
    override this.represent = "M"
    override this.tick () =
        this.repTime <- this.repTime - 1
// prioritizing:

// 1: flygt fra ulv
// 2: formering hvis muligt
// 3: Flyt tilfældigt
