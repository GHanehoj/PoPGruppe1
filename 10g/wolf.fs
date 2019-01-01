module Wolf
open Board
open Animal

type Wolf(startPos:position, repTimeDefault:int, feedTimeDefault:int, brd:Board<Animal>) =
    inherit Animal(startPos,repTimeDefault,brd)

    let viewLength = 2

    // Returns sequence of position of moose that are within view of the 
    // given position. 
    // (Skal muligvis boxe brd.findAtCoordinate(x+i, y+i) for ikke at få error) 
    member this.nearbyMoose ((x,y) : position) = 
        seq {
            for i = (-viewLength) to viewLength do 
                for j = (-viewLength) to viewLength do 
                    match this.animalAt((x+i),(y+j)) with
                    | Some(a) -> match a with
                                 | :? Wolf -> ()
                                 | _ -> yield (x+i, y+j) 
                    | _ -> ()
        }


    // Finds the nearest moose to the action Move(x,y) from all nearbyMoose to that position.
    // Returns a tuple consisting of the number of moves to the nearest
    // moose, and the position given as an argument. 
    member this.nearestMoose ((x,y) : position) = 
        let moose = this.nearbyMoose (x,y)
        let distList = Seq.map (fun elem -> (this.distanceTo (x,y) elem, (x,y))) moose 
        let sortedList = Seq.sortBy (fun elem -> fst elem) distList
        Seq.head sortedList

    // Selects the Move action that puts the animal most near a moose
    member this.huntMoose (actSeq : action seq) : action = 
        let moves = this.actSeqOf "Move" actSeq 
        
        let distList = Seq.map (fun elem -> this.nearestMoose (this.getCords elem)) moves
        let sortedList = Seq.sortBy (fun elem -> fst elem) distList
        Move(snd (Seq.head sortedList))
    override this.execute act =
        match act with 
        | Move(p) -> this.pos <- p
        | Reproduce(p) ->
            brd.insert(Wolf(p,repTimeDefault,feedTimeDefault,brd))
            this.resetRepTime()
        | Eat(x,y) ->
            this.feedTime <- feedTimeDefault
            match this.animalAt(x,y) with
            | Some(a) -> a.die()
            | None -> failwith "Trying to eat a None"

    // Selects action
    override this.prioritize (actSeq : action seq) = 
        if (this.feedTime < feedTimeDefault / 2) && not (Seq.isEmpty (this.actSeqOf "Eat" actSeq)) then 
            this.chooseRandom (this.actSeqOf "Eat" actSeq)  
        elif (this.feedTime < feedTimeDefault / 2) && not (Seq.isEmpty (this.nearbyMoose this.pos)) then
            this.huntMoose (this.actSeqOf "Move" actSeq)
        elif this.repTime = 0 then 
            this.chooseRandom (this.actSeqOf "Reproduce" actSeq)
        elif not (Seq.isEmpty (this.nearbyMoose this.pos)) then
            this.huntMoose (this.actSeqOf "Move" actSeq)
        elif not (Seq.isEmpty (this.actSeqOf "Move" actSeq)) then 
            this.chooseRandom (this.actSeqOf "Move" actSeq)
        else
            Move(this.pos)


    override this.represent = "W"
    member val feedTime = feedTimeDefault with get,set

    override this.tick () =
        this.repTime <- this.repTime - 1
        this.feedTime <- this.feedTime - 1
        if this.feedTime <= 0 then this.die()
// prioritizing:

// 1: Hvis sult < XXXX -> Så jagt elge
// 2: formering hvis muligt
// 3: Jagt elge
// 4: flyt tilfældigt
