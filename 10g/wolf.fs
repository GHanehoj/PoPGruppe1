module Wolf
open Board
open Animal

type Wolf(pos:position, repTime:int, feedTime:int, brd:Board<Animal>) as this =
    inherit Animal(pos,repTime,brd)

    let viewLength = 2

    // Returns sequence of position of moose that are within view of the 
    // given position. 
    // (Skal muligvis boxe brd.findAtCoordinate(x+i, y+i) for ikke at få error) 
    let nearbyMoose ((x,y) : position) = 
        seq {
            for i = (-viewLength) to viewLength do 
                for j = (-viewLength) to viewLength do 
                    match this.animalAt (x+i)  (y+j) with
                    | Some(a) -> match a with
                                 | :? Wolf -> ()
                                 | _ -> yield (x+i, y+j) 
                    | _ -> ()
        }

    let mutable _feedTime = feedTime

    // Finds the nearest moose to the action Move(x,y) from all nearbyMoose to that position.
    // Returns a tuple consisting of the number of moves to the nearest
    // moose, and the position given as an argument. 
    let nearestMoose (Move(x,y) : action) = 
        let moose = nearbyMoose (x,y)
        let distList = Seq.map (fun elem -> (distanceTo (Move(x,y)) elem, (x,y))) moose 
        let sortedList = Seq.sortBy (fun elem -> fst elem) distList
        Seq.head sortedList

    // Selects the Move action that puts the animal most near a moose
    let huntMoose (actSeq : action seq) : action = 
        let moves = actSeqOf "Move" actSeq 
        let distList = Seq.map (fun elem -> nearestMoose elem) moves
        let sortedList = Seq.sortBy (fun elem -> fst elem) distList
        Move(snd (Seq.head sortedList))
        
    // Selects action
    override this.prioritize (actSeq : action seq) = 
        if feedTime < 5 && not (Seq.isEmpty (actSeqOf "Eat" actSeq)) then 
            chooseRandom (actSeqOf "Eat" actSeq)  
        elif feedTime < 5 && not (Seq.isEmpty nearbyMoose) then
            huntMoose (actSeqOf "Move" actSeq)
        elif repTime = 0 then 
            chooseRandom (actSeqOf "Reproduce" actSeq)
        elif not (Seq.isEmpty nearbyMoose) then
            huntMoose (actSeqOf "Move" actSeq)
        else 
            chooseRandom (actSeqOf "Move" actSeq)


    override this.represent = "W"
    member this.feedTime
        with get() = _feedTime
        and set(newFeedTime) = _feedTime <- newFeedTime


    override this.tick () =
        this.repTime <- this.repTime - 1
        this.feedTime <- this.feedTime - 1
        if this.feedTime <= 0 then this.die()
// prioritizing:

// 1: Hvis sult < XXXX -> Så jagt elge
// 2: formering hvis muligt
// 3: Jagt elge
// 4: flyt tilfældigt
