module Wolf
open Board
open Animal

type Wolf(startPos:position, repTimeDefault:int, feedTimeDefault:int, brd:Board<Animal>) =
    inherit Animal(startPos,repTimeDefault,brd)

    let viewLength = 2
    override this.represent = "W"
    member val feedTime = feedTimeDefault with get,set

    // Returns sequence of position of moose that are within view of the 
    // given position. 
    member this.nearbyMoose () = 
        let (x,y) = this.pos
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
        let moose = this.nearbyMoose ()
        let distList = Seq.map (fun elem -> (this.distanceTo (x,y) elem, (x,y))) moose 
        let sortedList = Seq.sortBy (fun elem -> fst elem) distList
        Seq.head sortedList

    // Selects the Move action that puts the animal most near a moose
    member this.huntMoose (actSeq : action seq) : action = 
        let moves = this.actSeqOf "Move" actSeq 
        
        let distList = Seq.map (fun elem -> this.nearestMoose (this.getCords elem)) moves
        let sortedList = Seq.sortBy (fun elem -> fst elem) distList
        Move(snd (Seq.head sortedList))
 
    // Selects action
    override this.prioritize (actSeq : action seq) = 
        let isNotWolf (act : action) =
            let p = this.getCords act
            match this.animalAt(p) with 
            | Some(a) -> match a with 
                            | :? Wolf -> false 
                            | _ -> true
            | None -> false
        let eatSeq = Seq.filter isNotWolf (this.actSeqOf "Eat" actSeq)
        let moveSeq = this.actSeqOf "Move" actSeq
        let reproduceSeq = this.actSeqOf "Reproduce" actSeq
        
        if (this.feedTime < feedTimeDefault / 2) && not (Seq.isEmpty eatSeq) then 
            this.chooseRandom eatSeq  
        elif (this.feedTime < feedTimeDefault / 2) && not (Seq.isEmpty (this.nearbyMoose())) then
            this.huntMoose moveSeq
        elif this.repTime <= 0 && not (Seq.isEmpty reproduceSeq) then 
            this.chooseRandom reproduceSeq
        elif not (Seq.isEmpty eatSeq) then 
            this.chooseRandom eatSeq
        elif not (Seq.isEmpty (this.nearbyMoose())) then
            this.huntMoose moveSeq
        else
            this.chooseRandom moveSeq
   
    // Takes the action given
    override this.execute act =
        printfn "%s%A did %A" this.represent this.pos act
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

    override this.tick () =
        this.repTime <- this.repTime - 1
        this.feedTime <- this.feedTime - 1
        if this.feedTime <= 0 then this.die()
