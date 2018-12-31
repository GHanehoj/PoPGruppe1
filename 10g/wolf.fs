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

    member this.feedTime
        with get() = _feedTime
        and set(newFeedTime) = _feedTime <- newFeedTime

    override this.prioritize (actSeq: action seq) =
        Move(0,0)


    override this.tick () =
        this.repTime <- this.repTime - 1
        this.feedTime <- this.feedTime - 1
        if this.feedTime <= 0 then this.die()

// prioritizing:

// 1: Hvis sult < XXXX -> Så jagt elge
// 2: formering hvis muligt
// 3: Jagt elge
// 4: flyt tilfældigt


// Læg tilfældig 0<x<1 til alle prioriteringerne for at få tilfældig
