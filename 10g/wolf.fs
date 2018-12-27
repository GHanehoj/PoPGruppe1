module Wolf

type Wolf(pos:position, repTime:int, feedTime:int) =
    inherit Animal(pos,repTime)

    let viewLength = 2

    // Returns sequence of position of moose that are within view of the 
    // given position. 
    // (Skal muligvis boxe brd.findAtCoordinate(x+i, y+i) for ikke at få error) 
    let nearbyMoose ((x,y) : position) = 
        seq {
            for i = (-viewLength) to viewLength do 
                for j = (-viewLength) to viewLength do 
                    match brd.findAtCoordinate(x+i, y+i) with
                    | :? Wolf -> ()
                    | _ -> yield (x+i, y+i) 
        }

    let mutable _feedTime = feedTime

    member this.feedTime
        with get() = _feedTime
        and set(newFeedTime) = _feedTime <- newFeedTime

    override prioritize (actSeq: action seq) =
        ()


    override tick () =
        ()

// prioritizing:

// 1: Hvis sult < XXXX -> Så jagt elge
// 2: formering hvis muligt
// 3: Jagt elge
// 4: flyt tilfældigt


// Læg tilfældig 0<x<1 til alle prioriteringerne for at få tilfældig
