module Wolf

type Wolf(pos:position, repTime:int, feedTime:int) =
    inherit Animal(pos,repTime)

    let _feedTime = feedTime

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
