module Wolf

type Wolf(pos:position, repTime:int, aFac:AnimalFactory) =
    inherit Animal(pos,repTime, aFac)

    override prioritize (actSeq: action seq) =
        ()



// prioritizing:

// 1: Hvis sult < XXXX -> Så jagt elge
// 2: formering hvis muligt
// 3: Jagt elge
// 4: flyt tilfældigt


// Læg tilfældig 0<x<1 til alle prioriteringerne for at få tilfældig