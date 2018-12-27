module Moose

type Moose(pos:position, repTime:int) =
    inherit Animal(pos,repTime)

    let viewLength = 3
    
    // Returns sequence of position of wolves that are within view of the 
    // given position. 
    // (Skal muligvis boxe brd.findAtCoordinate(x+i, y+i) for ikke at få error) 
    // Kan denne elegant gøres til en abstrakt klasse? 
    let nearbyWolves ((x,y) : position) = 
        seq {
            for i = (-viewLength) to viewLength do 
                for j = (-viewLength) to viewLength do 
                    match brd.findAtCoordinate(x+i, y+j) with
                    | :? Moose -> ()
                    | _ -> yield (x+i, y+j) 
        }



    override prioritize (actSeq: action seq) =
        ()

// prioritizing:

// 1: flygt fra ulv
// 2: formering hvis muligt
// 3: Flyt tilfældigt
