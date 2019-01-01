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
    let nearbyWolves ((x,y) : position) = 
        seq {
            for i = (-viewLength) to viewLength do 
                for j = (-viewLength) to viewLength do 
                    match this.animalAt (x+i)  (y+j) with
                    | Some(a) -> match a with
                                 | :? Moose -> ()
                                 | _ -> yield (x+i, y+j) 
                    | _ -> ()
        }


    override this.represent = "M"
    override this.prioritize (actSeq: action seq) =
        Move(0,0)
    override this.tick () =
        this.repTime <- this.repTime - 1
// prioritizing:

// 1: flygt fra ulv
// 2: formering hvis muligt
// 3: Flyt tilfældigt
