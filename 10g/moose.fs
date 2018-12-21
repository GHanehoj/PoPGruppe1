module Moose

type Moose(pos:position, repTime:int) =
    inherit Animal(pos,repTime)

    override prioritize (actSeq: action seq) =
        ()

// prioritizing:

// 1: flygt fra ulv
// 2: formering hvis muligt
// 3: Flyt tilfældigt
