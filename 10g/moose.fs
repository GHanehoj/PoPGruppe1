module Moose

type Moose(pos:position, repTime:int, aFac:AnimalFactory) =
    inherit Animal(pos,repTime, aFac)

    override this.actionList = []
