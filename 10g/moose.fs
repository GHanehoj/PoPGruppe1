#r "animal"
open Animal

type Moose(pos:position, repTime:int) =
    inherit Animal(pos,repTime)
    override this.actionList = []
