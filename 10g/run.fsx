open Environment


[<EntryPoint>]
let main args =
    let env = Environment(int args.[0], args.[1], int args.[2], int args.[3], 
    int args.[4], int args.[5], int args.[6], int args.[7])
    env.startSimulation()
    0
