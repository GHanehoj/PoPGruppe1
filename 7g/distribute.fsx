//module Awari
type player = Player1 | Player2
type pit = {
  index : int
  beanCount: int
}
type board = pit list
let newPlayer (p:player) :player = 
    match p with
    |Player1 -> Player2
    |Player2 -> Player1

let boardSize = 14

let replaceAtIndex (index:int) (ni:pit) (b:board) : board =
    let rep (currentIndex:int) (i:pit) =
        if currentIndex = index then ni else i;
    List.mapi rep b

let getHome (b:board) (p:player):pit =
    match p with
    |Player1 -> b.[6]
    |Player2 -> b.[13]

let addBean (op:pit) (cp:pit) : pit =
    let indexDiff = (cp.index - op.index - 1 + boardSize) % boardSize
    // Should the chosen pit hold more than 14 beans
    let beanSum = (if cp <> op then cp.beanCount else 0) + op.beanCount / boardSize 
    if indexDiff < op.beanCount then
        {cp with beanCount = beanSum + 1}
    else
        {cp with beanCount = beanSum}

let updateLastPit (b:board) (p:player) (i:pit) : (board*pit) = 
    if i.beanCount = 1 and i.index % 7 <> 6 then
        let home = getHome b p
        let oppositePit = b.[12 - i.index]
        let updatedBoard =
            b 
            |> (replaceAtIndex home.index {home with beanCount = home.beanCount + i.beanCount + oppositePit.beanCount })
            |> (replaceAtIndex (oppositePit.index) {oppositePit with beanCount = 0}) 
            |> (replaceAtIndex i.index {i with beanCount = 0}) 
        (updatedBoard,i) 
    else
       (b,i) 

let distribute (b:board) (p:player) (i:pit) : (board*pit) =
    if i.beanCount = 0 then (b,i)
    else
        let lastIndex = (i.index + i.beanCount) % boardSize 
        let ni = b.[lastIndex]
        let nb = List.map (addBean ni) b
        updateLastPit nb p ni

let b = List.init 14 (fun x -> {index = x; beanCount = 3})
let p = Player1
let i = b.[1]

let (nb,ni) =  distribute b p i
printfn "%A" nb
