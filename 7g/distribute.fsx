//module Awari

type player = Player1 | Player2
type pit = {
  index : int
  beanCount: int
}
type board = pit list

let boardSize = 14
let distribute (b:board) (p:player) (i:pit) : (board*player*pit) =
    if i.beanCount = 0 then (b,p,i)
    else
        let addBean (index:int) (cp:pit) : pit =
            let indexDiff = (cp.index - i.index - 1 + boardSize) % boardSize
            printfn "%i %i" indexDiff i.beanCount 
            let beanSum = cp.beanCount + i.beanCount / 14 // Should the chosen pit hold more than 14 beans
            if indexDiff < i.beanCount then
                {cp with beanCount = beanSum + 1}
            else
                {cp with beanCount = beanSum}
        let lastIndex = (i.index + i.beanCount) % boardSize 
        let np = if lastIndex < boardSize / 2 then Player1 else Player2 
        let ni = b.[lastIndex]
        let nb = List.mapi addBean b
        (nb,np,ni)

let b = List.init 14 (fun x -> {index = x; beanCount = 3})
let p = Player1
let i = b.[1]

let (nb,np,ni) =  distribute b p i
printfn "%A" nb
