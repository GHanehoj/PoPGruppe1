module Awari
let boardSize = 14
let distribute (b:board) (p:player) (i:pit) : (board*player*pit) =
    if i.beanCount = 0 then (b,p,i)
    else
        let addBean (cp:pit) (index:int) : pit =
            let indexDiff = (cp.index - i.index - 1 + boardSize) % boardSize
            let beanSum = cp.beanCount + i.beanCount / 14 // Should the chosen pit hold more than 14 beans
            if diff < i.beanCount then
                {cp with beanCount = beanSum + 1}
            else
                {cp with beanCount = beanSum}
        let lastIndex = (i.index + i.beanCount) % boardSize 
        let np = if lastIndex < boardSize / 2 then Player1 else Player2 
        let ni = board.[lastIndex]
        let nb = List.mapi addBean board
        (nb,np,ni)
