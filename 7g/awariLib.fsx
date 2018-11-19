//module Awari
type player = Player1 | Player2
type pit = {
  index : int
  beanCount: int
}
type board = pit list 

let boardSize = 14

let printBoard (b:board) : unit = 
    let pitsToString acc i = acc + sprintf "%3i" i.beanCount
    let player2String = List.fold pitsToString " " (List.rev b.[7..12])
    let player1String = List.fold pitsToString " " (b.[0..5])
    let homePitString = sprintf "%i%21i" b.[13].beanCount b.[6].beanCount 
    printfn "%s\n%s\n%s" player2String homePitString player1String

let isHome (b:board) (p:player) (i:pit) : bool = 
    let pitPlayer = if i.index < 7 then Player1 else Player2
    (pitPlayer = p) && (i.index%7=6)

let isGameOver (b:board) : bool =
  let count (x:int,y:int) i:pit : int*int =
    if i.index%7<>6 then
      if i.index >6 then (x + i.beanCount,y)
      else (x,y+i.beanCount)
    else (x,y)
  
  let (p1Beans,p2Beans) = List.fold count (0,0) b
  (p1Beans = 0 || p2Beans = 0)



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
    if i.beanCount = 1 && i.index % 7 <> 6 then
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
        let nb = List.map (addBean i) b
        updateLastPit nb p ni



let rec getMove (b:board) (p:player) (s:string) : pit = 
  printfn "%s" s
  let inputPit = int (System.Console.ReadLine ())
  if not (List.contains inputPit [1;2;3;4;5;6]) then
    do printfn "Incorrect input, try again with one of the pits 1-6"
    getMove b p s  
  else
    match p with
    | Player1 -> b.[inputPit-1]
    | Player2 -> b.[inputPit+6]

let turn (b : board) (p : player) : board =
  let rec repeat (b: board) (p: player) (n: int) : board =
    printBoard b
    let str =
      if n = 0 then
        sprintf "Player %A's move? " p
      else 
        "Again? "
    let i = getMove b p str
    let (newB, finalPit)= distribute b p i
    if not (isHome newB p finalPit) 
       || (isGameOver b) then
      newB
    else
      repeat newB p (n + 1)
  repeat b p 0 

let rec play (b : board) (p : player) : board =
  if isGameOver b then
    b
  else
    let newB = turn b p
    let nextP =
      if p = Player1 then
        Player2
      else
        Player1
    play newB nextP

let b = List.init 14 (fun x -> {index = x; beanCount = 3})
let p = Player1

play b p |> ignore

