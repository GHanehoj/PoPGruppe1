module Awari
open System
type player = Player1 | Player2
type pit = {
  homePit : Boolean
  side : player
  beanCount: int
}
type board = {
  pits : pit array
}

// intentionally many missing implementations and additions
let printBoard (b:board) : unit =
  printfn "%4i%3i%3i%3i%3i%3i" (b.pits.[12].beanCount) (b.pits.[11].beanCount) (b.pits.[10].beanCount) (b.pits.[9].beanCount) (b.pits.[8].beanCount) (b.pits.[7].beanCount)
  printfn "%i%21i" (b.pits.[13].beanCount) (b.pits.[6].beanCount)
  printfn "%4i%3i%3i%3i%3i%3i"( b.pits.[0].beanCount) (b.pits.[1].beanCount) (b.pits.[2].beanCount) (b.pits.[3].beanCount) (b.pits.[4].beanCount) (b.pits.[5].beanCount)

let isHome (b:board) (p:player) (i:pit) : bool = (i.side = p) && (i.homePit)

let isGameOver (b:board) : Boolean =
  let mutable player1Beans = 0
  let mutable player2Beans = 0
  for pit in b.pits do
    if not pit.homePit then
      if pit.side = Player1 then player1Beans <- player1Beans + pit.beanCount
      if pit.side = Player2 then player2Beans <- player2Beans + pit.beanCount
  (player1Beans = 0) || (player2Beans = 0)

let getMove (b:board) (p:player) (s:string) : pit = 
  printfn "%s" s
  let mutable inputPit = int (System.Console.ReadLine ())
  while not (List.contains inputPit [1;2;3;4;5;6]) do
    printfn "Incorrect input, try again with one of the pits 1-6"
  match p with
  | Player1 -> b.pits.[inputPit-1]
  | Player2 -> b.pits.[inputPit+6]

let distribute (b:board) (p:player) (i:pit) : (board*player*pit) = 
  let placeBeans (b:board) (i:pit) (n:int) : board =
    

let turn (b : board) (p : player) : board =
  let rec repeat (b: board) (p: player) (n: int) : board =
    printBoard b
    let str =
      if n = 0 then
        sprintf "Player %A's move? " p
      else 
        "Again? "
    let i = getMove b p str
    let (newB, finalPitsPlayer, finalPit)= distribute b p i
    if not (isHome b finalPitsPlayer finalPit) || (isGameOver b) then
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