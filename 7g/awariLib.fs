module Awari
open System
type player = Player1 | Player2
type pit = {
  index : int
  beanCount: int
}
type board = pit list 

let isHome (b:board) (p:player) (i:pit) : bool = 
    let pitPlayer = if i.index < 7 then Player1 else Player2
    (pitPlayer = p) && (i.index%7=6)

let isGameOver (b:board) : Boolean =
  let mutable player1Beans = 0
  let mutable player2Beans = 0
  for pit in b do
    if pit.index%7<>6 then
      if pit.index >6 then player1Beans <- player1Beans + pit.beanCount
      if pit.index <6 then player2Beans <- player2Beans + pit.beanCount
  (player1Beans = 0) || (player2Beans = 0)

let getMove (b:board) (p:player) (s:string) : pit = 
  printfn "%s" s
  let mutable inputPit = int (System.Console.ReadLine ())
  while not (List.contains inputPit [1;2;3;4;5;6]) do
    printfn "Incorrect input, try again with one of the pits 1-6"
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
    let (newB, finalPitsPlayer, finalPit)= distribute b p i
    if not (isHome b finalPitsPlayer finalPit) 
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
