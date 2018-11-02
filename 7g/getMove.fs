module Awari
type player = Player1 | Player2
type pit = {
  index : int
  beanCount: int
}
type board = pit list

let getMove (b:board) (p:player) (s:string) : pit = 
  printfn "%s" s
  let mutable inputPit = int (System.Console.ReadLine ())
  while not (List.contains inputPit [1;2;3;4;5;6]) do
    printfn "Incorrect input, try again with one of the pits 1-6"
  match p with
  | Player1 -> b.[inputPit-1]
  | Player2 -> b.[inputPit+6]
