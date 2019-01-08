module Player
#load "pieces.fsx"
open Chess
// An abstract chess piece 
[<AbstractClass>]
type Player(board : Board, color : Color) =
  override this.ToString () = color.ToString()

  member val color = color with get,set
  abstract member nextMove : unit -> string

type Human (board : Board, color : Color) = 
  inherit Player(board, color)
  
  // Returns a codestring or "quit"
  override this.nextMove() =
    printfn "Enter your move"
    let input = System.Console.ReadLine()
    if input <> "quit" then
      if input.Length = 4 then
        match board.Code2Pos input.[0..1] with
        | Some origin ->       
          match board.Code2Pos input.[2..3] with
          | Some target ->
            if origin <> target then input
            else this.nextMove()
          | _ -> this.nextMove()
        | _ -> this.nextMove()
      else this.nextMove()
    else "quit"