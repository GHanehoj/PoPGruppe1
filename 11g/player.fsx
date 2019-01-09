module Player
#load "pieces.fsx"
open Chess
// An abstract Player, so a future implementation of AI is easily possible 
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
        match board.code2Pos input.[0..1] with
        | Some origin ->
          match board.[fst origin, snd origin] with
          | Some piece when piece.color = this.color ->   
            match board.code2Pos input.[2..3] with
            | Some target ->
              let availMoves = (piece.availableMoves board true)
              // .Value is used, as a chessPiece always only will appear in availMoves if its position isn't None
              printfn "%A" availMoves
              if List.contains target (fst availMoves) || List.exists (fun (availTarget : chessPiece) -> availTarget.position.Value = target) (snd availMoves) then 
                input
              else
                printfn "Invalid move: Move was not valid for this piece" 
                this.nextMove()
            | _ -> 
              printfn "Invalid move: Second coordinate was invalid" 
              this.nextMove()
          | _ -> 
            printfn "Invalid move: First coordinate must contain one of your pieces" 
            this.nextMove()
        | _ -> 
          printfn "Invalid move: First coordinate was invalid" 
          this.nextMove()
      else 
        printfn "Invalid move: Input should be exactly 4 characters ex. a1b2" 
        this.nextMove()
    else "quit"
