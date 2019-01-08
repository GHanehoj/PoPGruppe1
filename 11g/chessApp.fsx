#load "chess.fsx"
#load "pieces.fsx"
#load "player.fsx"
open Chess
open Pieces
open Player
/// Print various information about a piece
let printPiece (board : Board) (p : chessPiece) : unit =
  printfn "%A: %A %A" p p.position (p.availableMoves board true)

// Create a game
let board = Chess.Board () // Create a board
// Pieces are kept in an array for easy testing
let pieces = [|
  king (White) :> chessPiece;
  rook (White) :> chessPiece;
  rook (Black) :> chessPiece |]

let player1 = Human(board, White)
  
// Place pieces on the board
board.[3,1] <- Some pieces.[0]
board.[1,1] <- Some pieces.[1]
board.[4,1] <- Some pieces.[2]
printfn "%A" board
Array.iter (printPiece board) pieces
// Make moves
board.move (1,1) (6,1) // Moves a piece from (1,1) to (3,1)
printfn "%A" board
Array.iter (printPiece board) pieces

printfn "%A" (player1.nextMove())