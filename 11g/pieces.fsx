module Pieces
#load "chess.fsx"
open Chess
/// A king is a chessPiece which moves 1 square in any direction
type king(col : Color) =
  inherit chessPiece(col)
  override this.nameOfType = "king"
  override this.candiateRelativeMoves =
      [[(-1,0)];[(-1,1)];[(0,1)];[(1,1)];
      [(1,0)];[(1,-1)];[(0,-1)];[(-1,-1)]]
// A queen is a chessPiece which moves any number of squares in any direction
type queen (col : Color) =
    inherit chessPiece(col)
    let indToRel = [
        fun elm -> (elm,0); 
        fun elm -> (-elm,0);
        fun elm -> (0,elm);
        fun elm -> (0,-elm)
        fun elm -> (elm,elm); 
        fun elm -> (-elm,elm);
        fun elm -> (elm,-elm);
        fun elm -> (-elm,-elm)]
    let swap f a b = f b a
    override this.candiateRelativeMoves =
        List.map (swap List.map [1..7]) indToRel
    override this.nameOfType = "queen"

// A rook is a ChessPiece which moves horisontally and vertically
type rook (col : Color) =
    inherit chessPiece(col)
    let indToRel = [
        fun elm -> (elm,0); 
        fun elm -> (-elm,0);
        fun elm -> (0,elm);
        fun elm -> (0,-elm)]
    let swap f a b = f b a
    override this.candiateRelativeMoves =
        List.map (swap List.map [1..7]) indToRel
    override this.nameOfType = "rook"

// A bishop is a ChessPiece which moves diagonally
type bishop (col : Color) =
    inherit chessPiece(col)
    let indToRel = [
        fun elm -> (elm,elm); 
        fun elm -> (-elm,elm);
        fun elm -> (elm,-elm);
        fun elm -> (-elm,-elm)]
    let swap f a b = f b a
    override this.candiateRelativeMoves =
        List.map (swap List.map [1..7]) indToRel
    override this.nameOfType = "bishop"

// A knight is a ChessPiece which moves 2 steps in one direction and one step in the other
type knight (col : Color) =
    inherit chessPiece(col)
    override this.candiateRelativeMoves =
        [[(-1,2)];[(1,2)];[(-1,-2)];[(1,-2)];
        [(2,1)];[(2,-1)];[(-2,1)];[(-2,-1)]]
    override this.nameOfType = "night"
