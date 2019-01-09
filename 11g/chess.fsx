module Chess
type Color = White | Black
type Position = int * int
// An abstract chess piece 
[<AbstractClass>]
type chessPiece(color : Color) =
  let mutable _position : Position option = None
  abstract member nameOfType : string // "king", "rook", ...
  member this.color = color // White, Black
  member this.position // E.g., (0,0), (3,4), etc.
    with get() = _position
    and set(pos) = _position <- pos
  override this.ToString () = // E.g. "K" for white king
    match color with
      White -> (string this.nameOfType.[0]).ToUpper ()
      | Black -> (string this.nameOfType.[0]).ToLower ()

  // A list of runs, which is a list of relative movements, e.g.,
  // [[(1,0); (2,0);...]; [(-1,0); (-2,0)]...]. Runs must be
  // ordered such that the first in a list is closest to the piece
  // at hand.      
  abstract member candiateRelativeMoves : Position list list

  // Available moves and neighbours ([(1,0); (2,0);...], [p1; p2])
  member this.availableMoves (board : Board) (firstCall : bool) : (Position list * chessPiece list) =
    // Helper function to check if a board-state is valid (king isnt vulnurable).
    let checkValid (brd : Board) : bool =
      let mutable valid = true
      for i=0 to 7 do
        for j=0 to 7 do
          let (pieceOption : chessPiece option) = brd.[i,j]
          match pieceOption with
          | Some piece ->
            let (pieceMoves, pieceAttacks) = piece.availableMoves brd false
            if List.exists (fun (elm:chessPiece) -> elm.color = this.color && elm.nameOfType = "king") pieceAttacks then
              valid <- false
          | _ -> ()
      valid

    let mutable (availMoves, availAttacks) = board.getVacantNNeighbours this
    
    if firstCall then
      for move in availMoves do
        let hypoBoard = board.Clone()
        hypoBoard.move this.position.Value move
        if not (checkValid hypoBoard) then
          availMoves <- List.filter (fun elm -> elm <> move) availMoves
        board.reset()
      for target in availAttacks do
        let hypoBoard = board.Clone()
        hypoBoard.move this.position.Value target.position.Value
        if not (checkValid hypoBoard) then
          availAttacks <- List.filter (fun elm -> elm <> target) availAttacks
        board.reset()
    (availMoves,availAttacks)

// A board
and Board () =
  let _array = Collections.Array2D.create<chessPiece option> 8 8 None

  // Wrap a position as option type
  let validPositionWrap (pos : Position) : Position option =
    let (rank, file) = pos // square coordinate
    if rank < 0 || rank > 7 || file < 0 || file > 7 
    then None
    else Some (rank, file)

  // Convert relative coordinates to absolute and remove out
  // of board coordinates.
  let relativeToAbsolute (pos : Position) (lst : Position list) : Position list =
    let addPair (a : int, b : int) (c : int, d : int) : Position = 
      (a+c,b+d)
    // Add origin and delta positions
    List.map (addPair pos) lst
    // Choose absolute positions that are on the board
    |> List.choose validPositionWrap

  let codeChar2Int (codeChar : char) (coordAxis : int) : int option =
    match int codeChar with
    | x when x-(int 'a')>=0 && x-(int 'a')<8 && coordAxis=0 -> Some (x-(int 'a'))
    | x when x-(int '1')>=0 && x-(int '1')<8 && coordAxis=1 -> Some (x-(int '1'))
    | _ -> None

  // Board is indexed using .[,] notation
  member this.Item
    with get(a : int, b : int) = _array.[a, b]
    and set(a : int, b : int) (p : chessPiece option) = 
      if p.IsSome then p.Value.position <- Some (a,b)
      _array.[a, b] <- p

  // Produce string of board for, e.g., the printfn function.
  override this.ToString() =
    let rec boardStr (i : int) (j : int) : string =
      match (i,j) with 
        (8,0) -> ""
        | _ ->
          let stripOption (p : chessPiece option) : string = 
            match p with
              None -> ""
              | Some p -> p.ToString()
          // print top to bottom row
          let pieceStr = stripOption _array.[7-i,j]
          //let pieceStr = sprintf "(%d, %d)" i j
          let lineSep = " " + String.replicate (8*4-1) "-"
          match (i,j) with 
          (0,0) -> 
            let str = sprintf "%s\n| %1s " lineSep pieceStr
            str + boardStr 0 1
          | (i,7) -> 
            let str = sprintf "| %1s |\n%s\n" pieceStr lineSep
            str + boardStr (i+1) 0 
          | (i,j) -> 
            let str = sprintf "| %1s " pieceStr
            str + boardStr i (j+1)
    boardStr 0 0

  // Move piece by specifying source and target coordinates
  member this.move (source : Position) (target : Position) : unit =
    this.[fst target, snd target] <- this.[fst source, snd source]
    this.[fst source, snd source] <- None

  // Find the tuple of empty squares and first neighbour if any.
  member this.getVacantNOccupied (run : Position list) : (Position list * (chessPiece option)) =
    try
      // Find index of first non-vacant square of a run
      let idx = List.findIndex (fun (i, j) -> this.[i,j].IsSome) run
      let (i,j) = run.[idx]
      let piece = this.[i, j] // The first non-vacant neighbour
      if idx = 0
      then ([], piece)
      else (run.[..(idx-1)], piece)
    with
      _ -> (run, None) // outside the board

  // Find the list of all empty squares and list of neighbours
  member this.getVacantNNeighbours (piece : chessPiece) : (Position list * chessPiece list)  =
    match piece.position with
      None -> 
        ([],[])
      | Some p ->
        let convertNWrap = 
          (relativeToAbsolute p) >> this.getVacantNOccupied
        let vacantPieceLists = List.map convertNWrap piece.candiateRelativeMoves
        // Extract and merge lists of vacant squares
        let vacant = List.collect fst vacantPieceLists
        // Extract and merge lists of first obstruction pieces and filter out own pieces
        let opponent = vacantPieceLists |> List.choose snd |> List.filter (fun elm -> elm.color <> piece.color)
        (vacant, opponent)

  member this.Clone() : Board = 
    let returnBoard = Board()
    for i=0 to 7 do
      for j=0 to 7 do
        returnBoard.[i,j] <- this.[i,j]
    returnBoard
  
    member this.reset() =
      for i = 0 to 7 do
          for j = 0 to 7 do
              this.[i,j] <- this.[i,j] // resetting any positions changed by the clone

  member this.code2Pos (code : string) : Position option =
    let mutable (returnVal : Position option) = None
    if code.Length = 2 then
      // swap first and second to fit with the rest of the code
      let fstCoord = codeChar2Int code.[1] 1
      let sndCoord = codeChar2Int code.[0] 0
      if fstCoord.IsSome && sndCoord.IsSome then
        returnVal <- Some (fstCoord.Value, sndCoord.Value)
    returnVal

  member this.init () = 
    ()
