module Awari
type player = Player1 | Player2
type pit = {
  index : int
  beanCount: int
}
type board = pit list
let isHome (b:board) (p:player) (i:pit) : bool = 
    let pitPlayer = if i.index < 7 then Player1 else Player2
    (pitPlayer = p) && (i.index%7=6)
