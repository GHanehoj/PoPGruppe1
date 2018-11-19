type player = Player1 | Player2
type pit = {
  index : int
  beanCount: int
}
type board = pit list

let isGameOver (b:board) : bool =
  let count (x:int,y:int) i:pit : int*int =
    if i.index%7<>6 then
      if i.index >6 then (x + i.beanCount,y)
      else (x,y+i.beanCount)
    else (x,y)
  
  let (p1Beans,p2Beans) = List.fold count (0,0) b
  (p1Beans = 0 || p2Beans = 0)
