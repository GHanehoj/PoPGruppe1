module A
open System
type player = Player1 | Player2
type pit = {
  index : int
  beanCount: int
}
type board = pit list

let isGameOver (b:board) : Boolean =
  let mutable player1Beans = 0
  let mutable player2Beans = 0
  for pit in b do
    if pit.index%7<>6 then
      if pit.index >6 then player1Beans <- player1Beans + pit.beanCount
      if pit.index <6 then player2Beans <- player2Beans + pit.beanCount
  (player1Beans = 0) || (player2Beans = 0)
