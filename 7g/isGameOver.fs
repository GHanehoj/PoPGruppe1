module Awari
let isGameOver (b:board) : Boolean =
  let mutable player1Beans = 0
  let mutable player2Beans = 0
  for pit in b.pits do
    if not pit.homePit then
      if pit.side = Player1 then player1Beans <- player1Beans + pit.beanCount
      if pit.side = Player2 then player2Beans <- player2Beans + pit.beanCount
  (player1Beans = 0) || (player2Beans = 0)
