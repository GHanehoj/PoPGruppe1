#r 
open Awari
// Ikke alle funktioner bliver testet, da funktionerne, 
// der skaber interaktion med brugeren har sideeffekter, 
// der er svære at tjekke fra programmets side, uden at
// ændre i koden.

// Funktion til at hjælpe med bræt konstruktion
let createBoard f =
   List.init 14 (fun x -> {index = x; beanCount = f x})



let isHomeTest () =
    let b = createBoard id 
    printfn "Player 1 Home Test:    %b" (isHome Player1 b.[6] = true)
    printfn "Player 2 Home Test:    %b" (isHome Player2 b.[13] = true)
    printfn "No Home Test:          %b" (isHome Player1 b.[3] = false)

let isGameOverTest () =
    printfn "Player 1 No Beans:     %b" (isGameOver (createBoard (fun x -> if x < 6 then 0 else 1)) = true)
    printfn "Player 2 No Beans:     %b" (isGameOver (createBoard (fun x -> if x > 6 then 0 else 1)) = true)
    printfn "Not game over:         %b" (isGameOver (createBoard id) = false)

let replaceAtIndexTest () = 
    let z x = 0
    let f x = if x = 3 then 5 else 0
    let b = createBoard z
    let nb = createBoard f
    printfn "Replace at board:      %b" (replaceAtIndex 3 {beanCount = 5; index = 3} b = nb) 

let getHomeTest () =
    let b = createBoard id
    printfn "Player 1:              %b" (getHome b Player1 = b.[6])
    printfn "Player 2:              %b" (getHome b Player2 = b.[13])

let updateLastPitTest () =
    let ba  = [0;0;0;0;0;1;0;2;1;0;0;0;0;0]
    let nba = [0;0;0;0;0;0;3;0;1;0;0;0;0;0]
    printfn "Update last pit empty: %b" (updateLastPitTest ba Player1 ba.[5] = nba)

    let bb  = [0;0;0;0;1;2;0;2;1;0;0;0;0;0]
    let nbb = [0;0;0;0;1;2;0;2;1;0;0;0;0;0]
    printfn "Update last pit bean:  %b" (updateLastPitTest bb Player1 ba.[5] = nbb)
let distributeTest () =
    0

do isHomeTest ()
do isGameOverTest ()
do getHomeTest ()
do replaceAtIndexTest ()
do updateLastPitTest ()
