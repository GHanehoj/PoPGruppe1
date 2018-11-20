#r "awariLib" 
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
    // 1a & 2a 
    printfn "Player 1 Home Test:    %b" (isHome Player1 b.[6] = true)
    // 1b && 2a
    printfn "Player 2 Home Test:    %b" (isHome Player2 b.[13] = true)
    // 2b
    printfn "Wrong Home a:          %b" (isHome Player1 b.[13] = false)
    // 2c
    printfn "No Home:               %b" (isHome Player1 b.[3] = false)

let isGameOverTest () = 
    let ba =  createBoard (fun x -> if x < 6 then 0 else 1)
    // 1a
    printfn "Player 1 No Beans:     %b" (isGameOver ba = true)
    let bb = createBoard (fun x -> if x > 6 then 0 else 1)
    // 1b
    printfn "Player 2 No Beans:     %b" (isGameOver bb = true)
    // 1c
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
    let f (l: int list) x = l.[x]
    
    let listba  = [0;0;0;0;0;1;0;2;1;0;0;0;0;0]
    let listnba = [0;0;0;0;0;0;3;0;1;0;0;0;0;0]
    let ba = createBoard (f listba)
    let nba = createBoard (f listnba)
    printfn "Update last pit empty: %b" (fst (updateLastPit ba Player1 ba.[5]) = nba)
    

    let listbb  = [0;0;0;0;1;2;0;2;1;0;0;0;0;0]
    let listnbb = [0;0;0;0;1;2;0;2;1;0;0;0;0;0]
    let bb = createBoard (f listbb)
    let nbb = createBoard (f listnbb)
    printfn "Update last pit bean:  %b" (fst (updateLastPit bb Player1 bb.[5]) = nbb) 
let distributeTest () =
    let f (l: int list) x = l.[x]
    
    let listba  = [0;0;0;4;0;1;0;2;1;0;0;0;0;0]
    let listnba = [0;0;0;0;1;2;1;3;1;0;0;0;0;0]
    let ba = createBoard (f listba)
    let nba = createBoard (f listnba)
    printfn "Distribute non-empty:  %b" (fst (distribute ba Player1 ba.[3]) = nba)
    

    let listbb  = [0;0;0;0;1;2;0;2;1;0;0;0;0;0]
    let listnbb = [0;0;0;0;1;2;0;2;1;0;0;0;0;0]
    let bb = createBoard (f listbb)
    let nbb = createBoard (f listnbb)
    printfn "Distribute empty:      %b" (fst (distribute bb Player1 bb.[1]) = nbb) 

do isHomeTest ()
do isGameOverTest ()
do getHomeTest ()
do replaceAtIndexTest ()
do updateLastPitTest ()
do distributeTest ()
