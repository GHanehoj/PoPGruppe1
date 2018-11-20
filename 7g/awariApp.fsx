#r "awariLib"
open Awari

let rec displayMenu () =
    printfn "Welcome to Awari! \nPlease choose your gamemode (1/2)"
    printfn "1: Player vs. Player"
    printfn "2: Player vs. Ai"
    let input = System.Console.ReadLine()
    match input with
    | "1" -> startGame ()
    | "2" -> startAiGame ()
    | _   -> printfn "Please try again\n" 
             displayMenu ()


displayMenu ()