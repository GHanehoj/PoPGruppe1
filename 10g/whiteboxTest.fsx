#r "WolfMoose.dll"
open Animal
open Wolf
open Moose
open Board
open System
open Environment

printfn "White-box Test"

printfn ""
printfn "Test af Board:"
printfn "Initialiserer et board med størrelsen 10 ved følgende:"
printfn "let brd = Board<Animal> 10"
let brd = Board<Animal> 10

printfn ""
printfn "Test af brd.size():"
printfn "Forventer output 10: %A" (brd.size = 10)

printfn ""
printfn "Test af brd.insert() og brd.getContent():"
printfn "Der initieres nu en wolf og en moose ved følgende: "
printfn "let wlf = Wolf ((2,2), 10, 10, brd)"
printfn "let moose = Moose ((3,3), 10, brd)"
printfn "Herefter kaldes brd.insert wlf og brd.insert moose"
printfn "Herefter benyttes brd.getContent() til at tjekke om disse blev indsat korrekt."
let wlf = Wolf ((2,2), 10, 10, brd)
let moose = Moose ((3,3), 10, brd)
brd.insert wlf
brd.insert moose
let content = brd.getContent
printfn "brd.getContent, forventer output [wlf; moose] eller [moose; wlf]: %A" 
    (content = [wlf; moose] || content = [moose; wlf])

printfn ""
printfn "Test af Moose: "
printfn "Test af moose.nearbyWolves(): "
printfn "Forventer Seq.head (moose.nearbyWolves) = wlf.pos: %A" 
    (Seq.head (moose.nearbyWolves ()) = (wlf.pos))

printfn ""
printfn "Test af moose.takeTurn() og moose.distanceTo(): "
printfn "Forventer under prioriteringen et kald til moose.flee()."
printfn "moose.takeTurn kaldes "
do moose.takeTurn ()
printfn "Forventer nu at mooose.distanceTo (moose.pos) (wlf.pos) = 2: %A" 
    ((moose.distanceTo (moose.pos) (wlf.pos)) = 2)


printfn ""
printfn "Test af Wolf: "
printfn "Test af wlf.nearbyMoose(): "
printfn "Forventer Seq.head (wlf.nearbyMoose ()) = moose.pos: %A" 
    (Seq.head (wlf.nearbyMoose ()) = (moose.pos)) 

printfn ""
printfn "Test af wlf.takeTurn():"
printfn "Forventer et kald til wlf.huntMoose hvilket burde sætte wlf.pos = (3,3), 
    da moose.pos = (4,4)."
do wlf.takeTurn ()
printfn "Forventer wlf.pos = (3,3): %A" (wlf.pos = (3,3))

printfn ""
printfn "Test af moose.die(): "
printfn "Der kaldes nu igen wlf.takeTurn() og der forventes nu under 
    prioritize at Eat(4,4) vælges."
do wlf.takeTurn ()
printfn "Nu forventes moose.die() at være kaldet og derfor forventes følgende: "
printfn "Forventer moose.alive = false: %A" (moose.alive = false)
printfn "Forventer brd.getContent = [wlf]: %A" (brd.getContent = [wlf])
