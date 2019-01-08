module Game
#load "player.fsx"
open Chess
open Player

type Game () = 
    let run (brd : Board, p1 : Player, p2 : Player) =
        let mutable gameOver = false 
        let mutable currentPlayer = p2
        
        //Initiate board here
        
        printfn "%A" brd

        // Game loop
        while not gameOver do 
            printfn "%A:" currentPlayer
            let playerAction = currentPlayer.nextMove() 
            if playerAction = "quit" then
                printfn "%A has resigned." currentPlayer
                gameOver <- true 
            else 
                // Execute action 
                ()

            printfn "%A" brd
            if currentPlayer = p1 then
                currentPlayer <- p2
            else 
                currentPlayer <- p1

    member this.start () = 
        let brd = Board () 

        let gameMode =
            printfn "1: Player vs Player"
            printfn "2: Player vs Computer"
            printf "Choose Gamemode: "  
            System.Console.ReadLine()

        if gameMode = "1" then 
            let p1 = Human(brd, White)
            let p2 = Human(brd, Black) 
            run (brd, p1, p2)
        elif gameMode = "2" then
            let p1 = Human(brd, White)
            let p2 = Human(brd, Black)
            run (brd, p1, p2)
        else
            this.start ()