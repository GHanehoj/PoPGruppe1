module Game
#load "player.fsx"
open Chess
open Player

type Game () = 

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
            this.run (p1, p2)
        elif gameMode = "2" then
            let p1 = Human(brd, White)
            let p2 = Human(brd, Black)
            this.run (p1, p2)
        else
            this.start ()

    member this.run (p1 : Player, p2 : Player) =
        let mutable gameOver = false 

        while not gameOver do 
            if not gameOver then
                printfn "Player1:"
                let p1Action = p1.nextMove() 
                if p1Action = "quit" then
                    printfn "Player1 has resigned."
                    gameOver <- true 
                else 
                    // Execute p1 action 
                    ()

            if not gameOver then
                printfn "Player2:"
                let p2Action = p2.nextMove()
                if p2Action = "quit" then
                    printfn "Player2 has resigned."
                    gameOver <- true
                else 
                    // Execute p2 action
                    ()
        

