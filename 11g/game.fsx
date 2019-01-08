module Game
#load "player.fsx"
open Chess
open Player
open Pieces
type Game () = 
    let run (brd : Board, p1 : Player, p2 : Player) =
        let mutable gameOver = false 
        let mutable currentPlayer = p1
        printfn "%A" brd

        // Game loop
        while not gameOver do 
            printfn "%A:" currentPlayer
            let playerAction = currentPlayer.nextMove() 
            if playerAction = "quit" then
                printfn "%A has resigned." currentPlayer
                gameOver <- true 
            else 
                let origin = brd.code2Pos playerAction.[0..1]
                let target = brd.code2Pos playerAction.[2..3]
                brd.move origin.Value target.Value

            printfn "%A" brd
            if currentPlayer = p1 then
                currentPlayer <- p2
            else 
                currentPlayer <- p1

    member private this.init () =
        let brd = Board () 
        let mirror (x,y) = [(White,x,y);(Black,7-x,y)]
        let doubleMirror (x,y) =
            [(White,x,y); (White,x,7-y); (Black,7-x,y); (Black,7-x,7-y)]
        let cons (c : chessPiece) = c
        let pieces = [
            (knight >> cons,doubleMirror (0,1));
            (rook >> cons, doubleMirror (0,0));
            (bishop >> cons,doubleMirror (0,2));
            (king >> cons,mirror (0,3));
            (queen >> cons, mirror (0,4))
        ]
        let create (f,lst) =
            let it (c,x,y) =
                brd.[x,y] <- Some (f c)
            List.iter it lst
        do List.iter create pieces
        brd
    member this.start () = 
        let brd = this.init ()
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
