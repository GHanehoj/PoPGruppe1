module Environment
open Animal
open Wolf
open Moose
open Board
open System
type Environment(time : int, filename:string, boardSize : int, startMooseCount:int, mooseRepTime:int, startWolfCount:int, wolfRepTime:int, wolfFeedTime:int) =
    member val brd = Board<Animal>() with get,set  
    member this.print() = 
        let cords : Animal option[,] = Array2D.init boardSize boardSize (fun _ _ -> None)
        let content = this.brd.getContent
        
        let insert (a : Animal) = 
                let (x,y) = a.pos
                cords.[x,y] <- Some a
        let printAnimal (op : Animal option) = 
            match op with
            | Some a -> printf "%3s" a.represent 
            | None -> printf "%3s" "."
        let iter x y op =
            if x = boardSize - 1 then printf "\n"
            printAnimal op
        Array2D.iteri iter cords
           
        
    member this.startSimulation() =
        this.brd <- Board<Animal>()
        //0. Create board & animals
        let cords = [for x in 1..boardSize do for y in 1..boardSize -> (x,y)]
        let rnd = System.Random()
        let rec rndSelect lst length =
            match lst with
            | h::t when rnd.NextDouble() <= 1.0 / float length -> (h,t)
            | h::t -> 
                let (sel,tl) = rndSelect t (length - 1)
                (sel,h::tl)
            | [] -> failwith "Too many animals compared to tiles"
                    
            
        let rec selcords func lst i = 
            if i > 0 then
               let (cords,newList) = rndSelect lst (List.length lst) 
               func cords
               selcords func newList (i-1)
            else lst
        let createWolf (x,y) = 
            this.brd.insert(Wolf((x,y),wolfRepTime,wolfFeedTime,this.brd))
        let createMoose (x,y) =
            this.brd.insert(Moose((x,y),mooseRepTime,this.brd))
        selcords createWolf (selcords createMoose cords startMooseCount) startWolfCount |> ignore 
        
        let rec loop i =
            if i > 0 then 
                this.print()
                List.iter (fun (a:Animal) -> a.takeTurn()) this.brd.getContent 
                List.iter (fun (a:Animal) -> a.tick()) this.brd.getContent
                loop (i-1)
            else this.print()
        do loop time
