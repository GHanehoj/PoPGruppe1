module Board
open System
type Board<'a when 'a : equality >(boardSize:int) =
    let mutable _lst : 'a list = []
    member this.size = boardSize
    member this.delete (element:'a) = 
        _lst <- List.filter (fun x -> x <> element) _lst
    member this.getContent :'a list =  
        let rnd = Random()
        // As there is no good shuffle of list in the standarb library
        // This function simply sorts with a random key for each element
        List.sortBy (fun _ -> rnd.Next()) _lst
    member this.insert (element:'a) = 
        _lst <- element::_lst
