// Tager en liste af kædebrøker, samt et i fortællende hvor præcis en approximation
// retur værdien. Dette vil være t_i/n_i, returneret som en tuple (t_i,n_i)
let cfrac2frac (lst: int list) (i:int) : (int*int) = 
    let rec f (lst: int list) : (int*int) =
        match lst with
        | [] -> (1,0)
        | h::[] -> (h,1)
        | (h::m::t) ->
            let (tim1,nim1) = f (m::t)
            let (tim2,nim2) = f t
            let t = h * tim1 + tim2
            let n = h * nim1 + nim2
            (t,n)
    // Vender listen om og fjerner et antal elementer fra starten,
    // i tilfælde af at aproximationen ikke skal være fuldstændig.
    let revLst = List.rev (lst.[..i])
    
    f revLst
 
let k = [3;4;12;4]
printfn "%A" (cfrac2frac k 0)
