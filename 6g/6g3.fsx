let cfrac2frac (lst: int list) (i:int) : (int*int) = 
    let rec f (lst: int list) (i:int) : (int*int) =
        // For at undgå underflow fejl tilføjes 2 sentinel elementer til slutningen (bruges aldrig)
        let sentinelLst = lst @ [0;0]
        if i = -1 then
            (1,0)
        elif i = -2 then
            (0,1)
        else
            let t = sentinelLst.Head*(fst (f sentinelLst.Tail (i-1))) + fst((f sentinelLst.Tail.Tail (i-2)))
            let n = sentinelLst.Head*(snd(f sentinelLst.Tail (i-1))) + (snd(f sentinelLst.Tail.Tail (i-2)))
            (t,n)
    // Vender listen om og fjerner et antal elementer fra starten,
    // i tilfælde af aproximationen ikke skal være fuldstændig.
    let revLst = (List.rev lst).[lst.Length-(i+1)..]
    f revLst i
 
let k = [3;4;12;4]
printfn "%A" (cfrac2frac k 3)