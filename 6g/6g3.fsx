let cfrac2frac (lst: int list) (i:int) : (int*int) = 
    let rec f (lst: int list) (i:int) : (int*int) =
        // For at undgå underflow fejl tilføjes 2 sentinel elementer til slutningen (bruges aldrig)
        //printfn "%A %d" lst i
        match i with
        | -1 -> (1,0)
        | -2 -> (0,1)
        | _  ->
            let (tim1,nim1) = f lst.Tail (i-1)
            let (tim2,nim2) = f lst.Tail.Tail (i - 2)
            let t = lst.Head * tim1 + tim2
            let n = lst.Head * nim1 + nim2
            (t,n)
    // Vender listen om og fjerner et antal elementer fra starten,
    // i tilfælde af aproximationen ikke skal være fuldstændig.
    let revLst = List.rev lst.[..i]
    let s = revLst @ [0;0]
    f s i
 
let k = [3;4;12;4]
printfn "%A" (cfrac2frac k 3)
