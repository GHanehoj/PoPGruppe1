//6g0
let rec cfrac2float lst =
  match lst with
    | h::[] -> float h
    | h::t ->  float h +  (1.0 / cfrac2float t)
    | _ -> 0.0 // Dette er ikke en valid struktur, og er kun tilstede for at gøre compileren glad
printfn "%f" (cfrac2float [0;3;4;12;3;1])

//6g1
let rec float2cfrac (x:float) : int list =
    let q = int(x+10e-6)
    let rest = x-float(q)
    match abs rest with
    | x when x < 10e-6 -> [q]
    | _ -> q::float2cfrac(1.0/rest)

let k = 3.245
printfn "%A" (float2cfrac k)

//6g2
let rec frac2cfrac (t:int) (n:int) : (int list) =
    let q = t/n
    let rest = t%n
    match rest with
    | 0 -> [q]
    | _ -> q::frac2cfrac n rest

let t=649
let n=200
printfn "%A" (frac2cfrac t n)

//6g3
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
