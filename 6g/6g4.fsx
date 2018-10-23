//6g0
let rec cfrac2float (lst:int list) : float =
  match lst with
    | h::[] -> float h
    | h::t ->  float h +  (1.0 / cfrac2float t)
    | _ -> 0.0

//6g1
let rec float2cfrac (x:float) : int list =
    let q = int(x+10e-6)
    let rest = x-float(q)
    match abs rest with
    | x when x < 10e-6 -> [q]
    | _ -> q::float2cfrac(1.0/rest)

//6g2
let rec frac2cfrac (t:int) (n:int) : (int list) =
    let q = t/n
    let rest = t%n
    match rest with
    | 0 -> [q]
    | _ -> q::frac2cfrac n rest

//6g3
let cfrac2frac (lst: int list) (i:int) : (int*int) = 
    let rec f (lst: int list) : (int*int) =
        match lst with
        | [] -> (1,0)
        | h::[] -> (h,1)
        | h::m::t ->
            let (tim1,nim1) = f (m::t)
            let (tim2,nim2) = f t
            let t = h * tim1 + tim2
            let n = h * nim1 + nim2
            (t,n)
    let revLst = List.rev (lst.[..i])
    
    f revLst

// Whitebox test
printfn "Whitebox test\n"
// Da alle branches køres igennem ved hver kørsel af funktionen, er det kun 
// nødvendigt at bruge en enkelt test-case: [3;4;12;4]
printfn "cfrac2float [3;4;12;4] = 3.245 - %A\n" ((cfrac2float [3;4;12;4]) = 3.245)

// Da alle branches køres igennem ved hver kørsel af funktionen, er det kun 
// nødvendigt at bruge en enkelt test-case: 3.245
printfn "float2cfrac 3.245 = [3;4;12;4] - %A\n"  ((float2cfrac 3.245) = [3;4;12;4])


// Blackbox test
printfn "Blackbox test\n"
// Funktionen kan tage en ikke-tom liste af ints. Vi prøver derfor med følgende input: [3,4,12,4], [5;0;2]
printfn "cfrac2float:"
printfn "1: cfrac2float [3;4;12;4] = 3.245 - %A\n2: cfrac2float [5;0;2] = 7 - %A\n" ((cfrac2float [3;4;12;4]) = 3.245) ((cfrac2float [5;0;2]) = 7.0)

// Funktionen kan tage en float. Vi prøver derfor med følgende input: 0.0, 3.245
printfn "float2cfrac:"
printfn "1: float2cfrac 0 = [0] - %A\n2: float2cfrac 3.245 = [3;4;12;4] - %A\n" ((float2cfrac 0.0) = [0]) ((float2cfrac 3.245) = [3;4;12;4])

// Funktionen tager to heltal. Af matematiske årsager kan det sidste tal ikke være 0, da dette giver division med 0.
// Vi prøver derfor med følgende input: (0,3), (10,3), (649,200)
printfn "frac2cfrac:"
printfn "1: frac2cfrac 0 3 = [0] - %A\n2: frac2cfrac 10 3 = [3;3] - %A\n3: frac2cfrac 649 200 = [3;4;12;4] - %A\n" ((frac2cfrac 0 3) = [0]) ((frac2cfrac 10 3) = [3;3]) ((frac2cfrac 649 200) = [3;4;12;4])

// Funktion tager en ikke-tom liste af heltal. Vi prøver derfor med følgende input: [3,4,12,4], [5;0;2]
printfn "cfrac2frac:"
printfn "1: cfrac2frac [3;4;12;4] = (649, 200) - %A\n2: cfrac2frac [5;0;2] = (7, 1) - %A" ((cfrac2frac [3;4;12;4] 3) = (649,200)) ((cfrac2frac [5;0;2] 2) = (7,1))

