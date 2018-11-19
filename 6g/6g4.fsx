//Funktionerne indskrives igen, så de forskellige tests kan tilgå dem.
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
printfn "Whitebox test:\nHver funktion afprøves med et input som sikrer at vi kommer igennem alle forgreninger i koden.\nPå hver linje ses hvorvidt programmerne returnerer den forventede værdi (true/false)\n"
// Da alle branches køres igennem ved hver kørsel af funktionen, er det kun 
// nødvendigt at bruge en enkelt test-case: [3;4;12;4]
printfn "cfrac2float:"
printfn "cfrac2float [3;4;12;4] = 3.245 - %A\n" ((cfrac2float [3;4;12;4]) = 3.245)

// Da alle branches køres igennem ved hver kørsel af funktionen, er det kun 
// nødvendigt at bruge en enkelt test-case: 3.245
printfn "float2cfrac:"
printfn "float2cfrac 3.245 = [3;4;12;4] - %A\n"  ((float2cfrac 3.245) = [3;4;12;4])

// Da alle branches køres igennem ved hver kørsel af funktionen, er det kun 
// nødvendigt at bruge en enkelt test-case: (649,200)
printfn "frac2cfrac:"
printfn "frac2cfrac 649 200 = [3;4;12;4] - %A\n" ((frac2cfrac 649 200) = [3;4;12;4])

// Da alle branches køres igennem ved hver kørsel af funktionen, er det kun 
// nødvendigt at bruge en enkelt test-case: [3;4;12;4]
printfn "cfrac2frac:"
printfn "cfrac2frac [3;4;12;4] = (649, 200) - %A\n" ((cfrac2frac [3;4;12;4] 3) = (649,200))

// Blackbox test
printfn "Blackbox test:\nHver funktion er afprøvet med forskellige input - Edge-case og tilfældigt.\nPå hver linje ses hvorvidt programmerne returnerer den forventede værdi (true/false)\n"

// Funktionen kan tage en ikke-tom liste af ints. Vi prøver derfor med følgende input:
// Tilfældig liste, liste med 0, liste med mange elementer, liste med store tal, liste med 0 som sidste element
printfn "cfrac2float:"
printfn "1: cfrac2float [3;4;12;4] = 3.245 - %A"((cfrac2float [3;4;12;4]) = 3.245)
printfn "2: cfrac2float [5;0;2] = 7 - %A" ((cfrac2float [5;0;2]) = 7.0)
printfn "3: cfrac2float [3;1;1;1;1] = 3.6 - %A" ((cfrac2float [3;1;1;1;1]) = 3.6)
printfn "4: cfrac2float [5;1246] = 5.000802568218298 - %A" ((abs((cfrac2float [5;1246]) - 5.000802568218298)) < 10e-6)
printfn "5: cfrac2float [1;0] = Infinity - %A\n" ((cfrac2float [1;0]) = infinity)
// Det kan af resultatet ses, at funktionen beregner den rigtige værdi for de fire første eḱsempler. Den femte kædebrøk
// vil evaluere til noget der indeholder division med 0, og er derfor ikke defineret. F# evaluerer dog 1/0 til 'infinite',
// hvilket også er hvad vores funktion returnerer.


// Funktionen kan tage en float. Vi prøver derfor med følgende input:
// 0, tilfældigt tal, edgecase (tæt på 4), 4, stort tal, lavt tal, MEGET lavt tal.
printfn "float2cfrac:"
printfn "1: float2cfrac 0.0 = [0] - %A" ((float2cfrac 0.0) = [0])
printfn "2: float2cfrac 3.245 = [3;4;12;4] - %A" ((float2cfrac 3.245) = [3;4;12;4])
printfn "3: float2cfrac 3.999 = [3;1;999] - %A" ((float2cfrac 3.999) = [3;1;999])
printfn "4: float2cfrac 4.0 = [4] - %A" ((float2cfrac 4.0) = [4])
printfn "5: float2cfrac 15794.0 = [15794] - %A" ((float2cfrac 15794.0) = [15794])
printfn "6: float2cfrac 0.00004 = [0;25000] - %A" ((float2cfrac 0.00004) = [0;25000])
printfn "7: float2cfrac 0.000004 = [0] - %A\n" ((float2cfrac 0.000004) = [0])
// I de første 2 eksempler kan man se at funktionen ummidelbart beregner det korrekte resultat. Eksempel 3 og 4 viser,
// at funktionen kan skelne korrekt mellem et edgecase mellem 4 og 3.999. Eksempel 5 viser at funktionen håndterer store
// tal korrekt. Eksempel 6 beregner, korrekt, kædebrøken for et lille tal, hvorimod eksempel 7 returnerer [0], da inputtet er så lavt
// at det bliver rundet ned da det er under præcisionen af floats (10e-6).


// Funktionen tager to heltal. Vi prøver derfor med følgende input:
// 0 & tilfældigt tal, 2 tilfældige tal, 2 store tal, stort og lille tal, lille og stort tal, tilfældigt tal & 0.
printfn "frac2cfrac:"
printfn "1: frac2cfrac 0 3 = [0] - %A" ((frac2cfrac 0 3) = [0])
printfn "2: frac2cfrac 10 3 = [3;3] - %A" ((frac2cfrac 10 3) = [3;3])
printfn "3: frac2cfrac 649 200 = [3;4;12;4] - %A" ((frac2cfrac 649 200) = [3;4;12;4])
printfn "4: frac2cfrac 649 4 = [162;4] - %A" ((frac2cfrac 649 4) = [162;4])
printfn "5: frac2cfrac 2 245 = [0;122;2] - %A\n" ((frac2cfrac 2 245) = [0;122;2])
// I første de 5 eksempler ovenfor ses det, at alle de forskellige input giver det korrekte resultat.
// Vi har ikke testet resultatet ved division med 0, da der bruges ints i funkitonen, hvilket giver en exeption
// når man dividerer med 0. Da funktionen ikke er defineret ved input med 0 som 2. tal, gør dette ikke noget.


// Funktion tager en ikke-tom liste af heltal samt et heltal. Vi prøver derfor med følgende input: 
// Tilfældig liste af længde 4 & 3, tilfældig liste af længde 4 & 1, liste med 0 af længe 3 & 2,
// en liste med 1 element & 0, en liste med mange elementer og 2, en liste med 3 store tal & 2.
printfn "cfrac2frac:"
printfn "1: cfrac2frac [3;4;12;4] 3 = (649, 200) - %A" ((cfrac2frac [3;4;12;4] 3) = (649,200))
printfn "2: cfrac2frac [3;4;12;4] 1 = (13, 4) - %A" ((cfrac2frac [3;4;12;4] 1) = (13,4))
printfn "3: cfrac2frac [5;0;2] 2 = (7, 1) - %A" ((cfrac2frac [5;0;2] 2) = (7,1))
printfn "4: cfrac2frac [4] 0 = (4, 1) - %A" ((cfrac2frac [4] 0) = (4,1))
printfn "5: cfrac2frac [1;2;3;4;5;6;7] 6 = (10, 7) - %A" ((cfrac2frac [1;2;3;4;5;6;7] 6) = (9976,6961))
printfn "6: cfrac2frac [1;2;3;4;5;6;7] 2 = (10, 7) - %A" ((cfrac2frac [1;2;3;4;5;6;7] 2) = (10,7))
printfn "7: cfrac2frac [534;234;124] 2 = (15495202, 29017) - %A" ((cfrac2frac [534;234;124] 2) = (15495202, 29017))
// Det kan ses, at alle eksemplerne evaluerer til den korrekte værdi, og funktionen fungerer således