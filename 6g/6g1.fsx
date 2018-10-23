(*
    For at konvertere et reelt tal til en kædebrøk bruger vi algoritmen beskrevet
    i opgavearket. Hver iteration findes heltalskomponenten af x, 'q', samt resten, 'rest'.
    Hvis resten er 0 er algoritmen færdig, og det bagerste element skal være q-værdien
    fra den iteration. Ellers kaldes float2cfrac igen, og den nuværende q-værdi sættes foran resultatet.
*)

// Tager en float og returnerer en liste af ints, som repræsenterer kædebrøken
let rec float2cfrac (x:float) : int list =
    let q = int(x+10e-6)
    let rest = x-float(q)
    match abs rest with
    | x when x < 10e-6 -> [q]
    | _ -> q::float2cfrac(1.0/rest)


// For at teste at funktionen virker, bruger vi den på tallet 3.245, da dette burde returnere
// [3;4;12;4] - hvilket vi også ser den gør
let k = 3.245
printfn "%A" (float2cfrac k)
