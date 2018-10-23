(*
    Her ønsker vi at konvertere en heltalsbrøk til en kædebrøk. Dette kan igen implementeres
    ved at bruge algoritmen fra opgave-arket. Dette er en modificeret version af Euclids algoritme.
*)

// Tager 2 heltal, som repræsenterer hhv. tæller og nævner af en brøk, og returnerer den tilhørende kædebrøk.
let rec frac2cfrac (t:int) (n:int) : (int list) =
    let q = t/n
    let rest = t%n
    match rest with
    | 0 -> [q]
    | _ -> q::frac2cfrac n rest


// For at teste at vores program virker som det skal afprøver vi funktionen på brøken 649/200.
// Det korrekte resultat ([3;4;12;4]) returneres.
let t=649
let n=200
printfn "%A" (frac2cfrac t n)
