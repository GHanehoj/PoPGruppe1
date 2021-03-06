(*
    For at omregne en kædebrøk til det tilsvarende reelle tal, kan formlen for kædebrøkers værdi
    benyttes: q1+1/(q2+1/(q3+...)). Den nemmeste måde at implementere dette er ved en rekursiv funktion,
    som hvis listen kun har et element returnerer værdien, og ellers kalder sig selv på det resterende,
    og lægger det reciprokke af resultatet til den nuværende q-værdi.
*)

// Tager en kædebrøk og returnerer det reelle tal, som kædebrøken repræsenterer.
let rec cfrac2float (lst: int list) : float =
  match lst with
    | h::[] -> float h
    | h::t ->  float h +  (1.0 / cfrac2float t)
    | _ -> failwith "Empty list" // Dette er ikke en valid struktur, og er kun tilstede for at gøre compileren glad

// For at teste om koden virker, bruger vi funktionen på kædebrøken [3;4;12;4].
// Det forventede resultat er 3.245, hvilket også er hvad vi får.
printfn "%f" (cfrac2float [])
