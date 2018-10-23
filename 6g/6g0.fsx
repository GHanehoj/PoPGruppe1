let rec cfrac2float lst =
  match lst with
    | h::[] -> float h
    | h::t ->  float h +  (1.0 / cfrac2float t)
    | _ -> 0.0 // Dette er ikke en valid struktur, og er kun tilstede for at gøre compileren glad
printfn "%f" (cfrac2float [0;3;4;12;3;1])
