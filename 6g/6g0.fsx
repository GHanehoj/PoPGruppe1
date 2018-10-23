let rec cfrac2float lst =
  if List.isEmpty (List.tail lst) then
    float (List.head lst)
  else
    float (List.head lst) +  (1.0 / cfrac2float (List.tail lst))

printfn "%f" (cfrac2float [0;3;4;12;3;1])
