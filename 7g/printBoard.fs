module Awari

let printBoard (b:board) : unit = 
    let pitsToString acc i = acc + sprintf "%3i" i.beanCount
    let player2String = List.fold pitsToString " " (List.rev b.[7..12])
    let player1String = List.fold pitsToString " " (b.[0..5])
    let homePitString = sprintf "%i%21i" b.[13].beanCount b.[6].beanCount 
    printfn "%s\n%s\n%s" player1String homePutString player2String
