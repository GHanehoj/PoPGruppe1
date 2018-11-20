module Awari
type player = Player1 | Player2
type pit = {
  index : int
  beanCount: int
}
type board = pit list 

let boardSize = 14

// Hjælpefunktion, der udskriver spillepladen.
// Player1 har den øverste række, mens Player2 har den nederste
let printBoard (b:board) : unit = 
    let pitsToString acc i = acc + sprintf "%3i" i.beanCount
    let player2String = List.fold pitsToString " " (List.rev b.[7..12])
    let player1String = List.fold pitsToString " " (b.[0..5])
    let homePitString = sprintf "%i%21i" b.[13].beanCount b.[6].beanCount 
    printfn "%s\n%s\n%s\n" player2String homePitString player1String

// Hjælpefunktion til at bestemme om et bestemt felt er en bestemt spillers hjemmefelt
let isHome (p:player) (i:pit) : bool = 
    let pitPlayer = if i.index < 7 then Player1 else Player2
    (pitPlayer = p) && (i.index%7=6)

// Hjælpefunktion til at bestemme om en brætkonfiguration medfører at spillet er slut
let isGameOver (b:board) : bool =
  let count (x:int,y:int) i:pit : int*int =
    if i.index%7<>6 then
      if i.index >6 then (x + i.beanCount,y)
      else (x,y+i.beanCount)
    else (x,y)
  
  let (p1Beans,p2Beans) = List.fold count (0,0) b
  (p1Beans = 0 || p2Beans = 0)


// Hjæplefunktion til at erstatte feltet i et bestemt index med et givent felt.
let replaceAtIndex (index:int) (ni:pit) (b:board) : board =
    let rep (currentIndex:int) (i:pit) =
        if currentIndex = index then ni else i;
    List.mapi rep b

// Hjælpefunktion til at få hjemmefeltet for en given spiller
let getHome (b:board) (p:player):pit =
    match p with
    |Player1 -> b.[6]
    |Player2 -> b.[13]

// Opdaterer det sidste felt, hvis dette felt kun indeholder 1 bønne, dvs. distribute endte i et tomt felt
// ellers returnerers blot den givne konfiguration af felter 
let updateLastPit (b:board) (p:player) (i:pit) : (board*pit) = 
    if i.beanCount = 1 && i.index % 7 <> 6 then
        let home = getHome b p
        let oppositePit = b.[12 - i.index] 
        let updatedBoard =
            b 
            |> (replaceAtIndex home.index {home with beanCount = home.beanCount + i.beanCount + oppositePit.beanCount })
            |> (replaceAtIndex (oppositePit.index) {oppositePit with beanCount = 0}) 
            |> (replaceAtIndex i.index {i with beanCount = 0}) 
        (updatedBoard,i) 
    else
       (b,i) 


// Hovedfunktionen til at uddele bønnerne på et specifikt felt ud til alle de efterfølgende felter, 
// samt fjerne de oprindelige bønder i startfeltet.
let distribute (b:board) (p:player) (i:pit) : (board*pit) =
    
    // Hjælpefunktion til at bestemme hvordan bønnerne i et givent felt cp ændre sig efter
    // bønnerne i "op" er blevet fordelt.
    // op er den pit som alle bønnerne kommer fra.
    let addBean (op:pit) (cp:pit) : pit =
        let indexDiff = (cp.index - op.index - 1 + boardSize) % boardSize
        
        // Skulle det valgte felt indeholde nok bønder til at komme hele vejen rundt
        // adderes dette tal til alle felter.
        // Herudover sættes op til nu at have 0 bønner
        let beanSum = (if cp <> op then cp.beanCount else 0) + op.beanCount / boardSize
        
        // Hvis der var nok bønner i op til at nå det nuværrende felt,
        // forøges antallet af bønner her med 1.
        if indexDiff < op.beanCount then
            {cp with beanCount = beanSum + 1}
        else
            {cp with beanCount = beanSum}
    
    if i.beanCount = 0 then (b,i)
    else
        let lastIndex = (i.index + i.beanCount) % boardSize 
        let nb = List.map (addBean i) b
        let ni = nb.[lastIndex]
        updateLastPit nb p ni

 
let rec getAiMove (b:board) (p:player) (cIndex:int) (cMax:int) (maxIndex:int)= 
  let (hypoBoard, hypoFinalPit) = distribute b p b.[cIndex]
  let hypoHome = getHome hypoBoard p
  let home = getHome b p
  let homeDiff = hypoHome.beanCount-home.beanCount
  let hypoMax = (if (isHome p hypoFinalPit) then 100 else homeDiff)

  if (cIndex % 7) = 5 then
    if hypoMax > cMax then
      b.[cIndex]
    else
      b.[maxIndex]
  else
    if hypoMax > cMax then
      getAiMove b p (cIndex+1) hypoMax cIndex
    else
      getAiMove b p (cIndex+1) cMax maxIndex


// Funktionen der får et input fra brugeren, samt validerer dette input
let rec getMove (gameType:int) (b:board) (p:player) (s:string) : pit = 
  if (gameType = 1 || (gameType = 2 && p=Player1))then
    printfn "%s" s
    let inputString = System.Console.ReadLine () 
    if not (List.contains inputString ["1";"2";"3";"4";"5";"6"]) then
      do printfn "Incorrect input, try again with one of the pits 1-6"
      getMove gameType b p s  
    else
      let inputPit = int (inputString)
      match p with
      | Player1 -> b.[inputPit-1]
      | Player2 -> b.[inputPit+6]
  else
    match p with
    | Player1 -> getAiMove b p 0 0 0
    | Player2 -> getAiMove b p 7 0 0

// Funktionen der holder styr på turen, dvs om den nuværrende spiller
// skal have en tur til, eller om det er en ny spiller
let turn (gameType : int) (b : board) (p : player) : board =
  let rec repeat (b: board) (p: player) (n: int) : board =
    printBoard b
    let str =
      if n = 0 then
        sprintf "%A's move? " p
      else 
        "Again? "
    let i = getMove gameType b p str
    let (newB, finalPit)= distribute b p i
    if not (isHome p finalPit) 
       || (isGameOver b) then
      newB
    else
      repeat newB p (n + 1)
  repeat b p 0 

// Funktionen der skifter til ny spiller, skulle den gamle
// ikke længere have sin tur.
let rec play (gameType : int) (b : board) (p : player) : board =
  if isGameOver b then
    b
  else
    let newB = turn gameType b p
    let nextP =
      if p = Player1 then
        Player2
      else
        Player1
    play gameType newB nextP
// Starter spillet med standard brætopsætning
let startGame () = 
    let b = List.init 14 (fun x -> {index = x; beanCount = if x % 7 = 6 then 0 else 3})
    let p = Player1

    play 1 b p |> ignore

let startAiGame () = 
    let b = List.init 14 (fun x -> {index = x; beanCount = if x % 7 = 6 then 0 else 3})
    let p = Player1

    play 2 b p |> ignore

//startGame ()
