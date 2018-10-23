let rec frac2cfrac (t:int) (n:int) : (int list) =
    let q = t/n
    let rest = t%n
    if rest = 0 then
        [q]
    else
        q::frac2cfrac n rest

let t=649
let n=200
printfn "%A" (frac2cfrac t n)