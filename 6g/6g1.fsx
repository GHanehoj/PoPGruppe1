let rec float2cfrac (x:float) : int list =
    let q = int(x+10e-6)
    let rest = x-float(q)
    if abs rest < 10e-6 then
        [q]
    else
        q::float2cfrac(1.0/rest)

let k = 3.245
printfn "%A" (float2cfrac k)
