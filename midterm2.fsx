
let zero = fun f -> (fun x -> x)

let one = fun f -> (fun x -> (f x))

let showcn cn = (cn (fun n -> n + 1)) 0

let r1 = showcn one

//printfn "%i", (r1)
//Q1

let rec repeat f n =
    if n=0 then fun x -> x
    else fun x -> f (repeat f (n-1) x)

(*
let repeat f n =
  let rec helper f n =
    if n=0 then (fun x -> x)
      else (fun x -> f ((helper f (n-1)) x))
  helper f n
*)
//Q2
let rec bits n =
  if n=0 then [[]]
  elif n=1 then [[0]; [1]]
  else
    let prev = bits (n-1)
    let l1 = List.map (fun l -> 0::l) prev
    let l2 = List.map (fun l -> 1::l) prev
    l1 @ l2


//test Q1
let inc n = n+1
let plusfive = repeat inc 5
printfn "%i" (plusfive 3)

let never = repeat inc 0
printfn "%i" (never 3)

//test Q2
//printfn "%A" (bits 0)

//printfn "%A" (bits 1)

//printfn "%A" (bits 2)

//printfn "%A" (bits 3)


let x = 3 in
    let f = 
        let x = x + 1 in 
           fun y -> x + y in
              f x;;






let rec iter_sum(f, lo:float, hi:float, inc) =
    let rec helper(x:float, result:float) =
        if (x > hi) then result
        else helper(inc(x), f(x) + result)
    helper(lo,0.0)

let delta (x:float) = x+0.001
iter_sum(sin,(0.0 + (0.001/2.0)), 3.14159, delta)

let integral(f,lo:float,hi:float,dx:float) =
    let delta (x:float) = x+dx
    dx * iter_sum(f,(lo + (dx/2.0)), hi, delta)

let r_sq (x:float):float = x * x

integral(r_sq,0.0,1.0,0.001)

integral(sin,0.0, 3.14159, 0.001)