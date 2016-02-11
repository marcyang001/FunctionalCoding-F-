
let lz1 = [2;5;3;4;1]
let l1 = [1; 3; 2; 5; 4]    

let rec split list = 
    match list with
    | [] -> ([], [])
    | [ var1 ] -> ( [ var1 ] , [])
    | x::y::rs -> 
        let (a, b) = split rs
        (x :: a, y :: b) 

let rec merge(l1, l2) = 
    match (l1, l2) with 
    |(_, []) -> l1
    |([],_) -> l2
    |(x::xs, y::ys) ->
        if x <= y then 
            [x] @ merge(xs, y::ys)
        else 
            [y] @ merge(x::xs, ys)

//System.Console.Write(merge([1;3;5], [2;4]))

let rec mergesort lizt =
    match lizt with 
    |[] -> []
    | x::xs ->
        let (a, b) = split lizt
        match (a, b) with
        | (_, []) -> a
        | ([], _) -> b
        | (xs, ys) -> 
            let n1 = mergesort xs
            let n2 = mergesort ys
            merge(n1, n2)


System.Console.Write(mergesort lz1)