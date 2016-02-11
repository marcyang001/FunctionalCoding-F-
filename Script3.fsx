
let rec memberof(n,list) = 
    match list with
    |[] -> false
    | x::xs ->
        if (n = x) then
            true
        else 
            memberof(n, xs)

let rec isolate l =
    match l with 
    | [] -> []
    | x::xs ->
        if not (memberof(x, xs)) then
            x :: isolate xs
        else
            isolate xs

//System.Console.Write(isolate [1;2;3;4;5;6;2;3;4;4;5;5])  
                       

let rec common(l1, l2) =
    match l1 with
    | [] -> [] 
    | x::xs -> 
        if memberof(x, l2) then
            x::common(xs, l2)
        else 
            common(xs, l2)

//System.Console.Write(common ([3;4;5;7;2],[1;3;5;7;9;1]))  


System.Console.Write(isolate [])