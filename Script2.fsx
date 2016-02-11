
let d_list = [1;2;3]

let rec memberof(n,list) = 
    match list with
    |[] -> false
    | x::xs ->
        if (n = x) then
            true
        else 
            memberof(n, xs)


let rec remove(item, l) =
    match l with
    | [] -> []
    | x::xs -> 
        if item = x then
            remove(item, xs)
        else
             x::remove(item, xs)

System.Console.Write(remove(2, [1;2;2;3;2;1;6;7;0]))

System.Console.Write(remove(2, [2;2;2;2;2]))


