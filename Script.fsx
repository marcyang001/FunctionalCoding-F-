
let list1 = [1.1; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0]
let list = [3.0; 4.0; 8.0]
(* function that calculates the sum of a list *)
let sumlist l =
    let rec helper (sum, l) =
        match l with 
        | [] -> sum + 0.0
        | x::xs -> helper(sum+x, xs)
    helper(0.0, l)


let rec squarelist l = 
    match l with
    | [] -> []
    | x::xs -> [ x*x ] @ (squarelist xs)

let listsq = squarelist list1
 (* List.iter (fun elem -> printf "%g " elem) listsq *)

let mean l =
    let rec helper (sum, l, counter) =
        match l with 
        | [] -> sum / counter
        | x::xs -> helper(sum+x, xs, counter + 1.0)
    helper(0.0, l, 0.0)




let mean_diffs l =
    let n = mean l
    let rec helper (list) = 
        match list with
        |[] -> []
        | x::xs -> x - n :: (helper xs)
    helper(l)

let m = mean_diffs list



let variance l = 
    let md_list = mean_diffs l
    let s_list = squarelist md_list
    mean s_list

System.Console.WriteLine(variance list1)

(*
let rec helper(sum, list, counter) = 
        match list with
        | [] -> sum / counter
        | x::xs -> helper(sum+x, xs, counter + 1.0)
    helper(0.0, s_list, 0.0)
 
   *)



