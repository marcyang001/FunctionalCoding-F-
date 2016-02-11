(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Marc Yang, Id Number: 260531701 *) (* Edit this line. *)

(* Question 1 *) (* Do not edit this line. *)

(* val sumlist : l:float list -> float *)
let rec sumlist l =
    let rec helper (sum, l) =
        match l with 
        | [] -> sum + 0.0
        | x::xs -> helper(sum+x, xs)
    helper(0.0, l)

(* val squarelist : l:float list -> float list *)
let rec squarelist l = 
    match l with
    | [] -> []
    | x::xs -> [ x*x ] @ (squarelist xs)

(* val mean : l:float list -> float *)
let mean l =
    let rec helper (sum, l, counter) =
        match l with 
        | [] -> sum / counter
        | x::xs -> helper(sum+x, xs, counter + 1.0)
    helper(0.0, l, 0.0)


(* val mean_diffs : l:float list -> float list *)
let mean_diffs l =
    let n = mean l
    let rec helper (list) = 
        match list with
        |[] -> []
        | x::xs -> x - n :: (helper xs)
    helper(l)


(* val variance : l:float list -> float *)
let variance l = 
    let md_list = mean_diffs l
    let s_list = squarelist md_list
    mean s_list

(* End of question 1 *) (* Do not edit this line. *)

(* Question 2 *) (* Do not edit this line. *)

(* val memberof : 'a * 'a list -> bool when 'a : equality *)
let rec memberof l =  //(item,list)
    match l with
    | (item, []) -> false
    | (item, x::xs) ->
        if (item = x) then
            true
        else 
            memberof(item, xs)

(* val remove : 'a * 'a list -> 'a list when 'a : equality *)
let rec remove l =
    match l with
    | (item, []) -> []
    | (item, x::xs) -> 
        if item = x then
            remove(item, xs)
        else
             x::remove(item, xs) 

(* End of question 2 *) (* Do not edit this line *)

(* Question 3 *) (* Do not edit this line *)

(* val isolate : l:'a list -> 'a list when 'a : equality *)
let rec isolate l =
    match l with 
    | [] -> []
    | x::xs ->
        if not (memberof(x, xs)) then
            x :: isolate xs
        else
            isolate xs

(* End of question 3 *) (* Do not edit this line *)

(* Question 4 *) (* Do not edit this line *)

(* val common : 'a list * 'a list -> 'a list when 'a : equality *)
let rec common l =
    match l with
    | ([],[]) -> []
    | (x, []) -> []
    | ([], y) -> [] 
    | (r::rs, w) -> 
        let c = isolate(r::rs)
        match c with
        | [] -> []
        | d::ds ->
            if memberof(d, w) then
                d::common(ds, w)
            else 
                common(ds, w)

(* End of question 4 *) (* Do not edit this line *)

(* Question 5 *) (* Do not edit this line *)

(* val split : l:'a list -> 'a list * 'a list *)

let rec split l =
    match l with
    | [] -> ([], [])
    | [ var1 ] -> ( [ var1 ] , [])
    | x::y::rs -> 
        let (a, b) = split rs
        (x :: a, y :: b) 

(* val merge : 'a list * 'a list -> 'a list when 'a : comparison *)
let rec merge l = 
    match l with 
    |(l1, []) -> l1
    |([],l2) -> l2
    |(x::xs, y::ys) ->
        if x <= y then 
            [x] @ merge(xs, y::ys)
        else 
            [y] @ merge(x::xs, ys)

(* val mergesort : l:'a list -> 'a list when 'a : comparison *)
let rec mergesort l = 
    match l with 
    |[] -> []
    | x::xs ->
        let (a, b) = split l
        match (a, b) with
        | (_, []) -> a
        | ([], _) -> b
        | (xs, ys) -> 
            let n1 = mergesort xs
            let n2 = mergesort ys
            merge(n1, n2)

(* End of question 5 *) (* Do not edit this line *)



//System.Console.WriteLine(common([1;2;3], [2;3]))






let list1 = [1.1; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0]
let list = [3.0; 4.0; 8.0]
let lz1 = [2;5;3;4;1]
let l1 = [1; 3; 2; 5; 4]  
//System.Console.WriteLine(variance list)
let l = [1;2;2;3;2;1;6;7;0]
//System.Console.Write(memberof(1,l))
System.Console.Write(isolate [2;2;2;2])
//System.Console.Write(remove(2, [2;2;2;2;3]))






let li1 = [1;2;2;4;2;5]
let li2 = [1;4;2;3]

//System.Console.Write(common(li1, li2))

//System.Console.Write(mergesort li1)