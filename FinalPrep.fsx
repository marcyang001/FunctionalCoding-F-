open System

let n = [[1;2]; [2;4;9]] 


let squarematrix (l:int list list) = 
    let row = List.head l 
    let rowLength = row.Length
    let column = l.Length
    if (rowLength <> column) then 
        false
    else
        let total = List.fold(fun acc (x:int list) -> acc+ x.Length) 0 l 
        if (float(total)/float(rowLength) <> float(column)) then
            true
        else
            false


squarematrix n


List.item 1 [0;2;2]

let trace (matrix: int list list) =
    let rec helper(m, index, sum) = 
        match m with
        |[] -> sum
        |[[]] -> sum 
        | x::xs -> let element = List.item index x
                   helper(xs, index+1, sum + element)
    helper(matrix, 0, 0)

trace [[0;2;2];[0;2;2];[0;2;2]]

let rec inter item lst =
  match lst with
    | [] -> [[item]]
    | x :: xs -> (item :: lst) :: (List.map (fun u -> (x:: u)) (inter item xs))


let rec shuffle (l1, l2) = 
    match l1, l2 with
    | [], [] -> [[]]
    | [], y::ys -> [l2]
    | x::xs, [] -> [l1]
    | x::xs, y::ys -> (List.map(fun l -> x:: l) (shuffle(xs, l2))) @ List.map(fun l -> y:: l) (shuffle(l1, ys))


let x = 1
let y = 2
let z = x + y

//let reverseList list = List.fold (fun acc elem elem2 -> (elem+elem2)::acc) [] [1;3;5] [1;3;5]

List.fold (fun acc (x) -> acc + x) 0 [1]
//List.fold (fun acc x y -> (x+y)::acc) [] [1] [2]

let rec pascal n = 
    if n = 0 then [1]
    else 
       let rrow = pascal(n-1)@ [0]
       let rprime = 0::pascal(n-1)
       List.fold2 (fun acc rr rp -> (rr+ rp)::acc) [] rrow rprime

//Console.Write(pascal 4)

let max_so_far = 
    let max = ref 0
    let counter = ref 0
    fun n -> if (!counter = 0) then 
                (max := n; counter := !counter + 1; !max) 
             else
                if (!max < n) then
                    max := n
                    !max
                else
                    !max


let rec descendCheck l = 
    match l with 
    | [] -> true
    | [x] -> true
    | x::y::xs -> if (x < y) then false
                  else
                      descendCheck(y::xs)

let rec apply_list l =
    match l with
    | [] -> (fun x -> x)
    | f::fs -> (fun x -> f ((apply_list(fs) x)))


let pred n = n - 1
let square (n:int) = n * n
let inc n = n + 1

let lst = [pred; square; inc]


let a = ref 10
let b = a.Value

(*
let rec transpose m = 
    let helper (matrix) = 
        if (List.forall(fun row -> row = []) matrix) then 
            []
        else
            List.map(fun (y::ys) -> ys) matrix
    match m with
    | [] -> [] 
    | first::rest ->
        [(List.fold(fun acc (x::xs) -> acc @ [x]) [] m)] @ transpose(helper(m))

   *)
(*
let trace1 m = 
    let rec helper lst acc = 
        match lst with 
        | [] -> acc
        | [[]] -> 0 
        | (x::xs)::rest -> 
                    (helper(List.map(fun (y::ys) -> ys) rest) (x+acc))
    helper m 0 

    *)
let f = (fun x -> x) (fun x -> x) 1

let rec fib n =
    if n = 0 || n = 1 then
        1
    else
        fib (n-2) + fib (n-1)
      
(* Sticks an item onto a lazy stream *)
let cons x sigma = Seq.append (Seq.singleton x) sigma

(* Grabs the first item from a stream. *)
let first sigma = Seq.item 0 sigma

(* Removes the first item and gives you the rest of the stream. *)
let rest sigma = Seq.skip 1 sigma



(* Makes a list out of the first n elements of a stream.  Useful for display.  There is
a built-in primitive to do this but I like this better. *)

let rec prefix (n: int) sigma = 
    if (n = 0) then []
    else (first sigma) :: (prefix (n - 1) (rest sigma))

let power n=
    let rec helper exp= Seq.delay(fun () -> Seq.append (Seq.singleton (n*exp)) (helper(exp*n)))
    helper 1


let fibonacci = Seq.unfold (fun (x, y) -> Some(x, (y, x + y))) (0I,1I)


let fib1 = 
    let rec helper (x,y) = Seq.delay(fun () -> Seq.append (Seq.singleton (x)) (helper(y, y+x)))
    helper (1,1)





                         



//printfn "%A", trace1 [[1;2;3];[1;2;3];[1;2;3]]

(*
val it =
  [[1,2,3,4,5],[1,2,4,3,5],[1,2,4,5,3],[1,4,2,3,5],[1,4,2,5,3],[1,4,5,2,3],
   [4,1,2,3,5],[4,1,2,5,3],[4,1,5,2,3],[4,5,1,2,3]] : int list list
shuffle([1,1,2],[3,4]);
val it =
  [[1,1,2,3,4],[1,1,3,2,4],[1,1,3,4,2],[1,3,1,2,4],[1,3,1,4,2],[1,3,4,1,2],
   [3,1,1,2,4],[3,1,1,4,2],[3,1,4,1,2],[3,4,1,1,2]] : int list list
shuffle([],[1,2,3,4]);
val it = [[1,2,3,4]] : int list list

*)
