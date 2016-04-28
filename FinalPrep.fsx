
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

let reverseList list = List.fold (fun acc elem elem2 -> (elem+elem2)::acc) [] [1;3;5] [1;3;5]

List.fold (fun acc (x) -> acc + x) 0 [1]
List.fold (fun acc x y -> (x+y)::acc) [] [1] [2]

let rec pascal n = 
    if n = 0 then [1]
    else 
       let rrow = pascal(n-1)@ [0]
       let rprime = 0::pascal(n-1)
       List.fold2 (fun acc rr rp -> (rr+ rp)::acc) [] rrow rprime


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
