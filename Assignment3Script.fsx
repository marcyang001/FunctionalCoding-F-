
(* Question 1 *)
type Cell = { data : int; next : RList}
and RList = Cell option ref

(* For testing.  Uncomment if you want to use them. *)
let c1 = {data = 1; next = ref None}
let c2 = {data = 2; next = ref (Some c1)}
let c3 = {data = 3; next = ref (Some c2)}
let c5 = {data = 5; next = ref (Some c3)}


(* This converts an RList to an ordinary list. *)
let rec displayList (c : RList) =
  match !c with
    | None -> []
    | Some { data = d; next = l } -> d :: (displayList l)

(* This may be useful.  You don't have to use it.*)
let cellToRList (c:Cell):RList = ref (Some c)

(* Example for testing. *)
let bigger(x:int, y:int) = (x > y)

let rec insert comp (item: int) (list: RList) = 
    match !list with
    | None ->  list := Some {data = item; next = ref None}
    | Some {data = d; next = l} ->
        if comp(item, d) then list := Some {data = item; next = ref (Some {data = d; next = l})}
        else insert comp item l


(* Question 2*)

type transaction = Withdraw of int | Deposit of int | CheckBalance

let make_protected_account(opening_balance: int,password: string) =
    let balance = ref opening_balance
    let passcode = ref password
    fun (pass: string, t: transaction) ->
        if (pass <> !passcode) then
            printfn "Password is incorrect"
        else
            match t with
            | Withdraw(m)-> if (!balance > m) then
                                balance := !balance - m
                                printfn "Balance is %i" !balance
                            else
                                printfn "Insufficient funds. You have only %i" !balance
            | Deposit(m) -> (balance := !balance + m; (printf "Balance is %i\n" !balance))
            | CheckBalance -> (printf "Balance is %i\n" !balance)


(* Question 3 *)

open System.Collections.Generic;;

type ListTree<'a> = Node of 'a * (ListTree<'a> list)

(* For testing.  Uncomment if you want to use them. *)
let n14 = Node("14", [])
let n13 = Node("13", [n14])
let n5 = Node("5.0",[])
let n6 = Node("6.0",[])
let n7 = Node("7.0",[])
let n8 = Node("8.0",[])
let n9 = Node("9.0",[])
let n10 = Node("10.0",[])
let n12 = Node("12.0",[n13])
let n11 = Node("11.0",[n12])
let n2 = Node("2.0",[n5;n6;n7;n8])
let n3 = Node("3.0",[n9;n10])
let n4 = Node("4.0",[n11])
let n1 = Node("1.0",[n2;n3;n4])



//let q = Queue<ListTree<'a>> ()
//q.Enqueue(n1)

//let Node(parent, childrenList) = q.Dequeue()




let bfIter f ltr = 
    let q = Queue<'a> ()
    match ltr with
    | Node(a, b) -> q.Enqueue(a)
    let rec helper (item) = 
        match item with
        | Node(parent, list) -> 
            if not list.IsEmpty then
                for i in list do
                    match i with
                    | Node(a, l) ->
                        q.Enqueue(a)
            for j in list do
                helper(j)
    helper(ltr)
    while q.Count <> 0 do
        let c = q.Dequeue()
        f c

bfIter (fun n -> printfn "%s" n) n1;;
 
    
(*   This is how you set up a new Queue: let todo = Queue<ListTree<'a>> () *)








(* How I tested the BFS program.   
bfIter (fun n -> printfn "%i" n) n1;;

*)

        
                            
        
                         

