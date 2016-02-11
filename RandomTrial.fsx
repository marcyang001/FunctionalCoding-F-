let rec iter_sum(f, lo:float, hi:float, inc) =
  let rec helper(x:float, result:float) =
    if (x > hi) then result
    else helper(inc(x), f(x) + result)
  helper(lo,0.0);;

let integral(f,lo:float,hi:float,dx:float) =
  let delta (x:float) = x+dx
  dx * iter_sum(f,(lo + (dx/2.0)), hi, delta)

let r_sq (x:float):float = x * x

integral(r_sq,0.0,1.0,0.001)
let delta (x:float) = x + 1.0

//System.Console.Write(delta(1.0))
//System.Console.Write(iter_sum(r_sq, 1.0, 3.0, delta));

//System.Console.Write(integral(sin,0.0, 3.14159, 0.001))

type term = float * int
type poly = term list



exception EmptyList

let t1:term = (1.0, 8)

let (a, b) = t1
//System.Console.Write(a)

let l1:poly = [(3.5, 4); (4.0, 3)]
//System.Console.Write(l1)
 


let mtp(t:term,p:poly):poly =
   if p = [] then raise (EmptyList)

   else 
        let rec helper (t:term,ppoly:poly) = 
            let (c, cx) = t
            match ppoly with
            | [] -> []
            | x::xs -> 
            let (ax, bx) = x
            [(ax * c, cx+bx )] @ helper(t, xs)
        helper(t, p)


let d:poly = [(2.0, 5)]
let p:poly = [(2.0,6);(2.0,5);(2.0,1);(2.0,0)]
let p2:poly = [(2.1,7);(2.0,2);(2.0,1);(2.0,0)]
 
(* Add a term to a polynomial. *)
let rec atp(t:term,p:poly):poly = 
(* val atp : t:term * p:poly -> poly *)
    let (coef, deg) = t
    match p with 
    | [] -> [t]
    | x::xs -> 
           let (a, b) = x
           if b = deg then 
            (a+coef, b)::xs
           elif b > deg then
             (a,b)::atp(t, xs)
           else
              t::x::xs
    
let rec addpolys(p1:poly,p2:poly):poly = 
(* val addpolys : p1:poly * p2:poly -> poly *)
    match p1 with
    | [] -> p2
    | x::xs -> 
        addpolys(xs, atp(x,p2))
           


(* Multiply two polynomials.  All the remarks above apply here too. Raise an
exception if one of the polynomials is the empty list. *)
let rec multpolys(p1:poly,p2:poly) = 
(* val multpolys : p1:poly * p2:poly -> poly *)
    match p1 with 
    | [] -> p2
    | x::xs ->
        multpolys(xs, mtp(x, p2))

(* This is the tail-recursive version of Russian peasant exponentiation.  I have
done it for you.  You will need it for the next question.  *)
let exp(b:float, e:int) =
    let rec helper(b:float, e:int, a: float) =
        if (b = 0.0) then 0.0
        elif (e = 0) then a
        elif (e % 2 = 1) then helper(b,e-1, b*a)
        else helper(b*b,e/2,a)
    helper(b,e,1.0)

(* Here is how you evaluate a term. *)
let evalterm (v:float) ((c,e):term) = if (e=0) then c else c * exp(v,e)

//let t2 = (7.0,3)

(* Evaluate a polynomial viewed as a function of the indeterminate.  Use the function
above and List.fold and List.map and a dynamically created function for a one-line
answer.  *)
let evalpoly(p:poly,v:float):float = 
(* val evalpoly : p:poly * v:float -> float *)
    List.fold (fun acc termval -> acc + termval) 0.0 (p |> List.map(fun x -> evalterm v x))

(* Compute the derivative of a polynomial as a symbolic representation.  Do NOT use
deriv defined above.  I want the answer to be a polynomial represented as a list.
I have done a couple of lines so you can see how to raise an exception.  *)
let rec diff (p:poly):poly = 
(*  val diff : p:poly -> poly *)
    match p with
    | [] -> raise EmptyList
    | (coef,deg)::xs ->  
         if deg = 0 then [(0.0, 0)] else (coef * float(deg), deg-1)::(diff xs)


(* Question 4 *)

(* These functions are a bit longer but easier to code than Q3.  It is very similar
to the evaluator that I showed in class.  However I have used the Option type so that
the program gracefully returns None if no value is found.  This can be preferred to
raising an exception in some situations.  Learn option types from the web.  *)

type Exptree =
  | Const of int 
  | Var of string 
  | Add of Exptree * Exptree 
  | Mul of Exptree * Exptree;;

type Bindings = (string * int) list;;

(* The bindings are stored in a list rather than a BST for simplicity.  The
list is sorted by name, which is a string. *)
let rec lookup(name:string, env: Bindings) = 
(* val lookup : name:string * env:Bindings -> int option *)
    match env with 
    | [] -> None
    | x::xs ->
        let (str, num) = x
        if (str = name) then 
            Some(num)
        else
            lookup(name, xs)
                               
                    


//System.Console.Write(lookup("a", env))

            
                
                

(* Insert a new binding.  If the name is already there then the new binding should
be put in front of it so that the lookup finds the latest binding.  *)
let rec insert(name:string, value: int, b: Bindings) = 
(* val insert : name:string * value:int * b:Bindings -> (string * int) list*)
    let newItem = (name, value)
    match b with 
    | [] -> [(name, value)]
    | [(a,b)] -> if name < a then newItem::[(a,b)] else (a,b)::[newItem]
    | x1::xs -> 
        let (str, number) = x1
        if str = name then 
            newItem::x1::xs
        elif name < str then
            newItem::x1::xs
        else
            x1::insert(name, value, xs)


(* The recursive evaluator.  You have to match on the exp.  If a variable is not
found return None.  If you are applying an operator to None and something else the
answer is None.  This leads to a program of about 20 lines but it is conceptually
very easy.  *)



let rec eval(exp : Exptree, env:Bindings) =
(* val eval : exp:Exptree * env:Bindings -> int option  *)
    match exp with
    | Const a -> Some(a)
    | Var str -> 
        let result = lookup(str, env)
        if result = None then 
            None 
        else 
            result
    | Add(u, v) -> 
        if eval(u, env) = None || eval(v, env) = None then
            None 
        else
            let a = eval(u, env)
            let b = eval(v, env)
            match a, b with
            | (_, None) -> None
            | (None, _) -> None
            | Some (l), Some (r) -> Some(l + r)
    | Mul(u, v) ->
        if eval(u, env) = None || eval(v, env) = None then 
            None
        else 
            let a = eval(u, env)
            let b = eval(v, env)
            match a, b with
            | (_, None) -> None
            | (None, _) -> None
            | Some (l), Some (r) -> Some(l * r)

let env:Bindings = [("a",4);("a",3);("b",4);("c",5)]
System.Console.Write(eval(Add(Var "a", Const 3), env))

