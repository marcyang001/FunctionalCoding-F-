(* Assignment 2 *) (* Do not edit this line. *)
(* Student name: Marc Yang, Id Number: 260531701 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code must compile and must not go into infinite
loops.  You are free to use library functions now.  But loops are BANNED
as is any kind of imperative code based on updating values.  You can make functions
recursive at top level or modify them so that the recursion is hidden inside.  The
only things we really insist on are: (a) use the names we have used and (b) the
functions must have the types that we have shown.  We hope by now you understand that
everywhere where it says failwith "Error - not implemented" you have to remove this
and replace it with your code.  *)

(* Question 1 *)

let deriv (f, dx: float) = fun x -> ((f(x + dx) - f(x))/dx)
(* val deriv : f:(float -> float) * dx:float -> x:float -> float *)

let rec newton(f,guess:float,tol:float,dx:float) = 
    if (abs(f(guess)) < tol) then guess - f(guess)/(deriv(f, dx) guess)
    else 
        let newGuess = guess - f(guess)/(deriv(f, dx) guess)
        newton(f, newGuess, tol, dx)
(* val newton : f:(float -> float) * guess:float * tol:float * dx:float -> float *)

(* For testing 
let make_cubic(a:float,b,c) = fun x -> (x*x*x + a * x*x + b*x + c);;
newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001);;
*)

(* Question 2 *)

type term = float * int
type poly = term list

exception EmptyList

(* Multiply a term by a polynomial. *)
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
(* val mtp : t:term * p:poly -> poly *)


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

(* Add two polynomials.  The result must be properly represented. This means you
cannot have more than one term with the same exponent, you should not have a
term with a zero coefficient, except when the whole polynomial is zero and the
terms should be decreasing order of exponents.   Thus, for example,
5.2 x^7 - 3.8 x^4 +2.0 x - 1729.0 should be represented as
[(5.2,7);(-3.8,4);(2.0,1);(-1729.0,0)] *)

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

    
(* Question 3 *)
(* Most of these functions are only one or two lines.  One of them, the longest is
about 5 lines.  However, they require some thought.  They are short because I used
the Set library functions wherever I could.  I especially found Set.fold useful. *)

type Country = string;;
type Chart = Set<Country*Country>;;
type Colour = Set<Country>;;
type Colouring = Set<Colour>;;

(* This is how you tell that two countries are neghbours.  It requires a chart.*)
let areNeighbours ct1 ct2 chart =
  Set.contains (ct1,ct2) chart || Set.contains (ct2,ct1) chart;;
(* val areNeighbours :
  ct1:'a -> ct2:'a -> chart:Set<'a * 'a> -> bool when 'a : comparison
  *)

(* The colour col can be extended by the country ct when they are no neighbours
according to chart.*)
  
let canBeExtBy col ct chart =  
    Set.forall(fun x -> not (areNeighbours ct x chart)) col 
(*
   val canBeExtBy :
  col:Set<'a> -> ct:'a -> chart:Set<'a * 'a> -> bool when 'a : comparison
*)

(* Here you have to extend a colouring by a fixed country. *)
let rec extColouring (chart: Chart) (colours : Colouring) (country : Country) =
    if (colours.IsEmpty = true) then 
        colours.Add(set[country])
    else 
        let chosen = Set.minElement(colours)
        let rest = Set.remove chosen colours
        if canBeExtBy chosen country chart = true then 
            let newChosen = chosen.Add(country)
            rest.Add(newChosen)
        else
            set[chosen] + extColouring chart rest country


(* This collects the names of the countries in the chart.  A good place
to use Set.fold *) 
let countriesInChart (chart : Chart) = 
    Set.fold(fun (accSet:Colour) (c1, c2) -> accSet.Add(c1).Add(c2)) Set.empty chart
(* val countriesInChart : chart:Chart -> Set<Country> *)

(* Here is the final function.  It is also most conveniently done with Set.fold *)
let colourTheCountries (chart: Chart)  =
    let set1 = countriesInChart chart 
    Set.fold(fun acc country -> extColouring chart acc country) Set.empty set1

(* val colourTheCountries : chart:Chart -> Colouring *)

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
        let a = eval(u, env)
        let b = eval(v, env)
        match a, b with
        | (_, None) -> None
        | (None, _) -> None
        | Some (l), Some (r) -> Some(l + r)
    | Mul(u, v) ->
        let a = eval(u, env)
        let b = eval(v, env)
        match a, b with
        | (_, None) -> None
        | (None, _) -> None
        | Some (l), Some (r) -> Some(l * r)

(* val eval : exp:Exptree * env:Bindings -> int option  *)
let myWorld:Chart = Set.ofList [("Andorra","Benin");("Andorra","Canada");("Andorra","Denmark");("Benin","Canada"); ("Benin","Denmark");("Canada","Denmark");("Estonia","Canada");("Estonia","Denmark");("Estonia","Finland");("Finland","Greece");("Finland","Benin");("Greece","Benin");("Greece","Denmark");("Greece","Estonia")]

//System.Console.Write(colourTheCountries myWorld)


