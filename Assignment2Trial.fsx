
let rec iter_sum(f, lo:float, hi:float, inc) =
  let rec helper(x:float, result:float) =
    if (x > hi) then result
    else helper(inc(x), f(x) + result)
  helper(lo,0.0);;

let deriv (f, dx: float) = fun x -> ((f(x + dx) - f(x))/dx)
(* val deriv : f:(float -> float) * dx:float -> x:float -> float *)

(* val newton : f:(float -> float) * guess:float * tol:float * dx:float -> float *)
let rec newton(f, guess:float,tol:float,dx:float) = 
    //l
    if (abs(f(guess)) < tol) then guess - f(guess)/(deriv(f, dx) guess)
    else 
        let newGuess = guess - f(guess)/(deriv(f, dx) guess)
        newton(f, newGuess, tol, dx)


let make_cubic(a:float,b,c) = fun x -> (x*x*x + a * x*x + b*x + c);;
//System.Console.Write(newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001))


(* Question 2 *)

type term = float * int
type poly = term list



exception EmptyList

(* Multiply a term by a polynomial. *)
(* val mtp : t:term * p:poly -> poly *)
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


  
