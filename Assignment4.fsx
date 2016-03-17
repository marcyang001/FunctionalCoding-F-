
//module hw4

(* Assignment 4 *) (* Do not edit this line. *)
(* Student name: Marc Yang, Id Number: 260531701 *) (* Edit this line. *)

type typExp =
  | TypInt
  | TypVar of char
  | Arrow of typExp * typExp
  | Lst of typExp

type substitution = (char * typExp) list

(* check if a variable occurs in a term *)
let rec occurCheck (v: char) (tau: typExp) : bool =
    match tau with 
    | TypInt -> false
    | TypVar a -> if (v = a) then true else false 
    | Arrow (x,y) -> if (occurCheck(v) (x)) then true else occurCheck(v) (y)
    | Lst x -> occurCheck(v) (x)



let te3 = Arrow(TypInt, Arrow(TypVar 'a', Lst (TypVar 'b')))

//System.Console.Write(occurCheck ('c') (te3))
                       
     
(* substitute typExp tau1 for all occurrences of type variable v in typExp tau2 *)
let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
    if (occurCheck v tau2) = false then tau2
    else
        match tau2 with
        | TypInt -> TypInt
        | TypVar a -> if(v = a) then tau1 else TypVar c
        | Arrow (x, y) -> Arrow(substitute tau1 v x, substitute tau1 v y)
        | Lst x -> Lst(substitute tau1 v x)


//let m = substitute (TypInt) ('a') (Arrow(TypVar 'a', Arrow (TypInt, Lst (TypInt))));;
        
let applySubst (sigma: substitution) (tau: typExp) : typExp =
    List.fold(fun acc1 (v, texp1) -> substitute (texp1) (v) acc1) (List.fold(fun acc (v, texp) -> substitute (texp) (v) acc) tau sigma) sigma

(* This is a one-line program *)



//let rec unify (tau1: typExp) (tau2:typExp) : substitution =
//    failwith "Error - not implemented" 
(* Use the following signals if unification is not possible:

 failwith "Clash in principal type constructor"
 failwith "Failed occurs check"
 failwith "Not unifiable"

*)


(*

> let te4 = Prod(TypInt, Arrow(TypVar 'c', TypVar 'a'));;

val te4 : typExp = Prod (TypInt,Arrow (TypVar 'c',TypVar 'a'))

> let te3 = Prod (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'));;

val te3 : typExp = Prod (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'))

> unify te3 te4;;
val it : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]
> let result = it;;

val result : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]

> applySubst result te3;;
val it : typExp = Prod (TypInt,Arrow (TypInt,TypInt))
> applySubst result te4;;
val it : typExp = Prod (TypInt,Arrow (TypInt,TypInt))

*)


    

    
(*

> let te4 = Prod(TypInt, Arrow(TypVar 'c', TypVar 'a'));;

val te4 : typExp = Prod (TypInt,Arrow (TypVar 'c',TypVar 'a'))

> let te3 = Prod (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'));;

val te3 : typExp = Prod (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'))

> unify te3 te4;;
val it : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]
> let result = it;;

val result : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]

> applySubst result te3;;
val it : typExp = Prod (TypInt,Arrow (TypInt,TypInt))
> applySubst result te4;;
val it : typExp = Prod (TypInt,Arrow (TypInt,TypInt))

*)


  