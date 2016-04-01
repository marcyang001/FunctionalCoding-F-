module hw4
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


     
(* substitute typExp tau1 for all occurrences of type variable v in typExp tau2 *)
let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
    if (occurCheck v tau2) = false then tau2
    else
        match tau2 with
        | TypInt -> TypInt
        | TypVar a -> if(v = a) then tau1 else TypVar a
        | Arrow (x, y) -> Arrow(substitute tau1 v x, substitute tau1 v y)
        | Lst x -> Lst(substitute tau1 v x)

 
        
let applySubst (sigma: substitution) (tau: typExp) : typExp =
    List.fold(fun acc1 (v, texp1) -> substitute (texp1) (v) acc1) (List.fold(fun acc (v, texp) -> substitute (texp) (v) acc) tau sigma) sigma

(* This is a one-line program *)



let rec unify (tau1: typExp) (tau2:typExp) : substitution =
    match tau1, tau2 with
    | TypInt, TypInt -> []
    | TypInt, TypVar b -> [(b, TypInt)]
    | TypInt, Arrow(x, y) -> failwith "Not unifiable"
    | TypInt, Lst x ->  failwith "Not unifiable"
    | TypVar a, TypInt -> [(a, TypInt)]
    | TypVar a, TypVar b -> [(a, TypVar b)]
    | TypVar a, Arrow(x, y) -> if (occurCheck (a) (Arrow(x, y)) = false) then [(a, Arrow(x, y))] else failwith "Failed occurs check"
    | TypVar a, Lst x -> if (occurCheck (a) x = false) then [(a, Lst x)] else failwith "Failed occurs check"
    | Arrow(x, y), TypInt ->  failwith "Not unifiable"
    | Arrow(x, y), TypVar b ->  if (occurCheck (b) (Arrow(x, y)) = false) then [(b, Arrow(x, y))] else failwith "Failed occurs check"
    | Arrow(x, y), Lst z ->  failwith "Clash in principal type constructor"
    | Arrow(x1, y1), Arrow(x2, y2) -> let firstHalf = unify(x1) (x2)
                                      let newExp1 = applySubst (firstHalf) y1
                                      let newExp2 = applySubst (firstHalf) y2
                                      unify newExp1 newExp2 @ firstHalf 
    | Lst z, TypInt -> failwith "Not unifiable"
    | Lst z, TypVar b -> if (occurCheck (b) z = false) then [(b, Lst z)] else failwith "Failed occurs check"
    | Lst z, Arrow(x, y) -> failwith "Clash in principal type constructor"
    | Lst z, Lst y -> unify (z) (y) 


let te3 = Arrow(TypVar 'a',Arrow (TypVar 'b',TypVar 'c'))
let te4 = Arrow(TypInt, Arrow(TypVar 'c', TypVar 'a'))
let unifier = unify te3 te4

System.Console.Write(unify)

//let te3 = Arrow(TypVar 'a',Arrow (TypVar 'b',TypVar 'c'))
//let te4 = Arrow(TypInt, Arrow(TypVar 'c', TypVar 'a'))


//let unifier = unify te3 te4

//let tau1 = Arrow(TypInt, Arrow(TypVar 'a', Lst (TypVar 'b')))
//let tau2 = Arrow (TypVar 'a',Arrow (TypInt,Lst TypInt))


//let tau3 = Arrow(TypVar 'a', TypVar 'a')
//let tau4 = Arrow(TypVar 'b', TypVar 'c')

//let te3 = Arrow(TypInt, Arrow(TypVar 'a', Lst (TypVar 'b')))
//let te4 = Arrow (TypVar 'a',Arrow (TypInt,Lst TypInt))
//System.Console.Write(occurCheck ('c') (te3))


(* Use the following signals if unification is not possible:

 failwith "Clash in principal type constructor"
 failwith "Failed occurs check"
 failwith "Not unifiable"


*)



(*
let tau3 = Arrow(TypVar 'a', TypVar 'a')
- let tau4 = Arrow(TypVar 'b', TypVar 'c')
- let unifier = unify tau3 tau4;;

> let te4 = Arrow(TypInt, Arrow(TypVar 'c', TypVar 'a'));;

val te4 : typExp = Arrow(TypInt,Arrow (TypVar 'c',TypVar 'a'))

> let te3 = Arrow(TypVar 'a',Arrow (TypVar 'b',TypVar 'c'));;

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
  