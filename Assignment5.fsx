module hw5

(* Assignment 5 *) (* Do not edit this line. *)
(* Student name: Ada Lovelace, Id Number: 000000001 *) (* Edit this line. *)

(* This is the type definition for the expressions that we will produce as a result
of parsing the input strings. *)

type exptree = Var of char | Expr of char * exptree * exptree

(* We only allow lower-case one-character variable names*)
let charSet = ['a' .. 'z']

(* Here is an example input string.  Blank spaces are not allowed. *)
let example = "(a+(b+(c*d)+e)*f)*g"

(* This just tests if the character is one of the allowed variable names.*)
let isin (x: char) L = List.exists (fun y -> x = y) L

(* This is the top-level function.  It reads an input string and outputs the parse tree.
It is not recursive at top level but the main guts of it consists of three
mutually-recursive functions called expr, term and primary.  There is also a function
called getsym which returns the next character from the input string.  getsym is imperative
and uses the mutable local variables sym and cursor.  Please do not change the definition of
getsym or the declarations of sym and cursor.  No doubt all this can be done more slickly,
but I am trying to be as simple-minded as possible. *)

let parse (inputexp: string): exptree = 
    let sym = ref inputexp.[0]
    let cursor = ref 0

    let getsym () =
        cursor := !cursor + 1
        sym := inputexp.[!cursor]
    
    let rec expr (): exptree = 
        let t = term()
        if (!cursor = inputexp.Length - 1) then
            t
        else
            match !sym with
            | '+' -> getsym()
                     Expr('+', t, expr())
            | _ -> t

    (* stuff goes here *)
    and term (): exptree = 
        let p = primary()
        if (!cursor = inputexp.Length - 1) then
            p
        else
            match !sym with 
            | '*' -> getsym()
                     Expr('*', p, term())
            | _ -> p
                    
     (* stuff goes here *)
    and primary (): exptree =  //I did this for you.
        if !sym = '(' then
            getsym()
            let result = expr ()
            if not (!sym = ')') then 
                failwith "Mismatched parents"
            else 
                if (!cursor = inputexp.Length - 1) 
                then 
                    result
                else 
                    getsym()
                    result
        elif (isin !sym charSet) then 
            if (!cursor = inputexp.Length - 1) 
            then 
                (Var !sym) 
            else 
                let result = Var !sym in (getsym(); result)
        else
            printfn "sym is : %c." !sym
            failwith "In primary"

    expr() //This is part of the top-level function parse.

(* Now for Question 2 *)

(*  Do not change this.  tempstore will be a global variable.  Remember to reset this between
tests of the program. *)

let mutable tempstore = 0

let codegen (e: exptree) = 
    let rec helper (e: exptree, tag: char) = 
          match e with
          | (Expr(op, a, b))-> 
                               match a, b with
                               | (Expr(op1, x1, y1)), (Expr(op2, x2, y2)) ->  tempstore <- tempstore + 1 
                                                                              helper(a, '=')
                                                                              printfn "STORE %i" tempstore
                                                                              helper (b, op)
                                                                              tempstore <- tempstore - 1 
                               | (Var(x1)), (Expr(op2, x2, y2)) -> 
                                                                   tempstore <- tempstore + 1 
                                                                   helper(a, '=')
                                                                   printfn "STORE %i" tempstore
                                                                   helper (b, op)
                                                                   if (op = '+') then
                                                                      printfn "ADD %i" tempstore

                                                                   elif (op = '*') then
                                                                      printfn "MUL %i" tempstore
                                                                   tempstore <- tempstore - 1 
                               | (Expr(op1, x1, y1)), (Var(x2)) -> 
                                                                   helper(a, '=')
                                                                   helper(b, op)
                               | (Var(x1)), (Var(x2)) -> 
                                                         helper(a, '=')
                                                         helper(b, op)
          | (Var(a)) -> if (tag = '+') then
                            printfn "ADD %c" a
                        elif (tag = '*') then 
                            printfn "MUL %c" a
                        elif (tag = '=') then
                            printfn "LOAD %c" a
    helper(e,'=')
                   
let t5 = parse(example)

codegen(t5)

(* 
> codegen(t5);;
LOAD  a
STORE 1
LOAD  b
STORE 2
LOAD  c
MUL  d
ADD  e
ADD 2
MUL  f
ADD 1
MUL  g

*)