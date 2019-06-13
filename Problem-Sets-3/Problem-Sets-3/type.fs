module type_example
// PCF Type Inference Skeleton File

// This lets us refer to a constructor like "Parser.Parse.ID" simply as "ID".
open Parser.Parse

type typ = VARIABLE of string | INTEGER | BOOLEAN | ARROW of typ * typ

/// Convert a typ to a string, using as few parentheses as necessary.
let rec typ2str = function
| VARIABLE a -> "'" + a
| INTEGER    -> "int"
| BOOLEAN    -> "bool"
| ARROW (ARROW (t1, t2), t3) -> "(" + typ2str (ARROW (t1, t2)) + ") -> " + typ2str t3
| ARROW (t1, t2) -> typ2str t1 + " -> " + typ2str t2

// Code for handling substitutions, which are functions typ -> typ.

/// Identity substitution.
let I (t : typ) = t

/// unify (t1, t2) returns the most general substitution that
/// unifies types t1 and t2, failing if no such substitition exists.
let rec unify (t1, t2) =
  let rec replace (a, t) = function
  | VARIABLE b     -> if a = b then t else VARIABLE b
  | ARROW (t1, t2) -> ARROW (replace (a, t) t1, replace (a, t) t2)
  | t1             -> t1
  
  let rec occurs = function
  | (a, VARIABLE b)          -> (a = b)
  | (a, ARROW (t1, t2))      -> occurs (a, t1) || occurs (a, t2)
  | (a, _)                   -> false
  
  match (t1, t2) with
  | (VARIABLE a, t) -> 
      if t = VARIABLE a then I                  //noting to do
      elif occurs (a, t) then                   //causes infinite recursion
        failwith (sprintf "circularity: cannot unify %A and %A" a t)
      else replace (a, t)                       //subsitute all Variable a with t
  | (t, VARIABLE a)    -> unify (VARIABLE a, t) //a is a variable, t is not, unify t to a
  | (INTEGER, INTEGER) -> I                     //nothing to do
  | (BOOLEAN, BOOLEAN) -> I                     //nothing to do
  | (ARROW (t3, t4), ARROW (t5, t6)) ->         
      let s1 = unify (t3, t5)                   //unify left side
      let s2 = unify (s1 t4, s1 t6)             //unify right with new left
      s2 << s1                                  //compose s1 to s2
  | _ -> failwith (sprintf "mismatch: cannot unify %A and %A" t1 t2)

// Code for handling environments, which are functions string -> typ.

let emptyenv x = failwith ("identifier " + x + " is unbound")

/// update env x t is the same as env, except that x is mapped to t.
let update env (x : string) (t : typ) = fun y -> 
    if y = x then t                             //return new value for x 
    else env y                                  //return value from env

// Code for generating new type variables.

// We start by building the infinite stream of all type variables.
type 'a stream = Cons of 'a * (unit -> 'a stream)

//type variables only have one letter
let rec upfrom n =
  let letters = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m";
                 "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"]
  Cons (VARIABLE (letters.Item (n % 26) + if n/26 = 0 then "" else string (n/26)),
        fun () -> upfrom (n+1))
     
let alltypevars = upfrom 0

/// newtypevar() generates a new type variable, and reset() resets back to 'a.
let (newtypevar, reset) =                       //returns tuple
  let vars = ref alltypevars                    //mutable
  let hd (Cons (x, xsf)) = x
  let tl (Cons (x, xsf)) = xsf()
  ((fun () ->                                       
      let next = hd (!vars) in vars := tl (!vars)   
      next),                                    //next available var
   (fun () -> vars := alltypevars))             //reset to all vars

/// Milner's Algorithm W
// I have included the code for NUM and IF; you must complete the rest!
let rec W (env, e) = 
  match e with
  | NUM n  -> (I, INTEGER)
  | BOOL b -> (I, BOOLEAN)
  | IF (e1, e2, e3) ->
      let (s1, t1) = W (env, e1)
      let s2 = unify (t1, BOOLEAN)
      let (s3, t2) = W (s2 << s1 << env, e2)
      let (s4, t3) = W (s3 << s2 << s1 << env, e3)
      let s5 = unify (s4 t2, t3)
      (s5 << s4 << s3 << s2 << s1, s5 t3)
  | SUCC -> (I, INTEGER)
  | PRED -> (I, INTEGER)

  
/// infer e finds the principal type of e
let infer e =
  reset ();
  let (s, t) = W (emptyenv, e)
  printf "The principal type is\n %s\n" (typ2str t)

let test () =
    infer (NUM 12)
    infer (BOOL true)
    infer (IF(BOOL true, NUM 1, NUM 2))
    infer (IF(BOOL true, IF(BOOL true, NUM 1, NUM 2), IF(BOOL false, NUM 3, NUM 4)))
    ()
