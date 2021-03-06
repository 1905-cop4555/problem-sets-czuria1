﻿// in class example
type Person = {Name:string; Age:int}

// {} required to mean record
let p = {Age = 15; Name = "David"}

p.Age

// discriminated unions
type Shape = 
    | Rectangle of width : float * length : float
    | Circle of radiius : float
    | Prism of width : float * float * height : float

// constructing unions
let rect1 = Rectangle(length  = 1.3, width = 10.0)
let rect2 = Rectangle(width = 10.0, length  = 1.3)  // order does not matter

// simple unions
type color = Red | Green | Blue // constructors

let opinion = function 
    //| Red -> "nice" // without this you get a warning
    | Blue -> "lovely"
    | Green -> "ugly"

// trees
type 'a tree = 
    | Lf
    | Br of 'a * 'a tree * 'a tree

Br ("cat", Lf, Lf)

// sum in binary tree
let rec sum = function 
    | Lf -> 0
    | Br(m, t1, t2) -> m + sum t1 + sum t2

// find in binary tree
let rec element n = function
    | Lf -> false
    | Br(m, t1, t2) -> if n = m then true
                       elif n < m then element n t1
                       else element n t2

// correct insert in binary tree
let rec insert n = function
    | Lf -> Br(n, Lf, Lf)
    | Br(m, t1, t2) -> if n < m then Br(m, insert n t1, t2)
                       else Br(m, t1, insert n t2)

// build a tree from a list
let rec buildtree = function
    | [] -> Lf
    | x::xs -> insert x (buildtree xs)

let t1 = buildtree [3;1;4]
let t2 = buildtree ["cat";"dog";"bird"]

// insert into existing tree
let tr1 = insert 5 t1
let tr2 = insert "snake" t2

// parsing a string to a token list
type tokens = ZERO | ONE | EOF | ERROR

let rec parse = function 
    | "" -> [EOF]
    | s ->
        match s.Chars 0 with 
        | '0' -> ZERO :: parse (s.Substring 1)
        | '1' -> ONE :: parse (s.Substring 1)
        | c -> failwith (sprintf "PARSE: invalid input %A." c)

let string = "00101"

parse string

// using List.reduce 
List.reduce (+) ["a"; "b"]

// pattern matching option
let testoption = function
    | None -> "No value"
    | Some(n) -> sprintf "%i" n

testoption Some(15)

testoption None

// pivot for QuickSort
let rec split pivot = function
    | []    -> ([],[])
    | x::xs -> let (left,right) = split pivot xs
               if x < pivot then (x::left, right)
                            else (left, x::right)

split 4 [6; 2; 9; 1; 4; 6; 1]

// QuickSort function
let rec qsort = function
    | []    -> []
    | [x]   -> [x]
    | x::xs -> let (left, right) = split x xs    // x is the pivot
               qsort left @ x :: qsort right

qsort [3;1;4;1;5;9;2;6;5]
qsort [3.0; 1.5; 0.6]
qsort ["fish"; "dinosaur"; "elephant"; "cockatiel"]

// Accumulation Tail Recursion
let rec accfact = function
    | (0, a) -> a
    | (n, a) -> accfact(n-1, n*a)

let factorial n = accfact(n,1)

factorial 5

// Inifinte Streams
type 'a stream = Cons of 'a * (unit -> 'a stream)

let rec upfrom n = Cons(n, fun () -> upfrom(n+1))

let rec take n (Cons(x, xsf)) =
  if n = 0 then []
           else x :: take (n-1) (xsf())
           
let rec drop n (Cons (x, xsf)) =
  if n = 0 then Cons (x, xsf)
           else drop (n-1) (xsf())
         
let rec filter p (Cons(x, xsf)) =
  if p x then Cons(x, fun () -> filter p (xsf()))
         else filter p (xsf())
 
let rec eratosthenes (Cons(x, xsf)) =
  Cons(x, fun () -> eratosthenes (filter (fun n -> n%x <> 0) (xsf())))
  
let primes = eratosthenes (upfrom 2)

primes |> drop 9999 |> take 10