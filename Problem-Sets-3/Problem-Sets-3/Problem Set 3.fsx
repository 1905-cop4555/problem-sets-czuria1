(*
1. Building a simple tree.
    a. Create a discriminated union that can represent a linked list of integers.
    b. Write a function that converts a list into a linked list of nodes.
*)
type tree = 
    | Empty 
    | Cons of head:int * tail:tree

let rec convert = function
    | [] -> Empty
    | x::xs -> Cons(head = x, tail = convert xs)

(*
2. This CFG recognizes some strings of zeros and ones.
        S → 0A | 1B
        A → 0AA | 1S | 1
        B → 1BB | 0S | 0 
    a. Describe the strings that the CFG recognizes.
    b. This language is ambiguous. Find a string that is recognized by this grammar which has two derivations.
    c. Show the two derivation trees for the string in part (b).

    b. 00101011

    c. 
            S
           / \
          0   A
            / |  \
           0  A    A
              |  / |  \
              1 0  A    A
                   |  / | \
                   1 0  A  A
                        |  | 
                        1  1

            S
           / \
          0   A
            / |   \
           0  A      A
             / \     |
            1   S    1
               / \
              0   A
                 / \
                1   S
                   / \
                  0   A
                      |
                      1


*)

(*
3. A palindrome is a word that is spelled the same backwards as forwards. Our palindromes will have a vertical bar in 
    the middle, to separate the first half from the second half. If we restrict the alphabet to {a, b, |}, then some 
    example palindromes are a|a, b|b, ab|ba, ba|ab, aaab|baaa, ....

    a. Write a CFG to recognize palindromes over the alphabet {a, b, |}, with the bar in the middle.
    b. Write a parse function that accepts a string and generates tokens for the language.
    c. Write a syntax checker that verifies if a list of tokens represents a palindrome.
    d. Extend the syntax checker so it generates an abstract syntax tree and displays it, for valid palindromes.

    a. 
    S → aSa | bSb | "|"
*)

type TERMINAL = A|B

let eat token = function
    | [] -> failwith "premature termination of input"
    | x::xs ->
        if x = token
        then xs
        else failwith (sprintf "want %A, got %A" token x)

(*
4. Using the natural semantics from the lecture notes, show all the steps for verifying each judgement. 
    [When writing derivations, I tend to set a variable to the current state of memory, to save typing. 
    My derivations are six lines, six lines, and 22 lines.]
    a. ({i=5; j=8}, i := 2*j + i) => {i=21; j=8}
    b. ({i=3; j=8}, if (2*i > j) then i := 2*j else j := 2*i) => {i=3; j=6}
    c. ({i=1; j=10}, while (3*i <= j) do i := 3*i) => {i=9; j=10}

    a. ({i=5; j=8}, i := 2*j + i) => {i=21; j=8}
    1) ({i=5; j=8}, 2*j + i) => {i=21; j=8}
    2) ({i=5; j=8}, 2*j) => 16
    3) ({i=5; j=8}, j) => 8
    4) ({i=5; j=8}, 2) => 2
    5) ({i=5; j=8}, i) => 5

    b. ({i=3; j=8}, if (2*i > j) then i := 2*j else j := 2*i) => {i=3; j=6}
    1) ({i=3; j=8}, j := 2*i) => {i=3; j=6}
    2) ({i=3; j=8}, 2) => 2
    3) ({i=3; j=8}, i) => 3
    4) ({i=3; j=8}, if (2*i > j)) => false
    5) ({i=3; j=8}, 2*i) => 6
    6) ({i=3; j=8}, 2) => 2
    7) ({i=3; j=8}, i) => 3
    8) ({i=3; j=8}, j) => 8
*)

(*
5. Write a tail-recursive F# function interleave(xs,ys) that interleaves two lists:
      > interleave ([1;2;3],[4;5;6]);;
                val it : int list = [1; 4; 2; 5; 3; 6] 
    Assume that the two lists have the same length.

    Compare the timing of the recursive function from Problem Set 1 with this tail-recursive version. Time these examples in both versions.

    list1 = [1..2..19999], list2 = [2..2..20000]
    list1 = [1..2..199999], list2 = [2..2..200000]
*)

let rec nonTailInterleave = function
    | [], [] -> []
    | xs, [] -> xs
    | [], ys -> ys
    | x::xs, y::ys -> x::y::nonTailInterleave(xs,ys)

let interleave xs ys = 
    let rec aux xs ys a = 
        match xs, ys with 
        | [], [] -> a
        | x::xs, [] -> 
        | [], ys -> ys
        | x::xs, y::ys ->


interleave ([1;2;3],[4;5;6])

(*
6. Alternating series
   a. Generate an infinite sequence for the alternating series of 1/(2**n):
    1/2, -1/4, 1/8, -1/16, 1/32, -1/64, ...
   b. Display the 5th through 15th numbers in the series. The numbers should display as the floating point version of the fractions.
   c. Repeat the exercise using an infinite stream.
*)

let series n = seq { for a in 1 .. n do yield (pown -1 (a+1))*(pown 2 a)}

let seq1 = series 7

printfn "%A" seq1

(*
7. Multiples of a list
    a. Generate an infinite stream for the the natural numbers greater than zero that are divisible by each element in a 
    list of four elements. Use four, nested calls of filter on the infinite stream of natural numbers starting at one. 
    For example the output for the list [2;3;21;10]:
    210, 420, 630, 840, 1050, ...
    b. Display the 20th through 30th numbers in the series.
    c. Repeat the exercise using an infinite sequence. Sequences also have a filter function, so it can be solved similarly 
    to the infinite stream version. Just for fun, try to solve it without using the filter function.
    d. For both functions, be sure to dislay an appropriate error message if the list does not have exactly four elements.
*)

type 'a stream = Cons of 'a * (unit -> 'a stream)

let rec upfrom n = Cons(n, fun () -> upfrom(n+1))

let nats = upfrom 1

let rec take n (Cons(x, xsf)) = 
    if n = 1 then []
    else x :: take (n-1) (xsf()) 

let rec filter p (h::hs) (Cons(x, xsf)) = 
    if p h x then Cons(x, fun () ->  filter p hs (xsf())) 
    else filter p hs (xsf())

let list = [2;3;21;10]

let numbers = take 6 (filter (fun n l -> n%l = 0) list nats)

(*
8. Create a tail-recursive function that has a big integer as input and calculates 2I raised to that power.
    Calculate these powers of 2I: 0I, 1I, 2I, 4I, 16I, 256I, 1024I, 32768I and 65536I.
*)

let power n =
    let rec loop a = function 
        | 0 -> a
        | n -> loop (2I*n) (a-1I)
    loop 1I n

power 2I

power 4I

(*
10. List the steps that F# follows to determine the type for f: (fun f -> f (f 17.3)).

    (float -> float) -> float
*)

(*
11. Write a non-recursive fibonacci function using imperative F#.Compare the timing with a tail-recursive fibonacci.
*)

let fib n =
    let fib1 = ref 0
    let fib2 = ref 1
    let sum = ref 0
    let cnt = ref n
    while !cnt > 1 do 
        sum := !fib1 + !fib2
        fib1 := !fib2
        fib2 := !sum
        cnt := !cnt - 1
    !sum

fib 5

fib 6

fib 7

(*
12. Using imperative F#, create a record type for a student. The record will have a function that returns the student's 
GPA, a function that adds credit hours and a function that adds grade points. Initialize an instance of the record with 
appropriate functions and values. Use the instance to add grade points and credits several times, and display the GPA.
*)

type Student = {gpa: unit -> float; credits: float -> unit; grade: float -> unit}

let s1 =
    let cred = ref 4.00
    let pts = ref 0.0
    let avg = ref 4.00
    {gpa = fun () -> !avg; credits = fun c -> cred := !cred + c; grade = fun p -> pts := !pts + p}

//s1.gpa ()

(* Problem 16 *)

    
