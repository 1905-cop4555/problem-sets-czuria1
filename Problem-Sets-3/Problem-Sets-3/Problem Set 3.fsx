(*
1. Building a simple tree.
    a. Create a discriminated union that can represent a linked list of integers.
    b. Write a function that converts a list into a linked list of nodes.
*)
type linkedList = 
    | Empty 
    | Cons of head:int * tail:linkedList

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

type TERMINAL = A|B|I|EOF

let eat token = function
    | [] -> failwith "premature termination of input"
    | x::xs ->
        if x = token
        then xs
        else failwith (sprintf "want %A, got %A" token x)

let rec S = function
    | [] -> failwith "premature termination of input"
    | x::xs ->
        match x with
        | A -> xs |> S
        | B -> xs |> S
        | I -> xs
        | _ -> failwith (sprintf "S: got %A" x)

let accept() = printfn("Input accepted")
let error() = printfn("Input error")

let test_program program =
          let result = program |> S
          match result with 
          | [] -> failwith "Early termination or missing EOF"
          | x::xs -> if x = EOF then accept() else error()

test_program [A;I;A;EOF]

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

let rec interleave x y a = 
    match x, y with 
    | [], [] -> a
    | _, [] -> failwith "Different lengths"
    | [], _ -> failwith "Different lengths"
    | x::xs, y::ys -> x::y::interleave xs ys a


interleave [1;2;3] [4;5;6] []

(*
6. Alternating series
   a. Generate an infinite sequence for the alternating series of 1/(2**n):
    1/2, -1/4, 1/8, -1/16, 1/32, -1/64, ...
   b. Display the 5th through 15th numbers in the series. The numbers should display as the floating point version of the fractions.
   c. Repeat the exercise using an infinite stream.
*)

let seqInfinite = Seq.initInfinite (fun index ->
    let n = float(index + 1)
    1.0 / ((2.0**n) * (if ((index + 1) % 2 = 0) then -1.0 else 1.0)))

printfn "%A" seqInfinite

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

let rec filter p (Cons(x, xsf)) = 
    if p x then Cons(x, fun () ->  filter p (xsf())) 
    else filter p (xsf())

let list = [2;3;21;10]

let rec gcd a b =
    if b = 0 then abs a
    else gcd b (a % b)

let lcm (a,b) = a * b / (gcd a b)

let rec traverse = function
    | [] -> 1
    | [x] -> x
    | [x;y] -> lcm (x,y)
    | x::xs -> lcm (x, traverse xs)

let multiple = traverse list

let newList n l = l |> List.map (fun x -> x*n)

let numbers = take 6 (filter (fun n -> n%2 = 0) nats)

(*
8. Create a tail-recursive function that has a big integer as input and calculates 2I raised to that power.
    Calculate these powers of 2I: 0I, 1I, 2I, 4I, 16I, 256I, 1024I, 32768I and 65536I.
*)

let power n =
    let rec loop a n =
        match n with 
        | n when n = 0I -> a
        | n when n = 1I -> 2I
        | n -> loop (a + (2I*2I)) (n - 1I)
    loop 1I n

//power 0I

//power 1I

power 4I

(*
9. An interesting higher-order function is twice, which can be defined by
    > let twice f = (fun x -> f (f x));
      val twice : ('a -> 'a) -> 'a -> 'a 
    or, using F#'s function composition operator <<, by
    > let twice f = f << f;;
      val twice : ('a -> 'a) -> ('a -> 'a)
    If we also define
    > let successor n = n+1;;
      val successor : int -> int
    then we can evaluate expressions like
    > (twice (twice (twice (twice successor)))) 0;;
      val it : int = 16  
    It is pretty easy to see that with k occurrences of twice, these expressions will return 2^k.
    Remarkably, F# also allows us to evaluate expressions like

      twice twice twice twice successor 0
    in which the function applications are associated to the left, by F#'s default parsing conventions. (Notice that 
  this means that twice gets applied to itself!) Can you figure out a formula that gives the value of

      twice twice twice ... twice successor 0
    when there are k occurrences of twice? (I suggest that you approach this problem experimentally.)

    Consider a similar function named thrice

    let thrice f = f << f << f
    Can you predicte the output of

    thrice thrice successor 0 => 27
    twice thrice successor 0 => 9
    thrice twice successor 0 => 8
*)

//let twice f = (fun x -> f (f x))

let twice f = f << f

let successor n = n+1

//(twice (twice (twice (twice successor)))) 0

twice successor 0

let thrice f = f << f << f

thrice successor 0

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
    let cred = ref 0.0
    let pts = ref 0.0
    {
    gpa = fun () -> (((!cred)*(!pts))/(!cred)); 
    credits = fun c -> cred := !cred + c; 
    grade = fun p -> pts := !pts + p
    }

s1.grade 4.0
s1.credits 3.0
s1.grade 3.7
s1.credits 3.0
s1.gpa ()

(*
13. Using imperative F#, create a tuple for an integer stack, including push, pop, top and isEmpty functions. Use the 
    stack to implement factorial. Use a loop to push all the values from 1 through the parameter, then use another loop to 
    pop the values and calculate factorial. Compare the timing with a tail-recursive factorial.
*)

let mkstack init = 
    let stk = ref init
    ((fun x -> stk := x :: (!stk)), // push
        (fun () -> stk := List.tail (!stk)), // pop
        (fun () -> List.head (!stk)), // top
        (fun () -> List.isEmpty (!stk))) // isEmpty

let stack1 = mkstack [1]

(*
14. Refer to the lecture on types and the subset of the C language that was used. Perform a type derivation to verify 
    that the following code is well-typed.
    { int *x;
      int a[15];

      *x = 7;           => can't set an int to a pointer
      a[*x] = *x + 4;
    }

    derivation for *x on left               derivation for 7
    -------------------------               -------------------------


    E(x) = int* var
    ------------------------- (ID)
    E |- x : int* var                   
    ------------------------- (R-VAL)       
    E |- x : int*                                
    ------------------------- (L-VAL)       ------------------------- (ID)
    E |- *x : int var                       E |- x : int              (LIT)
    ---------------------------------------------------- (ASSIGN)
    error


*)

(* Problem 16 *)

(*
18. Measures
    a. Declare type measures for seconds, microseconds, millseconds, and nanoseconds.
    b. Declare constants for the number of seconds in each of the other types.
    c. Create functions that convert seconds to each of the other types. What is the principal type of each function?
    d. Create functions that convert each of the other types to seconds. What is the principal type of each function?
    e. Convert 5000 milliseconds to seconds and then to microseconds.
    f. Convert 0.00000009 seconds to microseconds and to nanoseconds.
*)

[<Measure>] type seconds
[<Measure>] type microseconds
[<Measure>] type milliseconds
[<Measure>] type nanoseconds

let micro = 0.000001<seconds/microseconds>
let milli = 0.001<seconds/milliseconds>
let nano = 0.000000001<seconds/nanoseconds>

let convertSecToMicro s = 
    micro * s

let convertSecToMilli s = 
    milli * s

let convertSecToNano s = 
    nano * s

//let sec1 = convertSecToMicro 5.0<seconds>
//let sec2 = convertSecToMilli 5.0<seconds>
//let sec3 = convertSecToNano 5.0<seconds>

let convertMilliToSec m = 
    m / milli

let convertMicroToNano m = 
    m * 1000.0<microseconds/nanoseconds>

let sec4 = convertMilliToSec 5000.0 |> convertSecToMicro

let sec5 = convertSecToMicro 0.00000009 |> convertMicroToNano
    
