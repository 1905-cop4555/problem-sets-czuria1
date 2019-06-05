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
*)

(*
4. Using the natural semantics from the lecture notes, show all the steps for verifying each judgement. 
    [When writing derivations, I tend to set a variable to the current state of memory, to save typing. 
    My derivations are six lines, six lines, and 22 lines.]
    ({i=5; j=8}, i := 2*j + i) => {i=21; j=8}
    ({i=3; j=8}, if (2*i > j) then i := 2*j else j := 2*i) => {i=3; j=6}
    ({i=1; j=10}, while (3*i <= j) do i := 3*i) => {i=9; j=10}

    ({i=5; j=8}, i := 2*j + i) => {i=21; j=8}
    1) ({i=5; j=8}, 2*j + i) => {i=21; j=8}
    2) ({i=5; j=8}, 2*j) => 16
    3) ({i=5; j=8}, j) => 8
    4) ({i=5; j=8}, 2) => 2
    5) ({i=5; j=8}, i) => 5

    ({i=3; j=8}, if (2*i > j) then i := 2*j else j := 2*i) => {i=3; j=6}
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