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

let rec interleave = function
    | [], [] -> []
    | xs, [] -> xs
    | [], ys -> ys
    | x::xs, y::ys -> x::y::interleave(xs,ys)