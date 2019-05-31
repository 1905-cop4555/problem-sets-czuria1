(*
1. Building a simple tree.
    a. Create a discriminated union that can represent a linked list of integers.
    b. Write a function that converts a list into a linked list of nodes.
*)
type tree = 
    | Empty 
    | Cons of head:int * tail:tree

(*
2. This CFG recognizes some strings of zeros and ones.
        S → 0A | 1B
        A → 0AA | 1S | 1
        B → 1BB | 0S | 0 
    a. Describe the strings that the CFG recognizes.
    b. This language is ambiguous. Find a string that is recognized by this grammar which has two derivations.
    c. Show the two derivation trees for the string in part (b).
*)