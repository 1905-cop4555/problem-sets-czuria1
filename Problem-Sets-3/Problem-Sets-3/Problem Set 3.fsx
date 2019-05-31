(*
1. Building a simple tree.
    a. Create a discriminated union that can represent a linked list of integers.
    b. Write a function that converts a list into a linked list of nodes.
*)
type 'a tree = 
    | Empty 
    | Cons of head:'a * tail:'a tree