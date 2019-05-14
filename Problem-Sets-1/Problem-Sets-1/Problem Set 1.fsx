(* 
1. Which of the following F# expressions is not well typed? Select one:
    2 + 5 * 10
    10I * 20I
    4 + 5.6 <<==
    "4" + "5.6"
    *)

2 * 5 * 10

10I * 20I

// must match on type
// 4 + 5.6
4.0 + 5.6

//"4" + "5.6"

(*
2. A curried function has a type of which form? Select one:
    t1 * t2 -> t3
    t1 -> t2 * t3
    t1 -> (t2 -> t3) <==
    (t1 -> t2) -> t3
*)

(*
3. If an F# function has type 'a -> 'b when 'a : comparison, which of the following is not a legal type for it? Select one:
    (float -> float) -> bool
    string -> (int -> int)
    int -> int <==
    int list -> bool list

*)

let comparison func n m =
    func n m

let equal x y = 
    if (x = y) then
        true
    else 
        false

let result = comparison equal 1 2;;

(*
4. Which of the following statements about F# lists is not true? Select one:
    They are immutable. (TRUE)
    Their built-in functions are polymorphic. (TRUE)
    They can be of any length. (TRUE)
    They can be heterogeneous. (FALSE) <==
*)

(*
5. Which of the following F# expressions evaluates to [1; 2; 3]? Select one:
    1::2::3::[] <==
    1@2@3@[]
    [1; 2; 3]::[]
    ((1::2)::3)::[]
*)

// appends using cons
1::2::3::[]

// the following expects a list
//1@2@3@[]

// the following appends a list within the empty list
[1; 2; 3]::[]

// the following is not a list so there is an error
//((1::2)::3)::[]

(*
6. How does F# interpret the expression List.map List.head foo @ baz? Select one:
    (List.map List.head) (foo @ baz)
    ((List.map List.head) foo) @ baz
    List.map (List.head (foo @ baz))
    (List.map (List.head foo)) @ baz <==
*)

//let foo = [[1;2;3]]
//let baz = [4;5;6]

// the following requires an input of a list list / why?
//List.map List.head foo @ baz

(*
 Precedence of type constructors:
    1) list
    2) *
    3) -> (this associates to the right)

    int * bool -> (string list)
    (int * bool) -> (string list)

7. How does F# interpret the type int * bool -> string list? Select one:
    (int * (bool -> string)) list
    ((int * bool) -> string) list
    int * (bool -> (string list))
    (int * bool) -> (string list) <==
*)

(*
8.Let F# function foo be defined as follows:
    let rec foo = function
            | (xs, [])    -> xs
            | (xs, y::ys) -> foo (xs@[y], ys)

    If foo is supposed to append its two list parameters, which of the following is true? Select one:

    foo fails Step 1 of the Checklist for Programming with Recursion. <== ??
    foo fails Step 2 of the Checklist for Programming with Recursion.
    foo fails Step 3 of the Checklist for Programming with Recursion.
    foo satisfies all three steps of the Checklist for Programming with Recursion.
*)

// function fails, why?
//let rec foo = function
//            | (xs, [])    -> xs
//            | (xs, y::ys) -> foo (xs@[y], ys)

//foo ([1], [2;3])

(*
9. Which of the following is the type that F# infers for (fun f -> f 17)? Select one:
    ('a -> 'b) -> 'b
    (int -> int) -> int 
    (int -> 'a) -> 'a <==
    ('a -> 'a) -> 'a
*)

// input type is an int and function then determines the type returned
let infer f =
    f 17

(*
10. Which of the following has type int -> int list? Select one:
    (@) [5]
    [fun x -> x+1]
    fun x -> 5::x
    fun x -> x::[5] <==
*)


// returns an int list, but requires an int list to append to
(@) [5]


// returns a  tuple
[fun x -> x+1]

// this one requires a list
fun x -> 5::x

// this one only requires an int as input type to append
// to the int list
fun x -> x::[5]

(*
17. Write an F# function revlists xs that takes a list of lists xs and reverses all the sub-lists:
  > revlists [[0;1;1];[3;2];[];[5]];;
        val it : int list list = [[1; 1; 0]; [2; 3]; []; [5]]
*)

//let revlists = fun list -> List.map List.rev list

// another way to write the same function 
let revlists list = List.map List.rev list 

revlists [[0;1;1];[3;2];[];[5]];;

(*
18. Write an F# function interleave(xs,ys) that interleaves two lists:
  > interleave ([1;2;3],[4;5;6]);;
        val it : int list = [1; 4; 2; 5; 3; 6]
    Assume that the two lists have the same length.
*)

//let rec interleave = function
    //| (xs, []) -> xs
    //| ([], ys) -> ys
    //| (x::xs, y::ys) -> x::y::interleave(xs, ys)

let rec interleave = function
    | [], [] -> []
    | xs, [] -> xs
    | [], ys -> ys
    | x::xs, y::ys -> x::y::interleave(xs,ys) // () is necessary because it inputs a tuple


interleave ([1;2;3],[4;5;6]);;

(*
19. Write an F# function cut xs that cuts a list into two equal parts:
  > cut [1;2;3;4;5;6];;
        val it : int list * int list = ([1; 2; 3], [4; 5; 6])
Assume that the list has even length.
To implement cut, first define an auxiliary function gencut(n, xs) that cuts xs into two pieces, where n gives the 
size of the first piece:

  > gencut(2, [1;3;4;2;7;0;9]);;
        val it : int list * int list = ([1; 3], [4; 2; 7; 0; 9])
*)

let rec gencut = function
    | n, [] -> []
    | 0, xs -> xs
    | n, x::xs -> x::gencut(n-1,xs)

gencut(2, [1;3;4;2;7;0;9]);;