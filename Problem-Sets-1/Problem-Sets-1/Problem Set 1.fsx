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
    They are immutable.
    Their built-in functions are polymorphic.
    They can be of any length.
    They can be heterogeneous.
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
7. How does F# interpret the type int * bool -> string list? Select one:
    (int * (bool -> string)) list
    ((int * bool) -> string) list
    int * (bool -> (string list))
    (int * bool) -> (string list)
*)

(*
8.Let F# function foo be defined as follows:
    let rec foo = function
            | (xs, [])    -> xs
            | (xs, y::ys) -> foo (xs@[y], ys)

    If foo is supposed to append its two list parameters, which of the following is true? Select one:

    foo fails Step 1 of the Checklist for Programming with Recursion.
    foo fails Step 2 of the Checklist for Programming with Recursion.
    foo fails Step 3 of the Checklist for Programming with Recursion.
    foo satisfies all three steps of the Checklist for Programming with Recursion.
*)

let rec foo = function
            | (xs, [])    -> xs
            | (xs, y::ys) -> foo (xs@[y], ys)

foo ([1], [2;3])