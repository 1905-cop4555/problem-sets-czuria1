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
11. What type does F# infer for the expression (3, [], true) ? Select one:
    int * 'a list * bool <==
    int * 'a * bool
    int * int list * bool
    Type error.
*)

(*
12. What type does F# infer for the expression fun x y -> x+y+"." ? Select one:
    string -> string -> string <==
    string * string -> string
    Type error.
    int -> int -> string
*)

(*
13. What type does F# infer for the expression fun xs -> List.map (+) xs ? Select one:
    int list -> int -> int list
    int list -> int list
    Type error.
    int list -> (int -> int) list <==
*)

fun xs -> List.map (+) xs
fun a -> List.map (*) [1;2;3] // a -> (int -> int) list

(*
14. Which of the following does F# infer to have type string -> string -> string ? Select one:
    fun x -> fun y -> x y "."
    fun x y -> String.length x * String.length y
    fun (x, y) -> x + y + "."
    (+)
     
    fun x y -> x + " " + y <== 
*)

// the following infers string -> string -> string because of + " "
fun x y -> x + " " + y

(*
15. Which of the following does F# infer to have type (string -> string) -> string ? Select one:
    fun f -> String.length (f "cat")
    fun x y -> x + " " + y
    fun f -> f (f "cat") <=
    fun f -> f "cat"
*)

// the following infers (string -> string) -> string because of precedence
fun f -> f (f "cat")

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

//let rec gencut = function
    //| n, [] -> []
    //| 0, xs -> xs
    //| n, x::xs -> x::gencut(n-1,xs)

//let rec gencut = function
    //| n, [] -> [], []
    //| 0, xs -> [], xs
    //| n, x::xs -> [x], gencut(n-1, xs)

let gencut(n, right) =
    let rec gencutHelper (n, left, right) = 
        match n, left, right with 
        | 0, left, right -> List.rev left, right
        | n, left, [] -> left, []
        | n, left, right::rtail -> gencutHelper(n-1, right::left, rtail)
    gencutHelper(n, [], right)

//gencut(2, [1;3;4;2;7;0;9]);;

let cut list = 
    let n = List.length list / 2
    gencut(n, list)

cut [1;2;3;4;5;6];;

(*
20. Write an F# function shuffle xs that takes an even-length list, cuts it into two equal-sized pieces, and then 
    interleaves the pieces:
  > shuffle [1;2;3;4;5;6;7;8];;
        val it : int list = [1; 5; 2; 6; 3; 7; 4; 8]
    (On a deck of cards, this is called a perfect out-shuffle.)
*)

//let rec interleave2 = function
//    | [], [] -> []
//    | xs, [] -> xs
//    | [], ys -> ys
//    | x::xs, y::ys -> x::y::interleave2(xs,ys)

//let gencut2(n, right) =
//    let rec gencutHelper (n, left, right) = 
//        match n, left, right with 
//        | 0, left, right -> List.rev left, right
//        | n, left, [] -> left, []
//        | n, left, right::rtail -> gencutHelper(n-1, right::left, rtail)
//    gencutHelper(n, [], right)

//let cut2 list = 
//    let n = List.length list / 2
//    gencut2(n, list)

//let shuffle list =
    //let halves = cut2 list
    //interleave2 halves

//shuffle [1;2;3;4;5;6;7;8];;

(*
21. Write an F# function countshuffles n that counts how many calls to shuffle on a deck of n distinct "cards" it takes 
    to put the deck back into its original order:
  > countshuffles 4;;
        val it : int = 2
    (To see that this result is correct, note that shuffle [1;2;3;4] = [1;3;2;4], and shuffle [1;3;2;4] = [1;2;3;4].) 
    What is countshuffles 52?
    Hint: Define an auxiliary function countaux(deck, target) that takes two lists and returns the number of shuffles 
    it takes to make deck equal to target.
*)

let rec interleave2 = function
    | [], [] -> []
    | xs, [] -> xs
    | [], ys -> ys
    | x::xs, y::ys -> x::y::interleave2(xs,ys)

let gencut2(n, right) =
    let rec gencutHelper (n, left, right) = 
        match n, left, right with 
        | 0, left, right -> List.rev left, right
        | n, left, [] -> left, []
        | n, left, right::rtail -> gencutHelper(n-1, right::left, rtail)
    gencutHelper(n, [], right)

let cut2 list = 
    let n = List.length list / 2
    gencut2(n, list)

let shuffle list =
    let halves = cut2 list
    interleave2 halves

let countshuffles n = 
    let countaux(deck, target) =
        let rec countauxHelper (deck, target, count) =
           if deck = target then count
           else countauxHelper(shuffle deck, target, count+1)
        countauxHelper(shuffle deck, target, 1)
    countaux([1..n], [1..n])
 

//countaux([1;2;3;4],[1;3;2;4]);;

//countshuffles 4;;

countshuffles 52;;

(*
22. Write an uncurried F# function cartesian (xs, ys) that takes as input two lists xs and ys and returns a list of 
    pairs that represents the Cartesian product of xs and ys. (The pairs in the Cartesian product may appear in any 
    order.) For example,
  > cartesian (["a"; "b"; "c"], [1; 2]);;
        val it : (string * int) list =
        [("a", 1); ("b", 1); ("c", 1); ("a", 2); ("b", 2); ("c", 2)]
*)

let rec cartesian = function
    | xs, [] -> []
    | xs, y::ys -> List.map (fun x -> (x,y)) xs :: cartesian(xs, ys)

cartesian (["a"; "b"; "c"], [1; 2]);;

(*
23. An F# list can be thought of as representing a set, where the order of the elements in the list is irrelevant. Write an F# function powerset such that powerset set returns the set of all subsets of set. For example,
  > powerset [1;2;3];;
        val it : int list list
        = [[]; [3]; [2]; [2; 3]; [1]; [1; 3]; [1; 2]; [1; 2; 3]]
*)

let rec powerset = function
    | [] -> [[]]
    | x::xs -> List.map (fun y -> x::y) (powerset xs) @ powerset xs 

powerset [1;2;3];;

(*
24. The transpose of a matrix M is the matrix obtained by reflecting Mabout its diagonal. For example, the transpose of
     / 1 2 3 \
     \ 4 5 6 /

    is
     / 1 4 \
     | 2 5 |
     \ 3 6 /

    An m-by-n matrix can be represented in F# as a list of m rows, each of which is a list of length n. For example, 
    the first matrix above is represented as the list
      [[1;2;3];[4;5;6]]

    Write an efficient F# function to compute the transpose of an m-by-nmatrix:
      > transpose [[1;2;3];[4;5;6]];;
            val it : int list list = [[1; 4]; [2; 5]; [3; 6]]

    Assume that all the rows in the matrix have the same length.
*)

let rec transpose = function
    | [[];_] -> []
    | m -> List.map List.head m :: transpose(List.map List.tail m)

transpose [[1;2;3];[4;5;6]];;