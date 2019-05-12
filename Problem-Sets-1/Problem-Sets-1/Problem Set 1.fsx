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