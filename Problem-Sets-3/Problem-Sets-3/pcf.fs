module Interp

// Skeleton file for PCF interpreter

// This lets us refer to "Parser.Parse.parsefile" simply as "parsefile",
// and to constructors like "Parser.Parse.APP" simply as "APP".
open Parser.Parse

// Here I show you a little bit of the implementation of interp. Note how ERRORs
// are propagated, how rule (6) is implemented, and how stuck evaluations
// are reported using F#'s sprintf function to create good error messages.
let rec interp = function
| APP (e1, e2) ->
    match (interp e1, interp e2) with
    | (ERROR s, _)  -> ERROR s        // ERRORs are propagated
    | (_, ERROR s)  -> ERROR s
    | (SUCC, NUM n) -> NUM (n+1)      // Rule (6)
    | (SUCC, v)     -> ERROR (sprintf "'succ' needs int argument, not '%A'" v)
    | (PRED, NUM n) -> if n = 0 then NUM 0 else NUM (n-1)
    | (PRED, v)     -> ERROR (sprintf "'pred' needs int argument, not '%A'" v)
    | (ISZERO, NUM n) -> if n = 0 then BOOL true else BOOL false
    | (ISZERO, v)     -> ERROR (sprintf "'iszero' needs int argument, not '%A'" v)
| BOOL b    -> BOOL b
| NUM n     -> NUM n
| SUCC      -> SUCC
| PRED      -> PRED
| ISZERO    -> ISZERO
| IF (e1, e2, e3)   ->
    match interp e1 with 
    | BOOL true -> interp e2
    | BOOL false -> interp e3
    | _ -> ERROR (sprintf "'if' needs a condition")
| _ -> ERROR (sprintf "Invalid input")

let rec subst e x t = 
    match e with 
    | NUM n -> NUM n
    | BOOL b ->  BOOL b
    | SUCC -> SUCC
    | PRED -> PRED
    | ISZERO -> ISZERO
    | APP (e1, e2) ->
        match (subst e1 x t, subst e2 x t) with
        |
    | ID i -> if i = x then t else ID i
    | IF (e1, e2, e3) -> IF (subst e1 x t, subst e2 x t, subst e3 x t)
    | FUN (s, e1) -> FUN (s, subst e1 x t)

// Here are two convenient abbreviations for using your interpreter.
let interpfile filename = filename |> parsefile |> interp

let interpstr sourcecode = sourcecode |> parsestr |> interp

let p1() =
    let term = interpstr "succ 0"
    printfn "%A" term
    ()

let p2() =
    let term = interpstr "succ 1"
    printfn "%A" term
    ()

let p3() =
    let term = interpstr "pred 10"
    printfn "%A" term
    ()

let p4() =
    let term = interpstr "succ (succ (succ 0))"
    printfn "%A" term
    ()

let p5() =
    let term = interpstr "iszero succ"
    printfn "%A" term
    ()

let p6() =
    let term = interpstr "succ pred 7"
    printfn "%A" term
    ()

let p7() =
    let term = interpstr "succ (pred 7)"
    printfn "%A" term
    ()

let p8() =
    let prg = interpfile "if.txt"
    printfn "%A" prg
    ()