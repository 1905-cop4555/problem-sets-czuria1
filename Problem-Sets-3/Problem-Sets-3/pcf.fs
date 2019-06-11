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

// Here are two convenient abbreviations for using your interpreter.
let interpfile filename = filename |> parsefile |> interp

let interpstr sourcecode = sourcecode |> parsestr |> interp

let result() =
    let term = interpstr "succ 0"
    printfn "%A" term
    ()