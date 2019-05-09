(*// Copy example
let rec fib1 = function 
    | 0 -> 0I
    | 1 -> 1I
    | n -> fib1(n-1) + fib1(n-2)

// Attempt at match of the same function
let rec fib2 n =
    match n with
    | 0 -> 0I
    | 1 -> 1I
    | n -> fib1(n-1) + fib1(n-2)*)

let mk_expon times one = 
    let rec expon x n = 
        if n = 0 then one
        elif n%2 = 0 then expon (times x x) (n/2)
        else times x (expon x (n-1))
    expon

// Rewrite mk_expon using match
let mk_expon2 times n = 
    match n with
    | 0 = 1
    | n%2 = 0
    expon

let expon1 = mk_expon (*) 3

