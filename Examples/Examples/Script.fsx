let rec fib1 = function 
    | 0 -> 0I
    | 1 -> 1I
    | n -> fib1(n-1) + fib1(n-2)

fib1 6