let mk_expon times one = 
    let rec expon x n = 
        if n = 0 then one
        elif n%2 = 0 then expon (times x x) (n/2)
        else times x (expon x (n-1))
    expon 


let expon1 = mk_expon (*) 1

let expon2 = mk_expon (+) ""