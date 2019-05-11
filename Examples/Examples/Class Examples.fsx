//let rec power (m,n) = 
//    if n = 0
//        then 1.0
//        else m * power (m, n-1);;

//power (2.0, 3);;

//let rec sum (n) = 
//    if n = 0
//        then 0
//        else n + sum(n-1);

//// Expected output = 6
//sum (3);;

//// Expected output = 45
//sum (9);;

let rec mult (m, n) = 
    if n = 1
        then m
        else m + mult(m, n-1);

// Expected output = 5
mult (5, 1);;

// Expected output = 20
mult (4, 5);;

// Expected output = -3
mult (3, -1);;