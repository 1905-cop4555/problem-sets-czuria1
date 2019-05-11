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
    if n = 0
        then 0
        else m * mult(n-1);

mult (5, 1);;