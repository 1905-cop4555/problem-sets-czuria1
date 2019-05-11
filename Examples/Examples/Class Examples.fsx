let rec power (m,n) = 
    if n = 0
        then 1.0
        else m * power (m, n-1);;

power (2.0, 3.0);;