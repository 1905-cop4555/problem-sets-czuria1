// semicolons are similar to eol
//[1; 2; 3];;

// another way of writing a list
//[
//1
//2
//3];;

//["hello" ; "world"];;

//[0..9];;

//[[1];[6..12];[]];;

// can't mix types in list
//[[1];["a";"b"];[]]

List.head [1;2;3];;
List.tail [1;2;3];;

List.map (fun n -> n+n) [1;2;3;4];;
List.map (fun n -> n*n) [1;2;3;4];;
List.map (fun n -> n=n+1) [1;2;3;4];;

List.filter (fun x -> x%2 = 0) [1;2;3;4;5];;
List.filter (fun x -> x%2 > 0) [1;2;3;4;5];;

List.reduce (*) [1..5];;
List.reduce (+) [1..5];;
List.reduce (-) [1..5];;