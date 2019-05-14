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

//List.head [1;2;3];;
//List.tail [1;2;3];;

//List.map (fun n -> n+n) [1;2;3;4];;
//List.map (fun n -> n*n) [1;2;3;4];;
//List.map (fun n -> n=n+1) [1;2;3;4];;

//List.filter (fun x -> x%2 = 0) [1;2;3;4;5];;
//List.filter (fun x -> x%2 > 0) [1;2;3;4;5];;

//List.reduce (*) [1..5];;
//List.reduce (+) [1..5];;
//List.reduce (-) [1..5];;

//let list = 1::2::3::[]

// a list of char is allowed
//let list2 = ['a'..'z']

// a list of tuples based on the element's index
//let list3 = List.init 3 (fun n -> (n, n+n))

//let list4 = list.Length
//let list5 = list.Head
//let list6 = list.Tail

// using the @ sign to append lists
//let list7 = ['a';'b';'c']
//let list8 = ['d';'e';'f']
//let list9 = list7 @ list8

//printfn "The appended list: %A" list9

//let rec prod ms = 
//    match ms with
//    | [] -> 1
//    | n::ms -> n * prod ms

//prod [2;3]

//let rec sum ms = 
//    match ms with
//    | [] -> 0
//    | n::ms -> n + sum ms

//sum [1..5];;

//let foo = [[1;2;3]]
//let baz = [[4;5;6]]

//List.head foo

//List.map List.tail foo @ baz

//List.map List.head foo @ baz

// class example pattern matching using function keyword
let rec prod = function
    | [] -> 1
    | n::ns -> n * prod ns

prod []
prod [1;2]
prod [2;4;6]

// class example pattern matching using function keyword
// which returns a list of numbers counting down from input number
let rec downFrom = function
    | 0 -> []
    | n -> n :: downFrom(n-1)

downFrom 6

// question from problem set
// input is a tuple of a list and a list
let rec foo = function
    | (xs, [])    -> xs
    | (xs, y::ys) -> foo (xs@[y], ys)

foo ([1], [])

let swap (a, b) = (b, a);;

swap(1,2)
