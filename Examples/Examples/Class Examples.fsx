﻿// semicolons are similar to eol
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

let list = 1::2::3::[]

// a list of char is allowed
let list2 = ['a'..'z']

// a list of tuples based on the element's index
let list3 = List.init 3 (fun n -> (n, n+n))

let list4 = list.Length
let list5 = list.Head
let list6 = list.Tail

// using the @ sign to append lists
let list7 = ['a';'b';'c']
let list8 = ['d';'e';'f']
let list9 = list7 @ list8

