(*
1. Recall that an F# function that takes two arguments can be coded in either uncurried form (in which case it takes a 
pair as its input) or curried form (in which case it takes the first argument and returns a function that takes the 
second argument). In fact it is easy to convert from one form to the other in F#. To this end, define an F# function 
curry f that converts an uncurried function to a curried function, and an F# function uncurry f that does the opposite 
conversion. For example,
      > (+);;
        val it : (int -> int -> int) = <fun:it@13-7>
        > let plus = uncurry (+);;
        val plus : (int * int -> int)
        > plus (2,3);;
        val it : int = 5
        > let cplus = curry plus;;
        val cplus : (int -> int -> int)
        > let plus3 = cplus 3;;
        val plus3 : (int -> int)
        > plus3 10;;
        val it : int = 13
    What are the types of curry and uncurry?
*)

let uncurry f (a,b) = f a b

(+)

let plus = uncurry (+)

let curry f a b = f (a, b)

let cplus = curry plus

let plus3 = cplus 3

plus3 10

(*
2. Discriminated Union
    a. Create a discriminated union for Coordinates that can be a Tuple, Threeple or Fourple that represent tuples of size two, three and four. The type for the union should be polymorphic.
    b. Instantiate a Tuple of integers, a Threeple of floats and a Fourple of strings.
    c. Create a function that has a parameter of a binary function and Coordinate. Apply the function to the Coordinate like List.reduce.
    d. Call the function with (+) for each of the Coordinates in part (b).
    e. Call the function with (-) for the numeric Coordinates in part (b). Be sure that your function implements the normal associativity for (-).
*)

type 'a Coordinate = 
    | Tuple of x: 'a * y: 'a
    | Threeple of x: 'a * y: 'a * z: 'a
    | Fourple of x: 'a * y: 'a * z: 'a * w: 'a

// construct union of a tuple of integers
let coor1 = Tuple(x = 1, y = 2)
let coor2 = Threeple(x = 1, y = 2, z = 3)
let coor3 = Fourple(x = 1, y = 2, z = 3, w = 4)

// function must take in a parameter of a binary function and a Coordinate
let coord_operator f coor = 
    match coor with 
    | Tuple(x,y) -> f x y
    | Threeple(x,y,z) -> f x (f y z)
    | Fourple(x,y,z,w) -> f x (f y (f z w))

coord_operator (+) coor1
coord_operator (+) coor2
coord_operator (+) coor3

coord_operator (-) coor1
coord_operator (-) coor2
coord_operator (-) coor3


(*
3. In the Notes on Programming Language Syntax page, an example syntax checker for a simple language is given, using C syntax. Write the syntax checker using F#, but you may only use functional programming and immutable date.
    Create the list of tokens as a discriminated union, which (in the simplest case) looks like an enumeration.

    type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF
    With this type declared, you can use the terminals like you would use enumerated values in Java.
    Use immutable data. The C-code example uses mutable data. Pass the program into the start symbol function. Pass the input yet to be processed to each non-terminal function.

    The main function might look like this:

        let test_program program =
          let result = program |> S
          match result with 
          | [] -> failwith "Early termination or missing EOF"
          | x::xs -> if x = EOF then accept() else error()
    You do not have to tokenize the input strings. Assume that the tokenizing has been done. Pass a list of tokens that represent a program into the start symbol. Try these program examples:

    [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF]

    [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
          
    Causes error: 
    [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]
    Print an accept message when the input is valid and completely consumed. Generate appropriate error messages for incorrect symbols, not enough input, and too much input.
*)

type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF

let eat token = function
    | [] -> failwith "premature termination of input"
    | x::xs ->
        if x = token
        then xs
        else failwith (sprintf "want %A, got %A" token x)

let E = eat ID

let rec S = function
    | [] -> failwith "premature termination of input"
    | x::xs ->
        let rec L tok = function
            | END::xs -> xs 
            | SEMICOLON::xs -> xs |> S |> L xs
            | _ -> failwith (sprintf "L: got %A" tok)
        match x with  
        | IF -> xs |> E |> eat THEN |> S |> eat ELSE |> S
        | BEGIN -> xs |> S |> L xs
        | PRINT -> xs |> E
        | _ -> failwith (sprintf "S: got %A" x)

let accept() = printfn("Input accepted")
let error() = printfn("Current general error message")

let test_program program =
          let result = program |> S
          match result with 
          | [] -> failwith "Early termination or missing EOF"
          | x::xs -> if x = EOF then accept() else error()

test_program [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF]

test_program [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]

// did get an error
test_program [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]


(*
4. Implement a syntax checker using functional programming and immutable data for the unambiguous grammar for arithmetic expressions, from the Notes on Programming Language Syntax.
        E -> E + T | E - T | T
        T -> T * F | T / F | F
        F -> i | (E)
    Use the suggestion in the notes to get around the fact that this grammar appears to need more than one lookahead token.

    You do not have to tokenize the input strings. Assume that the tokenizing has been done. Pass a list of tokens that represent a program into the start symbol. Try these program examples:

        test_program [ID;ADD;ID;ADD;ID;ADD;ID;EOF]
        test_program [ID;SUB;ID;MUL;ID;EOF]
        test_program [LPAREN;ID;SUB;ID;RPAREN;MUL;ID;EOF] 
*)

//let rec E = function
    //| [] -> failwith "premature termination of input"
    //| x::xs ->
        //let F = function
        //    | LPAREN::xs -> xs |> E
        //    | RPAREN::xs -> xs
        //    | _ -> failwith (sprintf "F: got %A" x)
        //let rec T = function
        //    | MUL::xs -> xs |> T |> F
        //    | DIV::xs -> xs |> T |> F
        //    | _ -> failwith (sprintf "T: got %A" x)
        //match x with 
        //| ADD -> xs |> E |> T xs
        //| SUB -> xs |> E |> T xs
        //| _ -> failwith (sprintf "E: got %A" x)

(*
E -> E + T | T
T -> i
          E
        / | \
       E  +  T
      /       \
     T         i
    /
   i

*)
type TERMINAL = ID|ADD|SUB|MUL|DIV|LPAREN|RPAREN|EOF

let eat token = function
    | [] -> failwith "premature termination of input"
    | x::xs ->
        if x = token
        then xs
        else failwith (sprintf "want %A, got %A" token x)

let T = eat ID

let rec E = function
    | [] -> failwith "premature termination of input"
    | x::xs ->
        match x with 
        | ID -> xs
        | ADD -> xs |> T |> E
        | _ -> failwith (sprintf "E: got %A" x)

let accept() = printfn("Input accepted")
let error() = printfn("Input error")

let test_program program =
          let result = program |> E
          match result with 
          | [] -> failwith "Early termination or missing EOF"
          | x::xs -> if x = EOF then accept() else error()

// 1 + 2
test_program [ID;ADD;ID;EOF]

(*
5. Given vectors u = (u1, u2,..., un) and v = (v1, v2,..., vn), the inner product of u and v is defined to be u1*v1 + u2*v2 + ... + u n*vn. 
Write a curried F# function inner that takes two vectors represented as int list and returns their inner product.
    Throw an exception if the lists do not have the same length.
    Do not use any built-in or borrowed functions. Write it from scratch.
    Use big integers.
    Write a version without using tail recursion.
    Write another version using tail recursion.
    Try both versions on the input [1I..50000I] [50001I..100000I]. Increase the ranges until you get stack overflow on the non-tail-recursive version.
      > inner [1;2;3] [4;5;6];;
        val it : int = 32
*)

let rec inner x y = 
    match x,y with 
    | [], [] -> 0
    | [], _ -> failwith ("Vector lists are not of the same length")
    | _, [] -> failwith ("Vector lists are not of the same length")
    | x::xs, y::ys -> x * y + inner xs ys

inner [1;2;3] [4;5;6]

let rec innerTail x y tail = 
    match x,y with 
    | [], [] -> tail
    | [], _ -> failwith ("Vector lists are not of the same length")
    | _, [] -> failwith ("Vector lists are not of the same length")
    | x::xs, y::ys -> innerTail xs ys (tail + x * y)

innerTail [1;2;3] [4;5;6] 0

(*
6. Given an m-by-n matrix A and an n-by-p matrix B, the product of A and B is an m-by-p matrix whose entry in 
position (i,j) is the inner product of row i of A with column j of B. For example,
                  / 0 1 \
    / 1 2 3 \  *  | 3 2 |  =  /  9 11 \
    \ 4 5 6 /     \ 1 2 /     \ 21 26 /
    Write an uncurried F# function to do matrix multiplication:

      > multiply ([[1;2;3];[4;5;6]], [[0;1];[3;2];[1;2]]);;
        val it : int list list = [[9; 11]; [21; 26]]
    Assume that the dimensions of the matrices are appropriate.

    Hint: Use transpose (from Problem Set 1), inner, and List.map.
*)

let rec transpose = function
    | [[];_] -> []
    | m -> List.map List.head m :: transpose(List.map List.tail m)

let rec inner x y = 
    match x,y with 
    | [], [] -> 0
    | x::xs, y::ys -> x * y + inner xs ys

let rec multiply m =
    match m with 
    | _, [] -> []
    | [], _ -> []
    | x::xs,y::ys -> 
        let newList = transpose ys
        let prodList = List.map (fun y -> inner x y) newList
        prodList::multiply(xs,ys)

multiply ([[1;2;3];[4;5;6]], [[0;1];[3;2];[1;2]])

(*
7. Evaluate the asymptotic time complexity of this function:
    let rec oddeven = function
    | [] -> []                              // O(1)
    | x::xs -> if x % 2 = 0                 // O(1)
               then oddeven xs @ [x]        // O(n^2)
               else x :: oddeven xs         // O(n) based on size of list xs

    Time complexity of O(n^2)
*)

(*
8. Two powerful List functions provided by F# are List.fold and List.foldBack. These are similar to List.reduce and 
   List.reduceBack, but more general. Both take a binary function f, an initial value i, and a list [x1;x2;x3;...;xn]. Then List.fold returns
      (f ... (f (f (f i x1) x2) x3) ... xn)
    while List.foldBack returns
      (f x1 (f x2 (f x3 ... (f xn i) ... )))
    In spite of this complicated behavior, they can be implemented very simply:
      > let rec fold f a = function
        | []    -> a
        | x::xs -> fold f (f a x) xs;;

      val fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

      > let rec foldBack f xs a =
          match xs with
          | []    -> a
          | y::ys -> f y (foldBack f ys a);;

      val foldBack : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
    (Note that they don't take their arguments in the same order.)
    Each of these functions can be used to implement flatten, which "flattens" a list of lists:

      let flatten1 xs = fold (@) [] xs

      let flatten2 xs = foldBack (@) xs []
    For example,

      > flatten1 [[1;2];[];[3];[4;5;6]];;
      val it : int list = [1; 2; 3; 4; 5; 6]
    Compare the efficiency of flatten1 xs and flatten2 xs, both in terms of asymptotic time compexity and experimentally. 
    To make the analysis simpler, assume that xs is a list of the form [[1];[2];[3];...;[n]]. Use your version of fold and 
    foldBack to experiment with the time complexity.
*)

let rec fold f a = function
        | []    -> a                    // O(1)
        | x::xs -> fold f (f a x) xs    // O(n + 1) * O(n + 1) = n^2 + 1

// O(1) * O(n^2 + 1) = O(n^2)

let rec foldBack f xs a =
          match xs with
          | []    -> a                      // O(1)
          | y::ys -> f y (foldBack f ys a)  // O(n + 1)

// O(1) * O(n + 1) = O(n)

let flatten1 xs = fold (@) [] xs

let flatten2 xs = foldBack (@) xs []

flatten1 [[1;2];[];[3];[4;5;6]]

(*
9. The built-in discriminated union
      type 'a option = None | Some of 'a
    is useful when handling invalid input. For example, a function that returns the last element in a list cannot return an element for the empty list. One possibility is to raise an exception. Another possibility is to use the option discriminated union. When the function is passed an empty list, the function will return None. When a non-empty list is passed to the function, it will return Some x, where x is the last element in the list.

    Write a recursive function that returns the last element in its list parameter, using the option type to handle invalid input. You may not use reduce. Do not reverse the list. Do not index into the list. Use recursion.

    Write a helper function that converts an option to a string. If the option is None then return "Invalid Input", otherwise use sprintf to convert the value in the option to a string.

    Write a test function that calls the above function with an empty list, a list of one element, and a list of multiple elements. Display an appropriate message to the terminal after each function call. The output of the test function should be similar to

    The last element of [] is "Invalid Input"
    The last element of ["cat"] is "cat"
    The last element of [1; 2; 3; 4; 5] is 5
*)

type 'a option = None | Some of 'a

let op = function
    | None -> "Invalid input"
    | Some x -> sprintf ("%A") x

let rec last = function
    | [] -> None
    | [x] -> Some x
    | x::xs -> last xs

last []
last ["cat"]
last [1; 2; 3; 4; 5]

(*
10. Interpreter 0 In this problem, we begin our exploration of the use of F# for language-oriented programming. 
    You will write an F# program to evaluate arithmetic expressions written in the language given by the following context-free grammar:
    E -> n | -E | E + E | E - E | E * E | E / E | (E)
*)

type Exp =
    Num of int
  | Neg of Exp
  | Sum of Exp * Exp
  | Diff of Exp * Exp
  | Prod of Exp * Exp
  | Quot of Exp * Exp

type 'a option = None | Some of 'a

let rec evaluate = function
    | Num n -> Some n
    | Neg x ->
        match evaluate x with
        | None -> None
        | Some x -> Some -x
    | Sum (x,y) ->
        match evaluate x, evaluate y with 
        | _, None -> None
        | None, _ -> None
        | Some x, Some y -> Some (x + y)
    | Diff (x,y) ->
        match evaluate x, evaluate y with 
        | _, None -> None
        | None, _ -> None
        | Some x, Some y -> Some (x - y) 
    | Prod (x,y) ->
        match evaluate x, evaluate y with 
         | _, None -> None
         | None, _ -> None
         | Some x, Some y -> Some (x * y)  
    | Quot (x,y) ->
        match evaluate x, evaluate y with 
         | _, None -> None
         | None, _ -> None
         | Some x1, Some y1 ->
             if evaluate y = Some 0 then None
             else match evaluate x, evaluate y with 
                 | _, None -> None
                 | None, _ -> None
                 | Some x, Some y -> Some (x / y)

evaluate (Prod(Num 3, Diff(Num 5, Num 1)))
evaluate (Diff(Num 3, Quot(Num 5, Prod(Num 7, Num 0))))

(*
11. Record
    Create a record type for Name, Credits and GPA.
    Create a record instance with the values "Jones", 109, 3.85.
*)

// record type
type Student = {Name: string; Credits: int; GPA: float}

// record instance
let s = {Name = "Jones"; Credits = 109; GPA = 3.85}

(*
12. Binary Search Tree
    Write a function that searches for a value in a binary search tree and then removes that node. Be sure to handle all of these cases:
        * The value is not in the tree.
        * Both children are Lf.
        * One child node is a Br the other is a Lf.
        * Both children are Br nodes.
*)

let rec delete n = function
    | Lf -> Lf
    | Br(m, t1, t2) when n = m -> ?
    | Br(m, t1, t2) ->
        if n < m then Br(m, delete n t1, t2)
        else Br(m, t1, delete n t2)

(*
13. Building Parse Trees
    Modify problem 3 so that it builds a parse tree as it processes input. On valid input, display the generated tree.
    Modify problem 4 so that it builds a parse tree as it processes input. On valid input, display the generated tree.
*)

type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF

let eat token = function
    | [] -> failwith "premature termination of input"
    | x::xs ->
        if x = token
        then xs
        else failwith (sprintf "want %A, got %A" token x)

let E = eat ID

let rec S = function
    | [] -> failwith "premature termination of input"
    | x::xs ->
        let rec L tok = function
            | END::xs -> xs 
            | SEMICOLON::xs -> xs |> S |> L xs
            | _ -> failwith (sprintf "L: got %A" tok)
        match x with  
        | IF -> xs |> E |> eat THEN |> S |> eat ELSE |> S
        | BEGIN -> xs |> S |> L xs
        | PRINT -> xs |> E
        | _ -> failwith (sprintf "S: got %A" x)

let accept() = printfn("Input accepted")
let error() = printfn("Current general error message")

let test_program program =
          let result = program |> S
          match result with 
          | [] -> failwith "Early termination or missing EOF"
          | x::xs -> if x = EOF then accept() else error()