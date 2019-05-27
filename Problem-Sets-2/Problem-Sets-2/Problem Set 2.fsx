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

type 'a Coordinates = 
    | Tuple of x: 'a * y: 'a
    | Threeple of x: 'a * y: 'a * z: 'a
    | Fourple of x: 'a * y: 'a * z: 'a * w: 'a

// construct union of a tuple of integers
let coor1 = Tuple(x = 1, y = 2)
let coor2 = Threeple(x = 1, y = 2, z = 3)
let coor3 = Fourple(x = 1, y = 2, z = 3, w = 4)

let coord_operator x:Coordinates = List.reduce f x



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

let rec S = function
    | [] -> failwith "premature termination of input"
    | x::xs ->
        match x with  
        | IF -> xs |> S |> eat ID
        | ID -> xs
        | THEN -> x::xs
        | ELSE -> x::xs
        | _ -> failwith (sprintf "S:, want _, got %A" x)

let accept() = printfn("Input accepted")
let error() = printfn("Current general error message")

let test_program program =
          let result = program |> S
          match result with 
          | [] -> failwith "Early termination or missing EOF"
          | x::xs -> if x = EOF then accept() else error()

test_program [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF]


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

(*
11. Record
    Create a record type for Name, Credits and GPA.
    Create a record instance with the values "Jones", 109, 3.85.
*)

// record type
type Student = {Name: string; Credits: int; GPA: float}

// record instance
let s = {Name = "Jones"; Credits = 109; GPA = 3.85}