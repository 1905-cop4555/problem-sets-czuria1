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

let coord_operator = function
    | Tuple -> "Tuple"


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

let syntax = function
    | IF -> "if"
    | THEN -> "then"

syntax IF

(*
11. Record
    Create a record type for Name, Credits and GPA.
    Create a record instance with the values "Jones", 109, 3.85.
*)

// record type
type Student = {Name: string; Credits: int; GPA: float}

// record instance
let s = {Name = "Jones"; Credits = 109; GPA = 3.85}