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

(*
11. Record
    Create a record type for Name, Credits and GPA.
    Create a record instance with the values "Jones", 109, 3.85.
*)

// record type
type Student = {Name: string; Credits: int; GPA: float}

// record instance
let s = {Name = "Jones"; Credits = 109; GPA = 3.85}