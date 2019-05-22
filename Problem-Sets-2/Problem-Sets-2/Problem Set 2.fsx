(*
11. Record
    Create a record type for Name, Credits and GPA.
    Create a record instance with the values "Jones", 109, 3.85.
*)

// record type
type Student = {Name: string; Credits: int; GPA: float}

// record instance
let s = {Name = "Jones"; Credits = 109; GPA = 3.85}