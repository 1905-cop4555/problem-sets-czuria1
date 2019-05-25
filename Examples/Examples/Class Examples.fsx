// in class example
type Person = {Name:string; Age:int}

// {} required to mean record
let p = {Age = 15; Name = "David"}

p.Age

// discriminated unions
type Shape = 
    | Rectangle of width : float * length : float
    | Circle of radiius : float
    | Prism of width : float * float * height : float

// constructing unions
let rect1 = Rectangle(length  = 1.3, width = 10.0)
let rect2 = Rectangle(width = 10.0, length  = 1.3)  // order does not matter

// simple unions
type color = Red | Green | Blue // constructors

let opinion = function 
    //| Red -> "nice" // without this you get a warning
    | Blue -> "lovely"
    | Green -> "ugly"

// trees
type 'a tree = 
    | Lf
    | Br of 'a * 'a tree * 'a tree