module Program
// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    Interp.p1()
    Interp.p2()
    Interp.p3()
    Interp.p4()
    Interp.p5()
    Interp.p6()
    Interp.p7()

    0 // return an integer exit code