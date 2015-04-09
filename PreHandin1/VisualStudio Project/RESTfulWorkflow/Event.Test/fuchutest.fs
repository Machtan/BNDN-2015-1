module test
open Fuchu

let ``Two is two`` = fun () -> Assert.Equal("2 = 2", 2, 2)

let ``This should fail`` =
    failtest "Nooooo it faiiiiiiled"

[<Tests>]
let test =
    testList "Results" [
        testCase "2=2" ``Two is two``;
        testCase "fail" ``This should fail``
    ]

//[<EntryPoint>]
//let main args =
