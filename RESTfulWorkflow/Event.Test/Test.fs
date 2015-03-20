module Lib

//From exsemplet in class
open NUnit.Framework
open FsUnit
//From FsUnit exsampel
open System
open FsUnitDepricated

//From exsemplet in class
let add a b =
    a + b

[<Test>]
let ``add when called with 2 and 4 returns 6``() =
    let res = add 2 4
    res |> should equal 6

//From FsUnit exsampel
[<TestFixture>]
type ``Not a Number tests`` ()=
    [<Test>] member test.
     ``Number 1 should be a number`` ()=
        1 |> should not (be NaN)

    [<Test>] member test.
     ``NaN should not be a number`` ()=
        Double.NaN |> should be NaN

    [<Test>] member test.
     ``float number 2.0 should be a number`` ()=
        2.0 |> should not (be NaN)

    [<Test>] member test.
     ``float number 2.0 should fail to not be a number`` ()=
        shouldFail(fun () -> 2.0 |> should be NaN)
