

let rec slowfib n = if n<2 then 1.0 else slowfib(n-1) + slowfib(n-2)

// make two slowfib async
let fibs n =
    let tasks = [ async { return slowfib(n - 1)};
                  async { return slowfib(n)}]
    Async.RunSynchronously (Async.Parallel tasks)

// makes n slowfib async
let fibs2 n =
    let tasks = 
        [ for i in 0..n do yield async { return slowfib(i)}]
    Async.RunSynchronously (Async.Parallel tasks)

// makes n slowfib async
let fibs3 n =
    Array.Parallel.init (n+1) slowfib



[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
