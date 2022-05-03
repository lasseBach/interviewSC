open System
open FunctionLib


[<EntryPoint>]
let main args =
    match Array.length args with 
    | 1 ->  countWords (args.[0]); 0
    | _ ->  printfn "Supply only one argument"; 1

    
    