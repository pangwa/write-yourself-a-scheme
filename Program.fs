// Learn more about F# at http://fsharp.org

open System
open NUnit.Framework
open FsUnit

[<Test>]
let ``test framework`` () =
    1 |> should equal 1

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
