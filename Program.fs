// Learn more about F# at http://fsharp.org

open System
open NUnit.Framework
open FsUnit
open Parser

[<Test>]
let ``test framework`` () =
    1 |> should equal 1

[<EntryPoint>]
let main argv =
    let input = if argv.Length = 0 then "" else argv.[0]
    let result = readExpr input
    printfn "%s\n" result
    0 // return an integer exit code
