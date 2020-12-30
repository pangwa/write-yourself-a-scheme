// Learn more about F# at http://fsharp.org

open System
open Parser
open Eval
open Repl

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        runRepl()
    else
        let result = argv |> Array.tryHead |> Option.defaultValue ""
                     |> readExpr |> Result.bind eval
        match result with
        | Ok v -> printfn "%s" (v.ToString())
        | Error e -> printfn "error: %s" (e.ToString())
    0 // return an integer exit code
