// Learn more about F# at http://fsharp.org

open System
open NUnit.Framework
open FsUnit
open LispTypes
open Parser
open Eval

[<EntryPoint>]
let main argv =
    let result = argv |> Array.tryHead |> Option.defaultValue ""
                 |> readExpr |> Result.bind eval
    match result with
    | Result.Ok v -> printfn "evaluated: %s" (v.ToString())
    | Result.Error e -> printfn "error: %s" (e.ToString())
    0 // return an integer exit code
