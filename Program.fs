// Learn more about F# at http://fsharp.org

open System
open NUnit.Framework
open FsUnit
open LispTypes
open Parser
open Eval

[<EntryPoint>]
let main argv =
    argv |> Array.tryHead |> Option.defaultValue "" |>
    readExpr |> eval |> (fun v -> v.ToString()) |> printfn "%s\n"
    0 // return an integer exit code
