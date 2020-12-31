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
        argv |> List.ofArray |>runOne 
    0 // return an integer exit code
