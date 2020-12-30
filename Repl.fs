module Repl

open System
open Parser
open Eval

let readPrompt (prompt: string) =
    Console.Write prompt
    let v = Console.ReadLine()
    if isNull v then "" else v

let rec until pred prompt action =
    let input = prompt()
    if not (pred input) then
      action input 
      until pred prompt action

let readAndEval = readExpr >> Result.bind eval

let evalString expr =
    match readAndEval expr with
    | Ok v -> v.ToString()
    | Error e -> sprintf "Eval failed: %s" (e.ToString())

let evalAndPrint expr = 
  evalString expr |> Console.WriteLine

let runRepl () =
    until ((=) "quit") (fun () -> readPrompt "Lisp>>>") evalAndPrint