module Repl

open System
open LispTypes
open Parser
open Eval

let readPrompt (prompt: string) =
    Console.Write prompt
    let v = Console.ReadLine()
    if isNull v then "" else v

let rec until pred prompt action =
    let input = prompt ()

    if not (pred input) then
        action input
        until pred prompt action

let readAndEval env = readExpr >> Result.bind (eval env)

let evalString env expr =
    match readAndEval env expr with
    | Ok v -> v.ToString()
    | Error e -> sprintf "Eval failed: %s" (e.ToString())

let evalAndPrint env expr =
    evalString env expr |> Console.WriteLine

let runOne args = 
  let env = List.tail args |> List.map LispString |> LispList
            |> fun x -> Map.add "args" x (Map.empty) 
            |> bindVars (primitiveBindings())
  match eval env (LispList [LispAtom "load"; LispString (args.[0])]) with
  | Ok v -> Console.WriteLine(v)
  | Error e -> sprintf "Eval failed: %s" (e.ToString()) |> Console.WriteLine

let runRepl () =
    until ((=) "quit") (fun () -> readPrompt "Lisp>>>") (evalAndPrint (primitiveBindings ()))
