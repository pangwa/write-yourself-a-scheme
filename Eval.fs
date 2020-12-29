module Eval

open FSharpPlus
open FParsec
open LispTypes

let rec unpackNum =
    function
    | LispNumber v -> v
    | LispString s -> match run pint64 s with
                        | Success (v, _, _) -> v
                        | Failure (err, _, _) -> 0L // parse failed, use 0
    | LispList [n] -> unpackNum n
    | _ -> 0L

let numbericBinOp op args = args |> List.map unpackNum |> List.reduce op |> LispNumber

let primitives: Map<string, List<LispVal> -> LispVal> =
    Map.empty.
        Add("+", numbericBinOp (+)).
        Add("-", numbericBinOp (-)).
        Add("*", numbericBinOp (*)).
        Add("/", numbericBinOp (/)).
        Add("mod", numbericBinOp (%)).
        Add("quotient", numbericBinOp (/)).
        Add("remainder", numbericBinOp (fun a b -> (divRem a b) |> snd))

let rec eval = 
  function
  | LispString _ as v -> v
  | LispNumber _ as v -> v
  | LispBool _ as v -> v
  | LispList [ LispAtom "quote"; v ] -> v
  | LispList (LispAtom func:: args) -> args |> List.map eval |> apply func

and apply func args =
    Map.tryFind func primitives |> Option.map (fun f -> f args) |> Option.defaultWith (fun () -> LispBool false) 