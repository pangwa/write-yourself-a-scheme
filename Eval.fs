module Eval

open FSharpPlus
open FSharpPlus.Data
open FParsec
open LispTypes

let rec unpackNum =
    function
    | LispNumber v -> Result.Ok v
    | LispString s as v ->
        match run pint64 s with
        | Success (v, _, _) -> Result.Ok v
        | Failure (err, _, _) -> Result.Error(TypeMismatch("number", v))
    | LispList [ n ] -> unpackNum n
    | v -> TypeMismatch("number", v) |> throwError


let numbericBinOp op args =
    match args with
    | [] -> NumArgs (2, []) |> throwError
    | [_] as v -> NumArgs (2, v) |> throwError
    | v ->
         let argsParsed: ThrowsError<List<int64>> = args |> List.map unpackNum |> List.sequence
         Result.bind (fun (head:: tail) -> List.foldM op head tail)
                      argsParsed  |> Result.map LispNumber

let safeMath op a b = op a b |> Result.Ok

let primitives: Map<string, List<LispVal> -> ThrowsError<LispVal>> =
    Map.empty.
        Add("+", numbericBinOp (safeMath (+))).
        Add("-", numbericBinOp (safeMath (-))).
        Add("*", numbericBinOp (safeMath (*))).
        Add("/", numbericBinOp (safeMath (/))).
        Add("mod", numbericBinOp (safeMath (%))).
        Add("quotient", numbericBinOp (safeMath (/))).
        Add("remainder", numbericBinOp (fun a b -> (divRem a b) |> snd |> Result.Ok))

let rec eval = 
  function
  | LispString _ as v -> Result.Ok v
  | LispNumber _ as v -> Result.Ok v
  | LispBool _ as v -> Result.Ok v
  | LispList [ LispAtom "quote"; v ] -> Result.Ok v
  | LispList (LispAtom func:: args) -> args |> mapM eval |> Result.bind (apply func)

and mapM fn =
    function
    | [] -> Result.Ok []
    | x :: xs -> match fn x with
                 | Result.Error e -> Result.Error e
                 | Result.Ok v -> mapM fn xs |> Result.map (fun vs -> v :: vs)
and apply func args =
    Map.tryFind func primitives
    |> Option.toResultWith (NotFunction("Unrecognized primitive function args", func))
    |> Result.bind  (fun f -> f args) 