module Eval

open FSharpPlus
open FSharpPlus.Data
open FParsec
open LispTypes
open Parser

let readOrThrow parser input =
    match run parser input with
    | Success (v, _, _) -> Result.Ok(v)
    | Failure (err, _, _) ->
        sprintf "No match %s" err
        |> ParseError
        |> throwError

let readExpr = readOrThrow (spaces >>. parseExpr .>> spaces .>> eof)
let readExprList = readOrThrow (sepEndBy parseExpr spaces)

let rec unpackNum =
    function
    | LispNumber v -> Result.Ok v
    | LispString s as v ->
        match run pint64 s with
        | Success (v, _, _) -> Result.Ok v
        | Failure (err, _, _) -> Result.Error(TypeMismatch("number", v))
    | LispList [ n ] -> unpackNum n
    | v -> TypeMismatch("number", v) |> throwError

let boolBinop unpacker op (args: List<LispVal>) =
    if args.Length <> 2 then
        NumArgs(2, args) |> throwError
    else
        monad {
            let! left = args.[0] |> unpacker
            let! right = args.[1] |> unpacker
            return op left right |> LispBool
        }

let unpackStr =
    function
    | LispString s -> Result.Ok s
    | LispNumber v -> sprintf "%d" v |> Result.Ok
    | LispBool v -> v.ToString() |> Result.Ok
    | notString -> TypeMismatch("string", notString) |> throwError

let unpackBool =
    function
    | LispBool v -> Result.Ok v
    | notBool -> TypeMismatch("boolean", notBool) |> throwError

let numBoolBinop = boolBinop unpackNum
let strBoolBinop = boolBinop unpackStr
let boolBoolBinop = boolBinop unpackBool

let numbericBinOp op args =
    match args with
    | [] -> NumArgs(2, []) |> throwError
    | [ _ ] as v -> NumArgs(2, v) |> throwError
    | v ->
        let argsParsed: ThrowsError<List<int64>> =
            args |> List.map unpackNum |> List.sequence

        Result.bind (fun (head :: tail) -> List.foldM op head tail) argsParsed
        |> Result.map LispNumber

let safeMath op a b = op a b |> Result.Ok

let car =
    function
    | [ LispList (x :: _) ] -> Result.Ok x
    | [ LispDottedList (x :: _, _) ] -> Result.Ok x
    | [ badArg ] -> TypeMismatch("pair", badArg) |> throwError
    | badArgList -> NumArgs(1, badArgList) |> throwError

let cdr v =
    match v with
    | [ LispList (_ :: xs) ] -> Result.Ok(LispList xs)
    | [ LispDottedList ([ _ ], x) ] -> Result.Ok x
    | [ LispDottedList (_ :: xs, x) ] -> Result.Ok(LispDottedList(xs, x))
    | [ badArg ] -> TypeMismatch("pair", badArg) |> throwError
    | badArgList -> NumArgs(1, badArgList) |> throwError

let cons =
    function
    | [ x1; LispList [] ] -> Result.Ok(LispList [ x1 ])
    | [ x; LispList xs ] -> x :: xs |> LispList |> Result.Ok
    | [ x; LispDottedList (xs, xlast) ] -> LispDottedList(x :: xs, xlast) |> Result.Ok
    | [ x1; x2 ] -> LispDottedList([ x1 ], x2) |> Result.Ok
    | badArgList -> NumArgs(2, badArgList) |> throwError

let rec eqv =
    function
    | [ l; r ] -> l = r |> LispBool |> Result.Ok
    | badArgList -> NumArgs(2, badArgList) |> throwError

let unpackEquals arg1 arg2 (unpacker: LispVal -> ThrowsError<obj>) =
    monad {
        let! unpacked1 = unpacker arg1
        let! unpacked2 = unpacker arg2
        return (unpacked1 = unpacked2)
    }
    </ catch /> (fun _ -> result false)

let anyUnpacker unpacker v =
    unpacker v |> Result.map (fun x -> x :> obj)

let rec equalFn =
    function
    | [ LispList a; LispList b ] ->
        let ret =
            a.Length = b.Length
            && List.zip a b
               |> List.forall (fun (x, y) -> equal2 x y)

        ret |> LispBool |> Result.Ok
    | [ LispDottedList (a1, b1); LispDottedList (a2, b2) ] ->
        let ret =
            a1.Length = a2.Length
            && List.zip a1 a2
               |> List.forall (fun (x, y) -> equal2 x y)
            && equal2 b1 b2

        ret |> LispBool |> Result.Ok
    | [ a; b ] ->
        let unpackers =
            [ anyUnpacker unpackNum
              anyUnpacker unpackStr
              anyUnpacker unpackBool ]

        let anyUnpackerEqual =
            unpackers
            |> List.exists
                (fun up ->
                    let ret = unpackEquals a b up
                    ret = Result.Ok(true))

        let ret =
            anyUnpackerEqual
            || (eqv [ a; b ] |> (=) (Result.Ok(LispBool true)))

        ret |> LispBool |> Result.Ok
    | badArgList -> NumArgs(2, badArgList) |> throwError

and equal2 x y =
    equalFn [ x; y ] |> (=) (Result.Ok(LispBool true))

let makeFunc varargs env (args: List<LispVal>) body =
    LispFunc((List.map (fun p -> p.ToString()) args), varargs, body, env)
    |> Result.Ok

let makeNormalFunc (envType: Env) args body = makeFunc None envType args body

let makeVarArgs =
    makeFunc << Some << (fun v -> v.ToString())

let primitives: Map<string, List<LispVal> -> ThrowsError<LispVal>> =
    Map
        .empty
        .Add("+", numbericBinOp (safeMath (+)))
        .Add("-", numbericBinOp (safeMath (-)))
        .Add("*", numbericBinOp (safeMath (*)))
        .Add("/", numbericBinOp (safeMath (/)))
        .Add("mod", numbericBinOp (safeMath (%)))
        .Add("quotient", numbericBinOp (safeMath (/)))
        .Add("remainder", numbericBinOp (fun a b -> (divRem a b) |> snd |> Result.Ok))
        .Add("=", numBoolBinop (=))
        .Add("<", numBoolBinop (<))
        .Add(">", numBoolBinop (>))
        .Add("/=", numBoolBinop (<>))
        .Add(">=", numBoolBinop (>=))
        .Add("<=", numBoolBinop (<=))
        .Add("&&", boolBoolBinop (&&))
        .Add("||", boolBoolBinop (||))
        .Add("string=?", strBoolBinop (=))
        .Add("string<?", strBoolBinop (<))
        .Add("string>?", strBoolBinop (>))
        .Add("string<=?", strBoolBinop (<=))
        .Add("string>=?", strBoolBinop (>=))
        .Add("car", car)
        .Add("cdr", cdr)
        .Add("cons", cons)
        .Add("eqv?", eqv)
        .Add("eq?", eqv)
        .Add("equal?", equalFn)

let isBound (env: Env) var = env.ContainsKey(var)

let getVar (env: Env) var =
    let mutable ret = ref (LispBool false)

    if env.TryGetValue(var, &ret) then
        Result.Ok !ret
    else
        UnboundVar("Getting an unbound variable", var)
        |> throwError

let setVar (env: Env) var value =
    if isBound env var then
        env.[var] := value
        Result.Ok value
    else
        UnboundVar("Setting an unbound variable", var)
        |> throwError

let defineVar (env: Env) var value =
    env.[var] <- ref value
    Result.Ok value

let bindVars (env: Env) (vars: Map<string, LispVal>) =
    for kv in vars do
        env.[kv.Key] <- ref kv.Value

    env

let runIOSafe<'T> (action: unit -> 'T) =
  try
    action() |> Result.Ok
  with
    | e -> e.ToString() |> DefaultError |> throwError
type FileHandle with
    member this.Close () = match this with
                           | ReadHandle reader -> runIOSafe (fun () -> reader.Dispose())
                           | WriteHandle writer -> runIOSafe (fun () -> writer.Dispose())

type FileMode =
    | ModeRead
    | ModeWrite

let makePort mode args =
    match args with
    | [LispString filename] ->
        match mode with
        | ModeRead -> runIOSafe (fun () -> new System.IO.StreamReader(filename) |> ReadHandle |> LispPort)
        | ModeWrite -> runIOSafe (fun () -> new System.IO.StreamWriter(filename) |> WriteHandle |> LispPort)
    | v -> TypeMismatch ("makePort: filename", LispList v) |> throwError

let makePortFromStdIn () =
     runIOSafe (fun () -> new System.IO.StreamReader(System.Console.OpenStandardInput()) |> ReadHandle |> LispPort)

let closePort  =
  function
  | [LispPort p] -> p.Close() |> Result.map (fun _ -> LispBool true)
  | _ -> LispBool false |> Result.Ok

let safeIO<'T> (fn: LispVal -> ThrowsError<'T>) =
    function
    | LispPort handle as v ->
        match fn v with
        | Result.Ok(v) ->
                handle.Close() |> ignore
                Result.Ok(v)
        | Result.Error(e) ->
                handle.Close() |> ignore
                Result.Error(e)
    | _ -> DefaultError "safeIO: unexpected param" |> Result.Error

let rec readProc =
  function
  | [] -> makePortFromStdIn() |> Result.bind (safeIO (fun p -> readProc [p]))
  | [LispPort (ReadHandle reader)] ->
            runIOSafe (fun () -> reader.ReadLine()) |> Result.map LispString
  | args  -> BadSpecialForm("readProc", LispList args) |> throwError

let rec writeProc =
  function
  | [obj] -> makePortFromStdIn() |> Result.bind (safeIO (fun p -> writeProc [obj; p]))
  | [obj; LispPort (WriteHandle writer)] ->
            runIOSafe (fun () -> writer.Write(sprintf "%s" (obj.ToString()))) |> Result.map (fun _ -> LispBool true)
  | args  -> BadSpecialForm("readProc", LispList args) |> throwError

let readFile filename = runIOSafe (fun() -> System.IO.File.ReadAllText(filename))

let readContents =
    function
    | [LispString filename] -> readFile filename |> Result.map LispString
    | args  -> BadSpecialForm("readContents", LispList args) |> throwError

let load filename = readFile filename |> Result.bind readExprList

let readAll =
    function
    | [LispString filename] -> load filename |> Result.map LispList
    | args  -> BadSpecialForm("readAll", LispList args) |> throwError

let rec eval (env: Env) =
    function
    | LispString _ as v -> Result.Ok v
    | LispNumber _ as v -> Result.Ok v
    | LispBool _ as v -> Result.Ok v
    | LispAtom id -> getVar env id
    | LispList [ LispAtom "quote"; v ] -> Result.Ok v
    | LispList [ LispAtom "if"; pred; conseq; alt ] ->
        eval env pred
        |> Result.bind
            (fun v ->
                match v with
                | LispBool false -> eval env alt
                | _ -> eval env conseq)
    | LispList [ LispAtom "set!"; LispAtom var; form ] -> eval env form |> Result.bind (setVar env var)
    | LispList [ LispAtom "define"; LispAtom var; form ] -> eval env form |> Result.bind (defineVar env var)
    | LispList (LispAtom "define" :: LispList (LispAtom var :: args) :: body) ->
                makeNormalFunc env args body |> Result.bind (defineVar env var)
    | LispList (LispAtom "define" :: LispDottedList (LispAtom var :: args, LispAtom vargs) :: body) ->
                makeVarArgs vargs env args body |> Result.bind (defineVar env var)
    | LispList (LispAtom "lambda" :: LispList p :: body) ->
                makeNormalFunc env p body
    | LispList (LispAtom "lambda" :: LispDottedList (LispAtom var :: args, LispAtom vargs) :: body) ->
                makeVarArgs vargs env args body |> Result.bind (defineVar env var)
    | LispList (LispAtom "lambda" :: (LispAtom vargs) :: body) ->
                makeVarArgs vargs env [] body
    | LispList [LispAtom "load"; LispString filename] ->
             load filename |> Result.bind (mapM (eval env)) |> Result.map List.last
    | LispList (func :: args) ->
            monad {
                let! fn = eval env func
                let! argVals = mapM (eval env) args
                let! ret = apply fn argVals
                return ret
            }
    | badform ->
        BadSpecialForm("Unrecognized special form", badform)
        |> throwError

and mapM fn =
    function
    | [] -> Result.Ok []
    | x :: xs ->
        match fn x with
        | Result.Error e -> Result.Error e
        | Result.Ok v -> mapM fn xs |> Result.map (fun vs -> v :: vs)

and apply func args =
    match func with
    | LispPrimitiveFunc fn -> fn args
    | LispIOFunc fn -> fn args
    | LispFunc (fparams, vargs, body, clojure) ->
        if fparams.Length <> args.Length && vargs = None then
            NumArgs(fparams.Length, args) |> throwError
        else
            let remainingArgs = List.drop fparams.Length args

            let evalBody (env: Env) = evalAllReturnLast env body

            let bindVarArgs arg env =
                match arg with
                | Some argName -> bindVars env (Map.empty.Add(argName, LispList remainingArgs))
                | None -> env

            let newEnv =
                List.zip fparams (List.take fparams.Length args)
                |> Map.ofList
                |> bindVars (new Env(clojure))
                |> bindVarArgs vargs

            evalBody newEnv
    | _ -> DefaultError "runtime error" |> throwError

and evalAllReturnLast env =
    function
    | [ x ] -> eval env x
    | x :: xs ->
        match eval env x with
        | Result.Error _ as v -> v
        | Result.Ok (_) -> evalAllReturnLast env xs

let applyProc =
  function
  | [func; LispList args] -> apply func args
  | func :: args -> apply func args
  | [] -> BadSpecialForm ("function", LispList []) |> throwError

let ioPrimitives =
    Map
        .empty
        .Add("apply", applyProc)
        .Add("open-input-file", makePort ModeRead)
        .Add("open-output-file", makePort ModeWrite)
        .Add("close-input-port", closePort)
        .Add("close-output-port", closePort)
        .Add("read", readProc)
        .Add("write", writeProc)
        .Add("read-contents", readContents)
        .Add("read-all", readAll)
    
let primitiveBindings () =
    Map.mapValues LispIOFunc ioPrimitives
    |> Map.union (Map.mapValues LispPrimitiveFunc primitives)
    |> bindVars (nullEnv ())