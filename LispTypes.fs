module LispTypes

let unwordsList list =
    list
    |> List.map (fun p -> p.ToString())
    |> String.concat " "

[<CustomEquality; NoComparison>]
type LispVal =
    | LispAtom of string
    | LispList of List<LispVal>
    | LispDottedList of List<LispVal> * LispVal
    | LispNumber of int64
    | LispString of string
    | LispBool of bool

    override this.ToString() =
        match this with
        | LispAtom s -> s
        | LispString s -> sprintf "\"%s\"" s
        | LispNumber v -> sprintf "%d" v
        | LispBool true -> "#t"
        | LispBool false -> "#f"
        | LispList v -> unwordsList v |> sprintf "(%s)"
        | LispDottedList (head, tail) ->
            head |> unwordsList |> (sprintf "(%s . %s)")
            <| (tail.ToString())

      override x.Equals(yObj) =
        let compareList (s1: List<LispVal>) (s2: List<LispVal>) =
           s1.Length = s2.Length
           && (List.zip s1 s2
               |> List.forall (fun (x, y) -> x = y))

        match yObj with
        | :? LispVal as y ->
            match (x, y) with
            | (LispAtom s1, LispAtom s2) -> s1 = s2
            | (LispString s1, LispString s2) -> s1 = s2
            | (LispNumber v1, LispNumber v2) -> v1 = v2
            | (LispBool b1, LispBool b2) -> b1 = b2
            | (LispList s1, LispList s2) -> compareList s1 s2
            | (LispDottedList (h1, t1), LispDottedList (h2, t2)) -> compareList h1 h2 && t1 = t2
            | _ -> false

type LispError =
    | NumArgs of int32 * List<LispVal>
    | TypeMismatch of string * LispVal
    | ParseError of string
    | BadSpecialForm of string * LispVal
    | NotFunction of string * string
    | UnboundVar of string * string
    | UnspecifiedReturn of string
    | DefaultError of string

    override this.ToString() =
        match this with
        | NumArgs (expected, found) ->
            sprintf
                "Expected %d args; found values: %s"
                expected
                (found
                 |> List.map (fun v -> v.ToString())
                 |> String.concat " ")
        | TypeMismatch (expected, found) -> sprintf "Invalid type expected %s, found %s" expected (found.ToString())
        | ParseError err -> sprintf "Parse error at %s" (err.ToString())
        | BadSpecialForm (message, form) -> sprintf "%s: %s" message (form.ToString())
        | NotFunction (message, func) -> sprintf "%s: %s" message func
        | UnboundVar (message, varname) -> sprintf "%s: %s" message varname
        | UnspecifiedReturn message -> message
        | DefaultError e -> sprintf "Error: %s" e

type ThrowsError<'T> = Result<'T, LispError>

let throwError e = Result.Error e