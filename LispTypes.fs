module LispTypes

let unwordsList list =
    list
    |> List.map (fun p -> p.ToString())
    |> String.concat " "

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
