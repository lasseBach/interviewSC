module FunctionLib

open System
open System.IO


type Result<'a> = Success of 'a | Failure of string

let isFile path =
    match File.Exists(path) with
    | true -> Success (path)
    | _ -> Failure "File does not exist"

let readFile = function
    | Success (path) -> 
        try Success (Array.toList(File.ReadAllLines(path)))
        with | :? (System.IO.IOException) as ex-> Failure ex.Message
    | Failure msg -> Failure msg


let splitOnSpace (ls : string list) =  
    let space = [|' ';'\t';'\r';'\n'|]
    ls |> List.collect (fun l -> Array.toList(l.Split(space, StringSplitOptions.RemoveEmptyEntries)))
    

let rmPunctuations (ws : string list) = 
    let punctuations = [|'!';'@';'#';'&';'(';')';'–';'[';'{';'}';']';':';';';'‘';',';'?';'/';'*'|]
    List.map (fun (w : string) -> (w.TrimStart(punctuations)).TrimEnd(punctuations)) ws

let prettyPrint (map : (string*int) list) =
    List.iter (function (k,v) -> printfn "%i: %s" v k) map

let eval ls =
    let ws = ls |> splitOnSpace |> rmPunctuations
    let wcs = Seq.groupBy id ws |> Seq.map(fun (k,v) -> (k,Seq.length v)) |> Seq.toList
    wcs

let countWords (path : string) =
    match (isFile path |> readFile) with
    | Success (ls) ->
        let res = eval ls
        prettyPrint res 
    | Failure msg -> printfn "%s" msg