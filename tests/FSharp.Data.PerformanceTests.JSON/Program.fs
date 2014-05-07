
open FSharp.Data

open System
open System.IO

open TestSupport

let GetTestCases () =
    let baseDir     = AppDomain.CurrentDomain.BaseDirectory
    let testDataDir = Path.Combine (baseDir, "TestData")

    let files       = Directory.GetFiles (testDataDir, "*.json") 
//                        |> Array.filter (fun f -> f.Contains("optionals"))
    
    files

let SanityCheck () =

    let testCases = GetTestCases ()

    printfn "Sanity checking JSON Parser based on %d testcases" testCases.Length

    for testCase in testCases do
        try
            let jsonData        = File.ReadAllText testCase
            let referenceJson   = JsonReferenceParser.parseMultiple jsonData |> Seq.toList
            let fdataJson       = JsonValue.ParseMultiple jsonData |> Seq.toList
            let result          = isEquals referenceJson fdataJson
            if not result then
                failwith "JSON Reference parser and FSharp.Data doesn't agree"
        with
            | e -> printfn "Sanity check failed for '%s' : %s" (Path.GetFileName testCase) e.Message


[<EntryPoint>]
let main argv = 

    SanityCheck ()

    0
