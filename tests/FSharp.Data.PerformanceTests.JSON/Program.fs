
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

    let mutable failures = 0

    for testCase in testCases do
        let fileName = Path.GetFileName testCase
        try
            let jsonData        = File.ReadAllText testCase
            let referenceJson   = JsonReferenceParser.parseMultiple jsonData |> Seq.toList
            let fdataJson       = JsonValue.ParseMultiple jsonData |> Seq.toList
            let result          = isEquals referenceJson fdataJson
            if not result then
                failwith "JSON Reference parser and FSharp.Data doesn't agree"
        with
            | e ->  failures <- failures + 1
                    printfn "Sanity check failed for '%s' : %s" fileName e.Message

    failures

let PerformanceTest (debug : bool) =
    
    let testData  = GetTestCases ()
                    |> Array.map (fun tc -> (Path.GetFileName tc), tc)
                    |> Map.ofArray

    let testCases = 
        [
        //  n         min     max     json test data
            100     , 4.5   , 5.5   , "TwitterStream.json"
            100     , 5.0   , 6.0   , "topics.json"
            1000    , 3.5   , 4.5   , "WorldBank.json"
            1000    , 6.0   , 7.0   , "GitHub.json"
        ] |> List.map (fun (n,min,max,tc) -> n,min,max,tc,testData.[tc])

    printfn "Performance checking JSON Parser based on %d testcases" testCases.Length

    for n,min,max,fileName,filePath in testCases do
        let jsonData        = File.ReadAllText filePath
        let eval            = Seq.toList >> ignore
        let reference       = timeIt debug (sprintf "Reference: %s" fileName  ) n <| fun () -> JsonReferenceParser.parseMultiple jsonData |> eval
        let fsharpData      = timeIt debug (sprintf "FSharp.Data: %s" fileName) n <| fun () -> JsonValue.ParseMultiple jsonData |> eval

        let ratio           = float reference / float fsharpData
        let success         = min < ratio && ratio < max
        let msg             = sprintf 
                                "Performance test %s, ratio: %.1f < %.2f < %.1f, reference: %d, time: %d, fileName: %s" 
                                (if success then "success" else "failed")
                                min
                                ratio
                                max
                                reference 
                                fsharpData 
                                fileName
        if debug || (not success) then
            printfn "%s" msg
        else
            ()

[<EntryPoint>]
let main argv = 

    let failures = SanityCheck ()

    if failures = 0 then
        PerformanceTest true
    else
        printfn "Sanity check failed, skipping performance tests"

    0
