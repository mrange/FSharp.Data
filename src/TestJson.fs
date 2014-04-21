// --------------------------------------------------------------------------------------
// Copyright (c) Microsoft Corporation 2005-2012.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 
//
// JSON test cases
// --------------------------------------------------------------------------------------

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Text
open FSharp.Data
open FSharp.Data.Runtime.HttpUtils

let failures = ref 0

let failure (m : string) = 
    failures := !failures + 1
    printfn "FAILURE: %s" m

let FdArray     = JsonValue.Array
let FdNull      = JsonValue.Null
let FdBoolean   = JsonValue.Boolean
let FdString    = JsonValue.String
let FdNumber    = JsonValue.Float
let FdObject    = JsonValue.Record

type Whitespace = string*string

type WsJson =
    | WsString  of Whitespace*string
    | WsFloat   of Whitespace*float
    | WsBoolean of Whitespace*bool
    | WsNull    of Whitespace
    | WsRecord  of Whitespace*(Whitespace*string*WsJson)[]
    | WsArray   of Whitespace*WsJson[]


let toJson (json : WsJson) : string = 
    let sb = StringBuilder()

    let write ((l,r) : Whitespace) (s : string) = 
        ignore <| sb.Append l
        ignore <| sb.Append s
        ignore <| sb.Append r

    let rec toJson (json : WsJson) = 
        match json with
        | WsString  (ws,s)   -> write ws ("\"" + (JavaScriptStringEncode s) + "\"")
        | WsFloat   (ws,f)   -> write ws (f.ToString(CultureInfo.InvariantCulture))
        | WsBoolean (ws,b)   -> write ws (if b then "true" else "false")
        | WsNull    ws       -> write ws "null"
        | WsRecord  ((l,r),ms)  -> 
            ignore <| sb.Append l
            ignore <| sb.Append '{'
            for i in 0..ms.Length-1 do
                if i > 0 then
                    ignore <| sb.Append ','
                let (ws,n,v) = ms.[i]
                write ws ("\"" + (JavaScriptStringEncode n) + "\"")
                ignore <| sb.Append ':'
                toJson v
            ignore <| sb.Append '}'
            ignore <| sb.Append r
        | WsArray   ((l,r),vs)  ->  
            ignore <| sb.Append l
            ignore <| sb.Append '['
            for i in 0..vs.Length-1 do
                if i > 0 then
                    ignore <| sb.Append ','
                let v = vs.[i]
                toJson v
            ignore <| sb.Append ']'
            ignore <| sb.Append r

    toJson json
    sb.ToString()

let rec toJsonValue (json : WsJson) : JsonValue = 
    match json with 
    | WsString  (_,s)   -> FdString   s
    | WsFloat   (_,f)   -> FdNumber   f
    | WsBoolean (_,b)   -> FdBoolean  b
    | WsNull    _       -> FdNull
    | WsRecord  (_,ms)  -> 
        let nms = ms |> Array.map (fun (_,n,m) -> n,toJsonValue m)
        FdObject nms
    | WsArray   (_,vs)  ->  
        let nms = vs |> Array.map toJsonValue
        FdArray nms


let isNear (l : float) (r : float) = 
    if r < 16. * Double.Epsilon then
        l < 16. * Double.Epsilon
    else
        let d = 1E-10
        let ratio = l / r
        ratio < (1. + d) && ratio > (1. - d)

let rec isEqual (l : JsonValue) (r : JsonValue) =
    match l,r with
    | JsonValue.Null        , JsonValue.Null                                    -> true
    | JsonValue.Boolean lb  , JsonValue.Boolean rb when lb = rb                 -> true
    | JsonValue.String  ls  , JsonValue.String  rs when ls = rs                 -> true
    | JsonValue.Float   lf  , JsonValue.Float   rf when isNear lf rf            -> true // The reasons we do a custom isEqual
    | JsonValue.Float   lf  , JsonValue.Number  rd when isNear lf (float rd)    -> true // The reasons we do a custom isEqual
    | JsonValue.Number  ld  , JsonValue.Float   rf when isNear (float ld) rf    -> true // The reasons we do a custom isEqual
    | JsonValue.Number  ld  , JsonValue.Number  rd when ld = rd                 -> true // The reasons we do a custom isEqual
    | JsonValue.Array   lvs , JsonValue.Array   rvs -> 
        if lvs.Length <> rvs.Length then false
        else 
            let mutable res = true
            let mutable i   = 0
            let length      = lvs.Length
            while res && i < length do
                let li = lvs.[i]
                let ri = rvs.[i]
                res <- isEqual li ri
                i <- i + 1
            res
    | JsonValue.Record  lms , JsonValue.Record  rms ->
        if lms.Length <> rms.Length then false
        else 
            let mutable res = true
            let mutable i   = 0
            let length      = lms.Length
            while res && i < length do
                let (ln,li) = lms.[i]
                let (rn,ri) = rms.[i]
                res <- (ln = rn) && isEqual li ri
                i <- i + 1
            res
    | _ -> false
  

let maxLevel            = 5
let maxArrayValues      = 6
let maxRecordMembers    = 6
let maxWhitespaceLength = 6
let maxStringLength     = 10

let generateWhitespace      (r : Random)            : Whitespace =
    let gws (r : Random) : string = 
        let l = r.Next(maxWhitespaceLength)
        System.String 
            [| for i in 1..l -> match r.Next(6) with 
                                | 1 -> '\n'
                                | 2 -> '\t'
                                | _ -> ' '
            |]

    (gws r),(gws r)

let generateRandomString    (l : int) (r : Random)  : string    = 
    let cs =    [| for i in 1..l -> match r.Next(35) with 
                                    | 0 -> "\\\""
                                    | 1 -> "\\/"
                                    | 2 -> "\\b"
                                    | 3 -> "\\f"
                                    | 4 -> "\\n"
                                    | 5 -> "\\r"
                                    | 6 -> "\\t"
                                    | 7 -> "\\t"
                                    | 8 -> "/"
                                    | 9 -> " "
                                    | n -> (char (int 'a' + n - 10)).ToString()
                |]

    let sb = StringBuilder()
    for c in cs do
        ignore <| sb.Append c

    sb.ToString ()    

let generateString          (n : int) (r : Random)  : WsJson   = 
    let l = r.Next(maxStringLength)
    let s = generateRandomString l r
    WsString (generateWhitespace r, s)

let generateFloat           (n : int) (r : Random)  : WsJson   = WsFloat (generateWhitespace r,  (pown 10. (r.Next(0,20))) * r.NextDouble())
let generateBoolean         (n : int) (r : Random)  : WsJson   = WsBoolean (generateWhitespace r, r.NextDouble() > 0.5)
let generateNull            (n : int) (r : Random)  : WsJson   = WsNull <| generateWhitespace r
let rec generateRecord      (n : int) (r : Random)  : WsJson   = 
     let l = r.Next(maxRecordMembers)
     let ms = [| for i in 1..l -> (generateWhitespace r, generateRandomString 4 r, generateJson (n + 1) r) |]
     WsRecord (generateWhitespace r, ms)
and generateArray           (n : int) (r : Random)  : WsJson   = 
     let l = r.Next(maxArrayValues)
     let vs = [| for i in 1..l -> (generateJson (n + 1) r) |]
     WsArray (generateWhitespace r, vs)
and generateJson            (n : int) (r : Random)  : WsJson   =
    let testCaseDistribution =
        [
            0.2 ,   generateString 
            0.4 ,   generateFloat
            0.6 ,   generateBoolean
            0.8 ,   generateNull
            0.9 ,   generateRecord
            1.0 ,   generateArray
        ]
    let rootTestCaseDistribution =
        [
            0.5 ,   generateRecord
            1.0 ,   generateArray
        ]
    let leafCaseDistribution =
        [
            0.2 ,   generateString 
            0.4 ,   generateFloat
            0.7 ,   generateBoolean
            1.0 ,   generateNull
        ]

    let tcd = 
        if n = 0            then    rootTestCaseDistribution 
        elif n > maxLevel   then    leafCaseDistribution
        else                        testCaseDistribution

    let p = r.NextDouble()
    let f = tcd |> List.tryFind (fun (pp,tc) -> p <= pp)
    match f with 
    | None          -> WsNull <| generateWhitespace r  
    | Some (_, tc)  ->
        tc n r

let generateTestCase (r : Random) : string*(JsonValue option) = 
    let expected    = generateJson 0 r
    let json        = toJson expected
    let e           = Some <| toJsonValue expected
    json,e

let runTestCases (parser : string->JsonValue) = 
    printfn "Running manual and generated test cases"

    let manualTestCases = 
        [
            // Positive tests
            """[]"""                                , Some <| FdArray [||]
            """[null]"""                            , Some <| FdArray [|FdNull|]
            """[true]"""                            , Some <| FdArray [|FdBoolean true|]
            """[false]"""                           , Some <| FdArray [|FdBoolean false|]
            """[""]"""                              , Some <| FdArray [|FdString ""|]
            """["Test"]"""                          , Some <| FdArray [|FdString "Test"|]
            """["Test\t"]"""                        , Some <| FdArray [|FdString "Test\t"|]
            """["\""]"""                            , Some <| FdArray [|FdString "\""|]
            """["\"\\\//\b\f\n\r\t\u0041"]"""       , Some <| FdArray [|FdString "\"\\//\b\f\n\r\t\u0041"|]
            """[0]"""                               , Some <| FdArray [|FdNumber 0.|]
            """[0.5]"""                             , Some <| FdArray [|FdNumber 0.5|]
            """[1234]"""                            , Some <| FdArray [|FdNumber 1234.|]
            """[-1234]"""                           , Some <| FdArray [|FdNumber -1234.|]
            """[1234.25]"""                         , Some <| FdArray [|FdNumber 1234.25|]
            """[-1234.25]"""                        , Some <| FdArray [|FdNumber -1234.25|]
            """[1234.50E2]"""                       , Some <| FdArray [|FdNumber 123450.|]
            """[-1234.5E+2]"""                      , Some <| FdArray [|FdNumber -123450.|]
            """[123450E-2]"""                       , Some <| FdArray [|FdNumber 1234.50|]
            """[-123450e-2]"""                      , Some <| FdArray [|FdNumber -1234.50|]
            """[4.26353146520608E+18]"""            , Some <| FdArray [|FdNumber 4.26353146520608E+18|]
            """[1.2345]"""                          , Some <| FdArray [|FdNumber 1.2345|]
            """[null,false]"""                      , Some <| FdArray [|FdNull;FdBoolean false|]
            """[{}]"""                              , Some <| FdArray [|FdObject [||]|]
            """{}"""                                , Some <| FdObject [||]
            """{"a":null}"""                        , Some <| FdObject [|"a",FdNull|]
            """{"a":[]}"""                          , Some <| FdObject [|"a",FdArray [||]|]
            """{"a":[],"b":{}}"""                   , Some <| FdObject [|"a",FdArray [||];"b",FdObject [||]|]
            """{"\"":null}"""                       , Some <| FdObject [|"\"",FdNull|]
            """{"\"\\\//\b\f\n\r\t\u0041":null}"""  , Some <| FdObject [|"\"\\//\b\f\n\r\t\u0041",FdNull|]
            // Whitespace cases
            """  []"""                              , Some <| FdArray [||]
            """[]  """                              , Some <| FdArray [||]
            """  []  """                            , Some <| FdArray [||]
            """[  true]"""                          , Some <| FdArray [|FdBoolean true|]
            """[true  ]"""                          , Some <| FdArray [|FdBoolean true|]
            """[  true  ]"""                        , Some <| FdArray [|FdBoolean true|]
            """[null,  true]"""                     , Some <| FdArray [|FdNull;FdBoolean true|]
            """[null  ,true]"""                     , Some <| FdArray [|FdNull;FdBoolean true|]
            """[null  ,  true]"""                   , Some <| FdArray [|FdNull;FdBoolean true|]
            """  {}"""                              , Some <| FdObject [||]
            """{}  """                              , Some <| FdObject [||]
            """  {}  """                            , Some <| FdObject [||]
            """{  "a":true}"""                      , Some <| FdObject [|"a",FdBoolean true|]
            """{"a":true  }"""                      , Some <| FdObject [|"a",FdBoolean true|]
            """{  "a":true  }"""                    , Some <| FdObject [|"a",FdBoolean true|]
            """{"a"  :true}"""                      , Some <| FdObject [|"a",FdBoolean true|]
            """{"a":  true}"""                      , Some <| FdObject [|"a",FdBoolean true|]
            """{"a"  :  true}"""                    , Some <| FdObject [|"a",FdBoolean true|]
            """{"a":[]  ,"b":{}}"""                 , Some <| FdObject [|"a",FdArray [||];"b",FdObject [||]|]
            """{"a":[],  "b":{}}"""                 , Some <| FdObject [|"a",FdArray [||];"b",FdObject [||]|]
            """{"a":[]  ,  "b":{}}"""               , Some <| FdObject [|"a",FdArray [||];"b",FdObject [||]|]
            // Negative tests
            """0"""                                 , None
            """true"""                              , None
            """[NaN]"""                             , None
            """[,]"""                               , None
            """[true,]"""                           , None
            """{,}"""                               , None
            """{{}}"""                              , None
            """{a:[]}"""                            , None
            """{"a":[],}"""                         , None
            """[0123]"""                            , None
            """[+123]"""                            , None
        ]

    let random = Random (19740531)

    let generatedTestCases = [ for i in 1..1000 -> generateTestCase random ]

    let testCases = 
        manualTestCases
//        generatedTestCases
//        manualTestCases@generatedTestCases

    for json,expected in testCases do
        try
            let result = parser json
            match expected with
            | Some exp when isEqual exp result  -> ()
            | Some exp                          -> failure <| sprintf "Parse succeeded but didn't produce expected value\nJSON:\n%s\nExpected:\n%A\nActual:\n%A" json exp result
            | None                              -> failure <| sprintf "Parse succeeded but expected to fail\nJSON:\n%s\nActual:\n%A" json result
        with
            | e ->
                match expected with
                | None      -> ()
                | Some exp  -> failure <| sprintf "Parse failed but expected to succeed\nJSON:\n%s\nExpected:\n%A\nException:\n%A" json exp e

let runSampleTestCases (newParser : string->seq<JsonValue>) (oldParser : string->seq<JsonValue>) =
    printfn "Running sample test cases"

    let samplePath      = Path.Combine (AppDomain.CurrentDomain.BaseDirectory, "TestData")
    let documentPaths   = Directory.GetFiles(samplePath, "*.json")

    for documentPath in documentPaths do
        try
            let document = File.ReadAllText documentPath
            let newJson = newParser document
            let oldJson = oldParser document

            let compared = 
                oldJson 
                |> Seq.mapi (fun i l -> i,l) 
                |> Seq.compareWith (
                    fun (li,l) (ri,r) -> 
                        if isEqual l r then 0 else 1000 * (ri + 1) + (li + 1)
                    ) (newJson |> Seq.mapi (fun i l -> i,l))

            if compared <> 0 then
                failure <| sprintf "Parsed successful for document but new and old parser differs, result: %d: %s" compared documentPath
            else
                ()
        with
            e -> 
                failure <| sprintf "Parsed failed for document: %s, message: %s" documentPath e.Message

let runPerformanceTestCases (newParser : string->seq<JsonValue>) (oldParser : string->seq<JsonValue>) =
    printfn "Running performance test cases"

    let testCases =
        [
            100000  ,"contacts.json"
            100     ,"topics.json"
            500     ,"GitHub.json"
        ]

    let samplePath      = Path.Combine (AppDomain.CurrentDomain.BaseDirectory, "TestData")

    for (iterations, testCase) in testCases do
        let documentPath    = Path.Combine (samplePath, testCase)
        try
            let document = File.ReadAllText documentPath

            // Dry run
            ignore <| newParser document
            ignore <| oldParser document

            printfn "Running %d iterations on document: %s using old parser" iterations documentPath
            let oldStopWatch   = Stopwatch()
            oldStopWatch.Start()
            for i in 1..iterations do
                ignore (oldParser document |> Seq.toList)
            oldStopWatch.Stop()

            printfn "Running %d iterations on document: %s using new parser" iterations documentPath
            let newStopWatch   = Stopwatch()
            newStopWatch.Start()
            for i in 1..iterations do
                ignore (newParser document |> Seq.toList)
            newStopWatch.Stop()

            printfn "Result: Old parser: %d ms, new parser: %d ms" oldStopWatch.ElapsedMilliseconds newStopWatch.ElapsedMilliseconds

        with
            e -> 
                failure <| sprintf "Parsed failed for document: %s, message: %s" documentPath e.Message

let testErrorMessage (parser : string->JsonValue) = 
    let testCases =
        [
            """[NaN]"""                             
            """0"""                                 
            """true"""                              
            """[,]"""                               
            """[true,]"""                           
            """{,}"""                               
            """{{}}"""                               
            """{a:[]}"""                            
            """{"a":[],}"""                         
            """[0123]"""                            
            """[+0123]"""                            
        ]

    let getErrorMessage json = 
        try
            ignore <| parser json
            "FAILED: No parser error"
        with 
            e -> e.Message
    
    for testCase in testCases do
        let msg = getErrorMessage testCase
        printfn "%s" msg


[<EntryPoint>]
let main argv = 
    let newParse         json = let p = FSharp.Data.Parser.JsonParserNew (json, None, false) in p.Parse ()
    let oldParse         json = let p = FSharp.Data.Parser.JsonParserOld (json, None, false) in p.Parse ()
    let newParseMultiple json = let p = FSharp.Data.Parser.JsonParserNew (json, None, false) in p.ParseMultiple ()
    let oldParseMultiple json = let p = FSharp.Data.Parser.JsonParserOld (json, None, false) in p.ParseMultiple ()

    printfn "Testing new Parser"
    runTestCases            newParse
    runSampleTestCases      newParseMultiple oldParseMultiple
    testErrorMessage        newParse
    runPerformanceTestCases newParseMultiple oldParseMultiple

    printfn "Testing old Parser"
    //runTestCases            oldParse
    0
