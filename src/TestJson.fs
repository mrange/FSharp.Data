// --------------------------------------------------------------------------------------
// Copyright (c) Microsoft Corporation 2005-2012.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 
//
// JSON test cases
// --------------------------------------------------------------------------------------

open System
open System.Text
open FSharp.Data

let Array   = JsonValue.Array
let Null    = JsonValue.Null
let Boolean = JsonValue.Boolean
let String  = JsonValue.String
let Number  = JsonValue.Float
let Object  = JsonValue.Record


type Whitespace = string*string

type JsonAST =
  | WsString  of Whitespace*string
  | WsFloat   of Whitespace*float
  | WsBoolean of Whitespace*bool
  | WsNull    of Whitespace
  | WsRecord  of Whitespace*(Whitespace*JsonValue*string)[]
  | WsArray   of Whitespace*(Whitespace*JsonValue)[]


let maxLevel        = 5
let generateWhitespace      (r : Random)            : Whitespace =
    let gws (r : Random) : string = 
        let l = r.Next(6)
        string <|   [| for i in 0..l -> match r.Next(6) with 
                                        | 1 -> '\n'
                                        | 2 -> '\t'
                                        | _ -> ' '
                    |]

    (gws r),(gws r)
        
let generateString          (n : int) (r : Random)  : JsonAST   = 
    let l = r.Next(10)

    let cs =    [| for i in 0..l -> match r.Next(35) with 
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

    WsString (generateWhitespace r, sb.ToString())
let generateFloat           (n : int) (r : Random)  : JsonAST   = WsFloat (generateWhitespace r,  1.E20 * r.NextDouble())
let generateBoolean         (n : int) (r : Random)  : JsonAST   = WsBoolean (generateWhitespace r, r.NextDouble() > 0.5)
let generateNull            (n : int) (r : Random)  : JsonAST   = WsNull <| generateWhitespace r
let generateRecord          (n : int) (r : Random)  : JsonAST   = WsNull <| generateWhitespace r
let generateArray           (n : int) (r : Random)  : JsonAST   = 
     
     WsNull <| generateWhitespace r
let generateJson            (n : int) (r : Random)  : JsonAST   =
    let testCaseDistribution =
        [
            0.2 ,   generateString 
            0.4 ,   generateFloat
            0.6 ,   generateBoolean
            0.8 ,   generateNull
            0.9 ,   generateRecord
            1.0 ,   generateArray
        ]
    if n > maxLevel then WsNull <| generateWhitespace r  
    else
        let p = r.NextDouble()
        match testCaseDistribution |> List.tryFind (fun (pp,tc) -> p <= pp) with 
        | None          -> WsNull <| generateWhitespace r  
        | Some (_, tc)  ->
            tc (n + 1) r

let generateTestCase (r : Random) : string*(JsonValue option) = 
    "", None

let runTestCases (parser : string->JsonValue) = 
    let manualTestCases = 
        [
            """[] """               , Some <| Array [||]
            // Simple cases
            """[]"""                , Some <| Array [||]
            """[null]"""            , Some <| Array [|Null|]
            """[true]"""            , Some <| Array [|Boolean true|]
            """[false]"""           , Some <| Array [|Boolean false|]
            """[""]"""              , Some <| Array [|String ""|]
            """["Test"]"""          , Some <| Array [|String "Test"|]
            """["Test\t"]"""        , Some <| Array [|String "Test\t"|]
            """["\"\\\//\b\f\n\r\t\u0041"]"""    
                                    , Some <| Array [|String "\"\\//\b\f\n\r\t\u0041"|]
            """[0]"""               , Some <| Array [|Number 0.|]
            """[0.5]"""             , Some <| Array [|Number 0.5|]
            """[1234]"""            , Some <| Array [|Number 1234.|]
            """[-1234]"""           , Some <| Array [|Number -1234.|]
            """[1234.25]"""         , Some <| Array [|Number 1234.25|]
            """[-1234.25]"""        , Some <| Array [|Number -1234.25|]
            """[1234.50E2]"""       , Some <| Array [|Number 123450.|]
            """[-1234.5E+2]"""      , Some <| Array [|Number -123450.|]
// TODO: Implement own comparer due to rounding issues
//            """[123450E-2]"""   , Some <| Array [Number 1234.50|]
//            """[-123450e-2]"""  , Some <| Array [Number -1234.50|]
            """[null,false]"""      , Some <| Array [|Null;Boolean false|]
            """[{}]"""              , Some <| Array [|Object [||]|]
            """{}"""                , Some <| Object [||]
            """{"a":null}"""        , Some <| Object [|"a",Null|]
            """{"a":[]}"""          , Some <| Object [|"a",Array [||]|]
            """{"a":[],"b":{}}"""   , Some <| Object [|"a",Array [||];"b",Object [||]|]
            // Whitespace cases
            """ []"""               , Some <| Array [||]
            """[] """               , Some <| Array [||]
            """ [] """              , Some <| Array [||]
            """[ true]"""           , Some <| Array [|Boolean true|]
            """[true ]"""           , Some <| Array [|Boolean true|]
            """[ true ]"""          , Some <| Array [|Boolean true|]
            """[null, true]"""      , Some <| Array [|Null;Boolean true|]
            """[null ,true]"""      , Some <| Array [|Null;Boolean true|]
            """[null , true]"""     , Some <| Array [|Null;Boolean true|]
            """ {}"""               , Some <| Object [||]
            """{} """               , Some <| Object [||]
            """ {} """              , Some <| Object [||]
            """{ "a":true}"""       , Some <| Object [|"a",Boolean true|]
            """{"a":true }"""       , Some <| Object [|"a",Boolean true|]
            """{ "a":true }"""      , Some <| Object [|"a",Boolean true|]
            """{"a" :true}"""       , Some <| Object [|"a",Boolean true|]
            """{"a": true}"""       , Some <| Object [|"a",Boolean true|]
            """{"a" : true}"""      , Some <| Object [|"a",Boolean true|]
            """{"a":[] ,"b":{}}"""  , Some <| Object [|"a",Array [||];"b",Object [||]|]
            """{"a":[], "b":{}}"""  , Some <| Object [|"a",Array [||];"b",Object [||]|]
            """{"a":[] , "b":{}}""" , Some <| Object [|"a",Array [||];"b",Object [||]|]
            // Failure cases
            """0"""             , None
            """true"""          , None
            """[NaN]"""         , None
            """[,]"""           , None
            """[true,]"""       , None
            """{,}"""           , None
            """{"a":[],}"""     , None
            """[0123]"""        , None
        ]

    let random = Random (19740531)

    let generatedTestCases = 
        [ for i in 0..100 -> generateTestCase random ]

    let testCases = manualTestCases

    let failures = ref 0

    let failure (m : string) = 
        failures := !failures + 1
        printfn "FAILURE: %s" m

    for json,expected in testCases do
        try
            let result = parser json
            match expected with
            | Some exp when exp = result    -> ()
            | Some exp                      -> failure <| sprintf "Parse succeeded but didn't produce expected value\nJSON:\n%s\nExpected:\n%A\nActual:\n%A" json exp result
            | None                          -> failure <| sprintf "Parse succeeded but expected to fail\nJSON:\n%s\nActual:\n%A" json result
        with
            | e ->
                match expected with
                | None      -> ()
                | Some exp  -> failure <| sprintf "Parse failed but expected to succeed\nJSON:\n%s\nExpected:\n%A\nException:\n%A" json exp e



[<EntryPoint>]
let main argv = 
    printfn "Testing new Parser"
    runTestCases <| fun json -> let p = FSharp.Data.Parser.JsonParserNew (json, None, false) in p.Parse ()

    printfn "Testing old Parser"
    runTestCases <| fun json -> let p = FSharp.Data.Parser.JsonParserOld (json, None, false) in p.Parse ()
    0
