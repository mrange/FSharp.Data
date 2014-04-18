// --------------------------------------------------------------------------------------
// Copyright (c) Microsoft Corporation 2005-2012.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 
//
// A simple F# portable parser for JSON data
// --------------------------------------------------------------------------------------

namespace FSharp.Data

open System
open System.IO
open System.Text
open System.Globalization
open FSharp.Data
open FSharp.Data.Runtime
open FSharp.Data.Runtime.HttpUtils
open FSharp.Data.Runtime.IO

/// Specifies the formatting behaviour of JSON values
[<RequireQualifiedAccess>]
type JsonSaveOptions = 
  /// Format (indent) the JsonValue
  | None = 0
  /// Print the JsonValue in one line in a compact way
  | DisableFormatting = 1

/// Represents a JSON value. Large numbers that do not fit in the 
/// Decimal type are represented using the Float case, while
/// smaller numbers are represented as decimals to avoid precision loss.
[<RequireQualifiedAccess>]
type JsonValue =
  | String of string
  | Number of decimal
  | Float of float 
  | Record of properties:(string * JsonValue)[]
  | Array of elements:JsonValue[]
  | Boolean of bool
  | Null

  override x.ToString() = x.ToString(JsonSaveOptions.None)

  member x.ToString saveOptions = 

    let rec serialize (sb:StringBuilder) indentation json =
      let newLine plus =
        if saveOptions = JsonSaveOptions.None then
          sb.AppendLine() |> ignore
          System.String(' ', indentation + plus) |> sb.Append |> ignore
      match json with
      | Null -> sb.Append "null"
      | Boolean b -> sb.Append(if b then "true" else "false")
      | Number number -> sb.Append(number.ToString(CultureInfo.InvariantCulture))
      | Float number -> sb.Append(number.ToString(CultureInfo.InvariantCulture))
      | String s -> 
          sb.Append("\"" + JavaScriptStringEncode(s) + "\"")
      | Record properties -> 
          let isNotFirst = ref false
          sb.Append "{"  |> ignore
          for k, v in properties do
            if !isNotFirst then sb.Append "," |> ignore else isNotFirst := true
            newLine 2
            if saveOptions = JsonSaveOptions.None then
              sb.AppendFormat("\"{0}\": ", k) |> ignore
            else
              sb.AppendFormat("\"{0}\":", k) |> ignore
            serialize sb (indentation + 2) v |> ignore
          newLine 0
          sb.Append "}"
      | Array elements -> 
          let isNotFirst = ref false
          sb.Append "[" |> ignore
          for element in elements do
            if !isNotFirst then sb.Append "," |> ignore else isNotFirst := true
            newLine 2
            serialize sb (indentation + 2) element |> ignore
          if elements.Length > 0 then 
            newLine 0
          sb.Append "]"

    (serialize (new StringBuilder()) 0 x).ToString()

/// [omit]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonValue =

  /// Active Pattern to view a `JsonValue.Record of (string * JsonValue)[]` as a `JsonValue.Object of Map<string, JsonValue>` for
  /// backwards compatibility reaons
  [<Obsolete("Please use JsonValue.Record instead")>]
  let (|Object|_|) x =
    match x with 
    | JsonValue.Record properties -> Map.ofArray properties |> Some
    | _ -> None

  /// Constructor to create a `JsonValue.Record of (string * JsonValue)[]` as a `JsonValue.Object of Map<string, JsonValue>` for
  /// backwards compatibility reaons
  [<Obsolete("Please use JsonValue.Record instead")>]
  let Object = Map.toArray >> JsonValue.Record 

// --------------------------------------------------------------------------------------
// JSON parser
// --------------------------------------------------------------------------------------

module Parser = 

    open System
    open System.Diagnostics
    open System.Collections.Generic
    
    let initialCapacity = 16

    [<Struct>]
    type Substring(i : string, b : int, e : int) =
        static let empty = Substring("",0,0)

        member x.Input      = i
        member x.Begin      = b
        member x.End        = e
        member x.Length     = e - b
        member x.IsEmpty    = b = e

        member x.Str        = i.Substring(b, e - b)
        member x.Char idx   = i.[b + idx]

        static member Empty = empty 

    
    [<Struct>]
    type ParserState<'UserState>(input : string, position : int, userState : 'UserState) = 
        
        member x.Input      = input
        member x.Position   = position
        member x.UserState  = userState
        member x.IsEOF      = position >= input.Length || position < 0

        member x.Match atLeast atMost (test : char->int->bool) : Substring*ParserState<'UserState> = 
            Debug.Assert (atLeast >= 0)
            Debug.Assert (atMost >= atLeast)

            let i           = input
            let ``begin``   = position
            let length      = i.Length

            let remaining   = length - ``begin``

            if atLeast > remaining then
                Substring.Empty,x
            else 
                let required    = ``begin`` + atLeast
                let ``end``     = ``begin`` + min remaining atMost
                
                let mutable cont = true
                let mutable iter = 0
                let mutable pos  = ``begin``

                while cont && pos < ``end`` do
                    cont    <- test i.[pos] iter
                    iter    <- iter + 1
                    pos     <- pos + 1

                let stopped = if cont then pos else pos - 1
                
                if required > stopped then
                    Substring.Empty,x
                else
                    Substring(i,``begin``, stopped),ParserState(i,stopped, userState)

                    
                    
    type ErrorMessage =
        | Expected      of string
    
    type ParserResult<'Result,'UserState> =
         | Success of 'Result * ParserState<'UserState>
         | Failure of ErrorMessage list * ParserState<'UserState> 

    type Parser<'Result, 'UserState> = ParserState<'UserState> -> ParserResult<'Result, 'UserState>

    let run (p : Parser<'T, unit>) (s : string) : ParserResult<'T, unit> = 
        let ps = ParserState(s, 0, ())
        p ps

    let prettify (_ : ErrorMessage list) (ps : ParserState<'UserState>) =
        // TODO:
        sprintf "Parse failed at %d" ps.Position

    let success (v : 'T) ps = Success (v, ps)
    let failure ems      ps = Failure (ems, ps)

    let preturn (v : 'T) : Parser<'T, 'UserState> = fun ps -> success v ps 

    let debug (p : Parser<'T, 'UserState>) : Parser<'T, 'UserState> = 
        fun ps -> 
            let r = p ps
            r

    let eof : Parser<unit, 'UserState> = 
        let ems = [Expected "EOF"]
        fun ps ->
            match ps.IsEOF with
            | false -> failure ems ps
            | true  -> success () ps

    let spaces : Parser<unit, 'UserState> = 
        let test ch _ = Char.IsWhiteSpace ch
        fun ps ->
            let ss,ps = ps.Match 0 Int32.MaxValue test
            success () ps

    let skipChar (c : char): Parser<unit, 'UserState> = 
        let test ch _ = ch = c
        let ems = [Expected <| c.ToString()]
        fun ps ->
            let ss,ps = ps.Match 1 1 test
            if not ss.IsEmpty then success () ps
            else failure ems ps

    let attempt (p : Parser<'T, 'UserState>) : Parser<'T, 'UserState> = 
        fun ps -> 
            p ps

    let orElse (l : Parser<'T, 'UserState>) (r : Parser<'T, 'UserState>) : Parser<'T, 'UserState> = 
        fun ps ->
            match l ps with
            | Failure (_,ps)    -> r ps
            | Success (lv, lps) -> Success (lv, lps)
    let ( <|> ) = orElse

    let map (p : Parser<'TFrom, 'UserState>) (m : 'TFrom->'TTo) : Parser<'TTo, 'UserState> = 
        fun ps ->
            match p ps with
            | Failure (ems,ps)  -> Failure (ems,ps)
            | Success (v, ps)   -> Success (m v, ps)
    let ( |>> ) = map

    let combine (l : Parser<'L, 'UserState>) (r : Parser<'R, 'UserState>) : Parser<'L*'R, 'UserState> = 
        fun ps ->
            match l ps with
            | Failure (lems,lps)  -> Failure (lems,lps)
            | Success (lv, lps)  -> 
                match r lps with
                | Failure (rems,rps)    -> Failure (rems,rps)
                | Success (rv, rps)     -> Success ((lv,rv), rps)
    let ( .>>. ) = combine

    let keepLeft (l : Parser<'L, 'UserState>) (r : Parser<'R, 'UserState>) : Parser<'L, 'UserState> = 
        fun ps ->
            match l ps with
            | Failure (lems,lps)  -> Failure (lems,lps)
            | Success (lv, lps)  -> 
                match r lps with
                | Failure (rems,rps)    -> Failure (rems,rps)
                | Success (_, rps)     -> Success (lv, rps)
    let ( .>> ) = keepLeft
        
    let keepRight (l : Parser<'L, 'UserState>) (r : Parser<'R, 'UserState>) : Parser<'R, 'UserState> = 
        fun ps ->
            match l ps with
            | Failure (lems,lps)  -> Failure (lems,lps)
            | Success (_, lps)  -> 
                match r lps with
                | Failure (rems,rps)    -> Failure (rems,rps)
                | Success (rv, rps)     -> Success (rv, rps)
    let ( >>. ) = keepRight

    let skipSatisfyImpl (test : char->int->bool) (ems : ErrorMessage list) : Parser<unit, 'UserState> =
        fun ps ->
            let ss,ps = ps.Match 1 1 test
            if not ss.IsEmpty then success () ps
            else failure ems ps
        
    let satisfyImpl (test : char->int->bool) (ems : ErrorMessage list) : Parser<char, 'UserState> =
        fun ps ->
            let ss,ps = ps.Match 1 1 test
            if not ss.IsEmpty then success (ss.Char 0) ps
            else failure ems ps

    let skipSatisfy (t : char->bool) : Parser<unit, 'UserState> = 
        let test ch _ = t ch
        let ems = [Expected "Satisfy"]
        skipSatisfyImpl test ems

    let satisfy (t : char->bool) : Parser<char, 'UserState> = 
        let test ch _ = t ch
        let ems = [Expected "Satisfy"]
        satisfyImpl test ems
        
    let skipAnyOf (s : string) : Parser<unit, 'UserState> = 
        let chars       = s.ToCharArray () 
        let set         = chars |> Set.ofArray
        let ems         = chars |> Array.map (fun c -> Expected <| c.ToString()) |> Seq.toList
        let test ch _   = set.Contains ch
        skipSatisfyImpl test ems

    let anyOf (s : string) : Parser<char, 'UserState> = 
        let chars       = s.ToCharArray () 
        let set         = chars |> Set.ofArray
        let ems         = chars |> Array.map (fun c -> Expected <| c.ToString()) |> Seq.toList
        let test ch _   = set.Contains ch
        satisfyImpl test ems

    let digit : Parser<char, 'UserState> =
        let ems = [Expected "HexDigit"]
        let test (ch : char) _ = 
            match ch with
            | _ when ch >= '0' && ch <= '9' -> true
            | _ -> false
        fun ps -> ps |> satisfyImpl test ems

    let hex : Parser<char, 'UserState> =
        let ems = [Expected "HexDigit"]
        let test (ch : char) _ = 
            match ch with
            | _ when ch >= '0' && ch <= '9' -> true
            | _ when ch >= 'a' && ch <= 'f' -> true
            | _ when ch >= 'A' && ch <= 'F' -> true
            | _ -> false
        fun ps -> ps |> satisfyImpl test ems

    let pipe2 
            (p0 : Parser<'T0, 'UserState>) 
            (p1 : Parser<'T1, 'UserState>) 
            (m  : 'T0->'T1->'T)
            : Parser<'T, 'UserState> = 
        fun ps ->
            match p0 ps with
            | Failure (ems0, ps0)   -> Failure (ems0, ps0)
            | Success (v0, ps0)     ->
                match p1 ps0 with
                | Failure (ems1, ps1)   -> Failure (ems1, ps1)
                | Success (v1, ps1)     -> success (m v0 v1) ps1
    let pipe3 
            (p0 : Parser<'T0, 'UserState>) 
            (p1 : Parser<'T1, 'UserState>) 
            (p2 : Parser<'T2, 'UserState>) 
            (m  : 'T0->'T1->'T2->'T)
            : Parser<'T, 'UserState> = 
        fun ps ->
            match p0 ps with
            | Failure (ems0, ps0)   -> Failure (ems0, ps0)
            | Success (v0, ps0)     ->
                match p1 ps0 with
                | Failure (ems1, ps1)   -> Failure (ems1, ps1)
                | Success (v1, ps1)     ->
                    match p2 ps1 with
                    | Failure (ems2, ps2)   -> Failure (ems2, ps2)
                    | Success (v2, ps2)     -> success (m v0 v1 v2) ps2
    let pipe4 
            (p0 : Parser<'T0, 'UserState>) 
            (p1 : Parser<'T1, 'UserState>) 
            (p2 : Parser<'T2, 'UserState>) 
            (p3 : Parser<'T3, 'UserState>)
            (m  : 'T0->'T1->'T2->'T3->'T)
            : Parser<'T, 'UserState> = 
        fun ps ->
            match p0 ps with
            | Failure (ems0, ps0)   -> Failure (ems0, ps0)
            | Success (v0, ps0)     ->
                match p1 ps0 with
                | Failure (ems1, ps1)   -> Failure (ems1, ps1)
                | Success (v1, ps1)     ->
                    match p2 ps1 with
                    | Failure (ems2, ps2)   -> Failure (ems2, ps2)
                    | Success (v2, ps2)     ->
                        match p3 ps2 with
                        | Failure (ems3, ps3)   -> Failure (ems3, ps3)
                        | Success (v3, ps3)     -> success (m v0 v1 v2 v3) ps3

    let choice (parsers : Parser<'T, 'UserState> list) : Parser<'T, 'UserState> = 
        fun ps ->
            let mutable finalEms    = []
            let mutable result      = None
            let mutable remaining   = parsers
            while result.IsNone && remaining.Length > 0 do
                let p = remaining.Head
                remaining <- remaining.Tail
                match p ps with
                | Failure (ems, _)  -> finalEms <- ems@finalEms
                | Success (v, ps)   -> result <- Some (v,ps)

            match result with 
            | None          -> failure finalEms ps
            | Some (v,ps)   -> success v ps
    
    let between 
            (b : Parser<_, 'UserState>) 
            (e : Parser<_, 'UserState>) 
            (p : Parser<'T, 'UserState>) 
            : Parser<'T, 'UserState> =
            pipe3 b p e <| fun _ v _ -> v

    let charReturn (c : char) (v : 'T) : Parser<'T, 'UserState> = 
        let test ch _   = c = ch
        let ems         = [Expected <| c.ToString()]
        fun ps ->
            let ss,ps = ps.Match 1 1 test
            if not ss.IsEmpty then success v ps
            else failure ems ps

    let stringReturn (s : string) (v : 'T) : Parser<'T, 'UserState> = 
        let test ch p   = s.[p] = ch
        let length      = s.Length
        let ems         = [Expected s]
        fun ps ->
            let ss,ps = ps.Match length length test
            if not ss.IsEmpty then success v ps
            else failure ems ps

    let many (p : Parser<'T, 'UserState>) : Parser<'T list, 'UserState> = 
        fun ps ->
            let result          = List<'T>(initialCapacity)
            let mutable cps     = ps
            while 
                match p cps with
                | Failure _         -> false
                | Success (v, vps)  ->
                    result.Add v
                    cps     <- vps
                    true
                do
                ()

            success (result |> Seq.toList) cps

    let manyChars (p : Parser<char, 'UserState>) : Parser<string, 'UserState> = 
        fun ps ->
            let result          = StringBuilder()
            let mutable cps     = ps
            while 
                match p cps with
                | Failure _         -> false
                | Success (v, vps)  ->
                    ignore <| result.Append(v)
                    cps     <- vps
                    true
                do
                ()

            success (result.ToString()) cps

    let sepBy (p : Parser<'T, 'UserState>) (sep : Parser<_, 'UserState>) : Parser<'T list, 'UserState> = 
        fun ps ->
            let mutable rems    = None
            let result          = List<'T>(initialCapacity)
            let mutable cps     = ps

            match p cps with
            | Failure _         -> ()
            | Success (f, fps)  ->
                result.Add f
                cps     <- fps
                while 
                    match sep cps with
                    | Failure _         -> false
                    | Success (_, sps)  ->
                        match p sps with
                        | Failure (ems,_)   -> 
                            rems <- Some ems
                            false
                        | Success (v, vps)  ->
                            result.Add v
                            cps     <- vps
                            true
                    do
                    ()
            match rems with
            | None      -> success (result |> Seq.toList) cps
            | Some ems  -> failure ems ps



    type JsonParserNew(jsonText:string, cultureInfo : CultureInfo option, tolerateErrors : bool) = 

        let satisfy1To9 c = c >= '1' && c <= '9'

        let hex2int c = 
            match c with
            | _ when c >= '0' && c <= '9'   -> (int c) - (int '0')     
            | _ when c >= 'a' && c <= 'f'   -> (int c) - (int 'a') + 10
            | _ when c >= 'A' && c <= 'F'   -> (int c) - (int 'A') + 10
            | _                             -> 0
        
        let makeDouble (d : int) (i : int64) (n :int) (f : float) (e : float) = 
            ((float d) * (pown 10. n) + (float i) + f)*e

        let p_ws            : Parser<unit, unit>        = spaces
        let p_token token   : Parser<unit, unit>        = skipChar token
        let p_wstoken token : Parser<unit, unit>        = attempt (p_ws >>. p_token token)

        let p_escape        : Parser<char, unit>      =  
                anyOf """"\/bfnrt"""
                |>> function
                    | 'b' -> '\b'
                    | 'f' -> '\f'
                    | 'n' -> '\n'
                    | 'r' -> '\r'
                    | 't' -> '\t'
                    | c   -> c
        let p_unicodeEscape : Parser<char, unit>      =
            p_token 'u' >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 -> 
                (hex2int h3)*0x1000 + (hex2int h2)*0x100 + (hex2int h1)*0x10 + hex2int h0 |> char
            )
        let p_char          : Parser<char, unit>        =
            choice
                [
                    satisfy (fun ch -> ch <> '"' && ch <> '\\')
                    p_token '\\' >>. (p_escape <|> p_unicodeEscape)
                ]
        let p_stringLiteral : Parser<string, unit>      =
            between (p_token '"') (p_token '"') (manyChars p_char)

        let p_digit1To9     : Parser<char, unit>        = satisfy satisfy1To9
        let p_digit         : Parser<int, unit>         = digit |>> hex2int
        let p_int           : Parser<int64*int, unit>   = many p_digit |>> (fun digits ->
                                let mutable result = 0L
                                for d in digits do
                                    result <- 10L * result + (int64 d)
                                result,digits.Length
                            )
        let p_e             : Parser<float, unit>       = 
            skipAnyOf "eE" >>. (choice [charReturn '-' 0.1;charReturn '+' 10.] <|> preturn 10.)
        let p_exponent      : Parser<float, unit>       = 
            p_e .>>. p_int |>> (fun (exp, (i,_)) -> pown exp (int i)) <|> preturn 1.
        let p_fraction      : Parser<float, unit>       = 
            (p_token '.' >>. (p_int |>> (fun (v,n) -> (float v) * (pown 0.1 n)))) <|> preturn 0.
        let p_sign          : Parser<float, unit>       = 
            (charReturn '-' -1.) <|> preturn 1.
        let p_digit19       : Parser<int, unit>         = 
            p_digit1To9 |>> hex2int
        let p_numberLiteral : Parser<float, unit>       = 
            p_sign .>>. choice 
                            [
                                // JSON doesn't allow numbers like 0123 (has to be 123).
                                // This is probably to avoid issues with octals numbers
                                pipe3 (p_token '0') p_fraction p_exponent (fun _ f e -> makeDouble 0 0L 0 f e)
                                pipe4 p_digit19 p_int p_fraction p_exponent (fun d (v,n) f e -> makeDouble d v n f e)
                            ] |>> (fun (s,n) -> s*n)


        let p_null          : Parser<JsonValue, unit>    = stringReturn "null"   JsonValue.Null
        let p_true          : Parser<JsonValue, unit>    = stringReturn "true"   <| JsonValue.Boolean true
        let p_false         : Parser<JsonValue, unit>    = stringReturn "false"  <| JsonValue.Boolean false
        let p_string        : Parser<JsonValue, unit>    = p_stringLiteral       |>> JsonValue.String
        let p_number        : Parser<JsonValue, unit>    = p_numberLiteral       |>> JsonValue.Float

        let rec p_value     : Parser<JsonValue, unit>    = fun cs -> 
            cs 
            |>  (p_ws 
                    >>. choice
                        [
                            p_null 
                            p_true
                            p_false
                            p_string
                            p_number  
                            p_object
                            p_array  
                        ])
        and p_member        : Parser<string*JsonValue, unit> = 
            p_ws >>. p_stringLiteral .>> p_ws .>> (p_token ':') .>>. p_value
        and p_object        : Parser<JsonValue, unit>        = 
            between (p_token '{') (p_wstoken '}') (sepBy p_member (p_wstoken ',') |>> (List.toArray >> JsonValue.Record))
        and p_array         : Parser<JsonValue, unit>        = 
            between (p_token '[') (p_wstoken ']') (sepBy p_value (p_wstoken ',') |>> (List.toArray >> JsonValue.Array))

        let p_root          : Parser<JsonValue, unit>        = p_ws >>. choice [p_object;p_array]
    
        let p_json  = p_root .>> p_ws .>> eof
        let p_jsons = (many p_root) .>> p_ws .>> eof

        member x.Parse () = 
            match run p_json jsonText with
            | Success (json, _)     -> json
            | Failure (ems, ps)     -> failwith <| prettify ems ps 

        member x.ParseMultiple () = 
            match run p_jsons jsonText with
            | Success (json, _)     -> json
            | Failure (ems, ps)     -> failwith <| prettify ems ps 

    type JsonParserOld(jsonText:string, cultureInfo, tolerateErrors) =

        let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture

        let mutable i = 0
        let s = jsonText

        // Helper functions
        let skipWhitespace() =
          while i < s.Length && Char.IsWhiteSpace s.[i] do
            i <- i + 1
        let decimalSeparator = cultureInfo.NumberFormat.NumberDecimalSeparator.[0]
        let isNumChar c = 
          Char.IsDigit c || c=decimalSeparator || c='e' || c='E' || c='+' || c='-'
        let throw() = 
          let msg = 
            sprintf 
              "Invalid Json starting at character %d, snippet = \n----\n%s\n-----\njson = \n------\n%s\n-------" 
              i (jsonText.[(max 0 (i-10))..(min (jsonText.Length-1) (i+10))]) (if jsonText.Length > 1000 then jsonText.Substring(0, 1000) else jsonText)
          failwith msg
        let ensure cond = 
          if not cond then throw()  

        // Recursive descent parser for JSON that uses global mutable index
        let rec parseValue() =
            skipWhitespace()
            ensure(i < s.Length)
            match s.[i] with
            | '"' -> JsonValue.String(parseString())
            | '-' -> parseNum()
            | c when Char.IsDigit(c) -> parseNum()
            | '{' -> parseObject()
            | '[' -> parseArray()
            | 't' -> parseLiteral("true", JsonValue.Boolean true)
            | 'f' -> parseLiteral("false", JsonValue.Boolean false)
            | 'n' -> parseLiteral("null", JsonValue.Null)
            | _ -> throw()

        and parseString() =
            ensure(i < s.Length && s.[i] = '"')
            i <- i + 1
            let buf = new StringBuilder()
            while i < s.Length && s.[i] <> '"' do
                if s.[i] = '\\' then
                    ensure(i+1 < s.Length)
                    match s.[i+1] with
                    | 'b' -> buf.Append('\b') |> ignore
                    | 'f' -> buf.Append('\f') |> ignore
                    | 'n' -> buf.Append('\n') |> ignore
                    | 't' -> buf.Append('\t') |> ignore
                    | 'r' -> buf.Append('\r') |> ignore
                    | '\\' -> buf.Append('\\') |> ignore
                    | '/' -> buf.Append('/') |> ignore
                    | '"' -> buf.Append('"') |> ignore
                    | 'u' ->
                        ensure(i+5 < s.Length)
                        let hexdigit d = 
                            if d >= '0' && d <= '9' then int32 d - int32 '0'
                            elif d >= 'a' && d <= 'f' then int32 d - int32 'a' + 10
                            elif d >= 'A' && d <= 'F' then int32 d - int32 'A' + 10
                            else failwith "hexdigit" 
                        let unicodeGraphShort (s:string) =
                            if s.Length <> 4 then failwith "unicodegraph";
                            uint16 (hexdigit s.[0] * 4096 + hexdigit s.[1] * 256 + hexdigit s.[2] * 16 + hexdigit s.[3])
                        let makeUnicodeChar (c:int) =  [| byte(c % 256); byte(c / 256) |]
                        let bytes = makeUnicodeChar(int(unicodeGraphShort(s.Substring(i+2, 4))))
                        let chars = UnicodeEncoding.Unicode.GetChars(bytes)
                        buf.Append(chars) |> ignore
                        i <- i + 4  // the \ and u will also be skipped past further below
                    | _ -> throw()
                    i <- i + 2  // skip past \ and next char
                else
                    buf.Append(s.[i]) |> ignore
                    i <- i + 1
            ensure(i < s.Length && s.[i] = '"')
            i <- i + 1
            buf.ToString()

        and parseNum() =
            let start = i
            while i < s.Length && isNumChar(s.[i]) do
                i <- i + 1
            let len = i - start
            match TextConversions.AsDecimal cultureInfo (s.Substring(start,len)) with  
            | Some x -> JsonValue.Number x
            | _ -> 
                match TextConversions.AsFloat [| |] (*useNoneForMissingValues*)false cultureInfo (s.Substring(start,len)) with  
                | Some x -> JsonValue.Float x
                | _ -> throw()

        and parsePair() =
            let key = parseString().Trim('"')
            skipWhitespace()
            ensure(i < s.Length && s.[i] = ':')
            i <- i + 1
            skipWhitespace()
            key, parseValue()

        and parseEllipsis() =
            let mutable openingBrace = false
            if i < s.Length && s.[i] = '{' then
                openingBrace <- true
                i <- i + 1
                skipWhitespace()
            while i < s.Length && s.[i] = '.' do
                i <- i + 1
                skipWhitespace()
            if openingBrace && i < s.Length && s.[i] = '}' then
                i <- i + 1
                skipWhitespace()

        and parseObject() =
            ensure(i < s.Length && s.[i] = '{')
            i <- i + 1
            skipWhitespace()
            let pairs = ResizeArray<_>()
            if i < s.Length && s.[i] = '"' then
                pairs.Add(parsePair())
                skipWhitespace()
                while i < s.Length && s.[i] = ',' do
                    i <- i + 1
                    skipWhitespace()
                    if tolerateErrors && s.[i] = '}' then
                        () // tolerate a trailing comma, even though is not valid json
                    else
                        pairs.Add(parsePair())
                        skipWhitespace()
            if tolerateErrors && i < s.Length && s.[i] <> '}' then
                parseEllipsis() // tolerate ... or {...}
            ensure(i < s.Length && s.[i] = '}')
            i <- i + 1
            JsonValue.Record(pairs |> Array.ofSeq)

        and parseArray() =
            ensure(i < s.Length && s.[i] = '[')
            i <- i + 1
            skipWhitespace()
            let vals = ResizeArray<_>()
            if i < s.Length && s.[i] <> ']' then
                vals.Add(parseValue())
                skipWhitespace()
                while i < s.Length && s.[i] = ',' do
                    i <- i + 1
                    skipWhitespace()
                    vals.Add(parseValue())
                    skipWhitespace()
            if tolerateErrors && i < s.Length && s.[i] <> ']' then
                parseEllipsis() // tolerate ... or {...}
            ensure(i < s.Length && s.[i] = ']')
            i <- i + 1
            JsonValue.Array(vals |> Seq.toArray)

        and parseLiteral(expected, r) =
            ensure(i+expected.Length < s.Length)
            for j in 0 .. expected.Length - 1 do
                ensure(s.[i+j] = expected.[j])
            i <- i + expected.Length
            r

        // Start by parsing the top-level value
        member x.Parse() = 
            let value = parseValue()
            skipWhitespace()
            if i <> s.Length then
                throw()
            value

        member x.ParseMultiple() = 
            seq {
                while i <> s.Length do
                    yield parseValue()
                    skipWhitespace() 
            }

type private JsonParser = Parser.JsonParserNew

type JsonValue with

  /// Parses the specified JSON string
  static member Parse(text, ?cultureInfo) = 
    JsonParser(text, cultureInfo, false).Parse()

  /// Loads JSON from the specified stream
  static member Load(stream:Stream, ?cultureInfo) = 
    use reader = new StreamReader(stream)
    let text = reader.ReadToEnd()
    JsonParser(text, cultureInfo, false).Parse()

  /// Loads JSON from the specified reader
  static member Load(reader:TextReader, ?cultureInfo) = 
    let text = reader.ReadToEnd()
    JsonParser(text, cultureInfo, false).Parse()

  /// Loads JSON from the specified uri asynchronously
  static member AsyncLoad(uri:string, ?cultureInfo) = async {
    let! reader = asyncReadTextAtRuntime false "" "" "JSON" uri
    let text = reader.ReadToEnd()
    return JsonParser(text, cultureInfo, false).Parse()
  }

  /// Loads JSON from the specified uri
  static member Load(uri:string, ?cultureInfo) =
    JsonValue.AsyncLoad(uri, ?cultureInfo=cultureInfo)
    |> Async.RunSynchronously

  /// Parses the specified JSON string, tolerating invalid errors like trailing commans, and ignore content with elipsis ... or {...}
  static member ParseSample(text, ?cultureInfo) =
    JsonParser(text, cultureInfo, true).Parse()

  /// Parses the specified string into multiple JSON values
  static member ParseMultiple(text, ?cultureInfo) =
    JsonParser(text, cultureInfo, false).ParseMultiple()

  /// Sends the JSON to the specified uri. Defaults to a POST request.
  member x.Request(uri:string, ?httpMethod, ?headers) =  
    let httpMethod = defaultArg httpMethod HttpMethod.Post
    let headers = defaultArg headers []
    let headers =
        if headers |> List.exists (fst >> ((=) (fst (HttpRequestHeaders.UserAgent ""))))
        then headers
        else HttpRequestHeaders.UserAgent "F# Data JSON Type Provider" :: headers
    let headers = HttpRequestHeaders.ContentType HttpContentTypes.Json :: headers
    Http.Request(
      uri,
      body = TextRequest (x.ToString(JsonSaveOptions.DisableFormatting)),
      headers = headers,
      httpMethod = httpMethod)

  /// Sends the JSON to the specified uri. Defaults to a POST request.
  member x.RequestAsync(uri:string, ?httpMethod, ?headers) =
    let httpMethod = defaultArg httpMethod HttpMethod.Post
    let headers = defaultArg headers []
    let headers =
        if headers |> List.exists (fst >> ((=) (fst (HttpRequestHeaders.UserAgent ""))))
        then headers
        else HttpRequestHeaders.UserAgent "F# Data JSON Type Provider" :: headers
    let headers = HttpRequestHeaders.ContentType HttpContentTypes.Json :: headers
    Http.AsyncRequest(
      uri,
      body = TextRequest (x.ToString(JsonSaveOptions.DisableFormatting)),
      headers = headers,
      httpMethod = httpMethod)

  [<Obsolete("Please use JsonValue.Request instead")>]
  member x.Post(uri:string, ?headers) =  
    x.Request(uri, ?headers = headers)
