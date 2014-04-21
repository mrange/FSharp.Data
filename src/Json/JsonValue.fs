﻿// --------------------------------------------------------------------------------------
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
open System.Linq.Expressions
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

    open OptimizedClosures
    open System
    open System.Collections.Generic
    open System.Diagnostics
    open System.Linq.Expressions
    open System.Text           

    type FSharpFuncWrap<'T1, 'T2, 'U>(func : Func<'T1, 'T2, 'U>) =
        inherit FSharpFunc<'T1, 'T2, 'U>()
        override x.Invoke(i1,i2) = func.Invoke(i1,i2)
        override x.Invoke(i1 : 'T1) : ('T2 -> 'U) = fun i2 -> func.Invoke(i1, i2)

    let fastAnyOf (anyOf : string) (matchResult : bool) : Func<char, int, bool> =
        let parameter0     = Expression.Parameter   (typeof<char>   , "ch"      )
        let parameter1     = Expression.Parameter   (typeof<int>    , "index"   )
        let resultVariable = Expression.Variable    (typeof<bool>   , "result"  )

        let switchCase =
            Expression.SwitchCase    (
                Expression.Assign     (resultVariable, Expression.Constant matchResult)           ,
                anyOf |> Seq.map (fun ch -> Expression.Constant (ch) :> Expression) |> Seq.toArray)

        let switchStatement =
            Expression.Switch (
               parameter0                                                                           ,
               Expression.Assign        (resultVariable, Expression.Constant (not matchResult))     ,
               switchCase                                                                           )

        let body =
            Expression.Block (
                [|resultVariable|]  ,
                switchStatement     ,
                resultVariable      )

        let lambda =
            Expression.Lambda<Func<char, int, bool>>(
               body         ,
               parameter0   ,
               parameter1   )

        lambda.Compile ();

    let fastMap (items : (string*'T) list) (defaultValue : 'T) : Func<char, int, 'T> =
        let tt = typeof<'T>
        let parameter0     = Expression.Parameter   (typeof<char>   , "ch"      )
        let parameter1     = Expression.Parameter   (typeof<int>    , "index"   )
        let resultVariable = Expression.Variable    (tt             , "result"  )

        let switchCases =
            items
            |> List.map (fun (anyOf,v) ->
                Expression.SwitchCase    (
                    Expression.Assign     (resultVariable, Expression.Constant(v, tt))                  ,
                    anyOf |> Seq.map (fun ch -> Expression.Constant (ch) :> Expression) |> Seq.toArray) )
            |> List.toArray

        let switchStatement =
            Expression.Switch (
                parameter0                                                                          ,
                Expression.Assign        (resultVariable, Expression.Constant(defaultValue, tt))    ,
                switchCases                                                                         )

        let body =
            Expression.Block (
                [|resultVariable|]  ,
                switchStatement     ,
                resultVariable      )

        let lambda =
            Expression.Lambda<Func<char, int, 'T>>(
                body         ,
                parameter0   ,
                parameter1   )

        lambda.Compile ();

    // Generic parsers functions (similar look to FParsec)

    let initialCapacity = 16

    [<Struct>]
    type Substring(i : string, b : int, e : int) =
        member x.Input      = i
        member x.Begin      = b
        member x.End        = e
        member x.Length     = e - b
        member x.IsEmpty    = b = e

        member x.Str        = i.Substring(b, e - b)
        member x.Char idx   = i.[b + idx]

    let emptySubstring  = Substring("",0,0)
    let eosChar         = '\uFFFF'

    type CharTest = FSharpFunc<char,int, bool>

    type CharStream<'UserState>(input : string, userState : 'UserState) =

        let mutable position            = 0
        let mutable noErrorMessages     = true

        member x.StateTag               = position
        member x.Input                  = input
        member x.Position               = position
        member x.UserState              = userState
        member x.IsEndOfStream          = position >= input.Length || position < 0
        member x.NoErrorMessages        = noErrorMessages
        member x.Peek ()                = if x.IsEndOfStream then '\uFFFF' else input.[position]

        member x.SetPosition pos        = position <- pos
        member x.SetNoErrorMessages flag= noErrorMessages <- flag

        member x.Match atLeast atMost (test : CharTest) : Substring =
            Debug.Assert (atLeast >= 0)
            Debug.Assert (atMost >= atLeast)

            let i           = input
            let ``begin``   = position
            let length      = i.Length

            let remaining   = length - ``begin``

            if atLeast > remaining then
                emptySubstring
            else
                let required    = ``begin`` + atLeast
                let ``end``     = ``begin`` + min remaining atMost

                let mutable cont = true
                let mutable iter = 0
                let mutable pos  = ``begin``

                while cont && pos < ``end`` do
                    cont    <- test.Invoke(i.[pos], iter)
                    iter    <- iter + 1
                    pos     <- pos + 1

                let stopped = if cont then pos else pos - 1

                if required > stopped then
                    emptySubstring
                else
                    position <- stopped
                    Substring(i,``begin``, stopped)

        member x.MatchChar (test : CharTest) : char =

            let i           = input
            let pos         = position
            let length      = i.Length

            if pos < length && test.Invoke(i.[pos], 0) then
                position <- pos + 1
                i.[pos]
            else
                eosChar

        member x.SkipWhitespaces () : unit =

            let i           = input
            let length      = i.Length

            let mutable pos  = position

            while   pos < length &&
                    match i.[pos] with
                    | ' '
                    | '\t'
                    | '\n'
                    | '\r'  -> true
                    | _     -> false
                do
                pos <- pos + 1

            position <- pos

    type ErrorMessage =
        | Expected      of string
        | NotExpected   of string

    let noErrors : ErrorMessage list = []

    let expectedChar    (ch     : char)     = 
        Expected  <| "'" + ch.ToString() + "'"
    let expectedString  (s      : string)   = 
        Expected  <| "'" + s.ToString() + "'"
    let expectedAnyOf   (anyOf  : string)   = 
        Expected  <| if anyOf.Length = 1 then "'" + anyOf + "'" else "any char in '" + anyOf + "'"
    let notExpectedAnyOf   (anyOf  : string)   = 
        NotExpected  <| if anyOf.Length = 1 then "'" + anyOf + "'" else "any char in '" + anyOf + "'"

    let inline mergeErrors 
        (noErrorMessages    : bool              )
        (previousStateTag   : int               ) 
        (currentStateTag    : int               ) 
        (previousErrors     : ErrorMessage list ) 
        (currentErrors      : ErrorMessage list ) 
        : ErrorMessage list =
        if noErrorMessages then noErrors
        elif previousStateTag < currentStateTag then
            currentErrors
        else
            currentErrors@previousErrors

    [<Struct>]
    type Reply<'Result>(isOk : bool, result : 'Result, errorMessages : ErrorMessage list) =

        member x.Ok                 = isOk
        member x.Error              = not isOk
        member x.Result             = result
        member x.ErrorMessages      = errorMessages

    type Parser<'Result, 'UserState> = CharStream<'UserState> -> Reply<'Result>

    type ParserResult<'Result, 'UserState> =
        | Success           of 'Result * 'UserState * int
        | Failure           of string * int * 'UserState

    let prettify (ems : ErrorMessage list) (ps : CharStream<'UserState>) =
        let expected    = HashSet<string>()
        let notExpected = HashSet<string>()
        let rec collectMessages (ems : ErrorMessage) =
            match ems with
            | Expected      m   ->  ignore <| expected.Add m
            | NotExpected   m   ->  ignore <| notExpected.Add m

        for em in ems do
            collectMessages em

        let input       =   System.String
                                (
                                    ps.Input
                                    |> Seq.map (fun ch -> if Char.IsWhiteSpace ch then ' ' else ch)
                                    |> Seq.toArray
                                )
        let pos         = ps.Position
        let snippetSize = 80
        let midSnippet  = snippetSize / 2
        let beginSnippet= max 0 <| pos - midSnippet
        let endSnippet  = min input.Length <| pos + midSnippet
        let snippet     = input.Substring(beginSnippet, endSnippet - beginSnippet)
        let indicator   = System.String ('-', pos - beginSnippet)

        let e   = System.String.Join(", ", expected     |> Seq.sort |> Seq.toArray)
        let ne  = System.String.Join(", ", notExpected  |> Seq.sort |> Seq.toArray)
        let reason =
            match e, ne with
            | "","" -> ""
            | l,""  -> "expecting " + l
            | "",r  -> "didn't expect " + r
            | l,r   -> "expected " + l + " and didn't expect " +  r

        sprintf
            "Parse error at position: %d, input:\n%s\n%s^\nFound '%s' but %s"
            pos
            snippet
            indicator
            (input.[pos].ToString())
            reason

    let run (p : Parser<'T, unit>) (s : string) : ParserResult<'T, unit> =
        let ps = CharStream(s, ())
        let r = p ps
        if r.Ok then Success (r.Result, ps.UserState, ps.Position)
        else
            // Failed, now generate error message
            ps.SetPosition 0
            ps.SetNoErrorMessages false
            let r = p ps
            Failure (prettify r.ErrorMessages ps, ps.Position, ps.UserState)

    let inline success (v : 'T) ems    = Reply<'T> (true, v, ems)
    let inline failure ems             = Reply<'T> (false, Unchecked.defaultof<'T>, ems)

    let preturn (v : 'T) : Parser<'T, 'UserState> = fun ps -> success v []

    let debug (p : Parser<'T, 'UserState>) : Parser<'T, 'UserState> =
        fun ps ->
            let r = p ps
            r

    let eof : Parser<unit, 'UserState> =
        let ems = [Expected "EOF"]
        fun ps ->
            if ps.IsEndOfStream then
                success () ems
            else
                failure ems

    let spaces : Parser<unit, 'UserState> =
        fun ps ->
            ps.SkipWhitespaces ()
            success () noErrors

    let skipChar (c : char): Parser<unit, 'UserState> =
        let test = CharTest.Adapt <| fun ch _ -> ch = c
        let ems = [expectedChar c]
        fun ps ->
            let ch = ps.MatchChar test
            if ch <> eosChar then success () ems
            else failure ems

    let orElse (l : Parser<'T, 'UserState>) (r : Parser<'T, 'UserState>) : Parser<'T, 'UserState> =
        fun ps ->
            let stateTag0 = ps.StateTag
            let rl = l ps
            let mutable ems = rl.ErrorMessages
            if rl.Ok then rl
            else
                let stateTag1 = ps.StateTag
                let rr = r ps
                ems <- mergeErrors ps.NoErrorMessages stateTag0 stateTag1 ems rr.ErrorMessages
                if rr.Ok then success rr.Result ems
                else failure ems
    let ( <|> ) = orElse

    let map (p : Parser<'TFrom, 'UserState>) (m : 'TFrom->'TTo) : Parser<'TTo, 'UserState> =
        fun ps ->
            let r = p ps
            if r.Ok then success (m r.Result) r.ErrorMessages
            else failure r.ErrorMessages
    let ( |>> ) = map

    let combine (l : Parser<'L, 'UserState>) (r : Parser<'R, 'UserState>) : Parser<'L*'R, 'UserState> =
        fun ps ->
            let stateTag0 = ps.StateTag
            let rl = l ps
            let mutable ems = rl.ErrorMessages
            if rl.Error then failure rl.ErrorMessages
            else
                let stateTag1 = ps.StateTag
                let rr = r ps
                ems <- mergeErrors ps.NoErrorMessages stateTag0 stateTag1 ems rr.ErrorMessages
                if rr.Error then failure ems
                else success (rl.Result, rr.Result) ems
    let ( .>>. ) = combine

    let keepLeft (l : Parser<'L, 'UserState>) (r : Parser<'R, 'UserState>) : Parser<'L, 'UserState> =
        fun ps ->
            let stateTag0 = ps.StateTag
            let rl = l ps
            let mutable ems = rl.ErrorMessages
            if rl.Error then failure rl.ErrorMessages
            else
                let stateTag1 = ps.StateTag
                let rr = r ps
                ems <- mergeErrors ps.NoErrorMessages stateTag0 stateTag1 ems rr.ErrorMessages
                if rr.Error then failure ems
                else success rl.Result ems
    let ( .>> ) = keepLeft

    let keepRight (l : Parser<'L, 'UserState>) (r : Parser<'R, 'UserState>) : Parser<'R, 'UserState> =
        fun ps ->
            let stateTag0 = ps.StateTag
            let rl = l ps
            let mutable ems = rl.ErrorMessages
            if rl.Error then failure rl.ErrorMessages
            else
                let stateTag1 = ps.StateTag
                let rr = r ps
                ems <- mergeErrors ps.NoErrorMessages stateTag0 stateTag1 ems rr.ErrorMessages
                if rr.Error then failure ems
                else success rr.Result ems
    let ( >>. ) = keepRight

    let skipSatisfyImpl (test : CharTest) (ems : ErrorMessage list) : Parser<unit, 'UserState> =
        fun ps ->
            let ch = ps.MatchChar test
            if ch <> eosChar then success () ems
            else failure ems

    let satisfyImpl (test : CharTest) (ems : ErrorMessage list) : Parser<char, 'UserState> =
        fun ps ->
            let ch = ps.MatchChar test
            if ch <> eosChar then success ch ems
            else failure ems

    let skipAnyOf (s : string) : Parser<unit, 'UserState> =
        let test        = FSharpFuncWrap<char,int,bool>(fastAnyOf s true)
        let ems         = [expectedAnyOf s]
        skipSatisfyImpl test ems

    let anyOf (s : string) : Parser<char, 'UserState> =
        let fastSet     = fastAnyOf s true
        let test        = CharTest.Adapt <| fun ch p -> fastSet.Invoke(ch,p)
        let ems         = [expectedAnyOf s]
        satisfyImpl test ems

    let noneOf (s : string) : Parser<char, 'UserState> =
        let fastSet     = fastAnyOf s false
        let test        = CharTest.Adapt <| fun ch p -> fastSet.Invoke(ch,p)
        let ems         = [notExpectedAnyOf s]
        satisfyImpl test ems

    let digit : Parser<char, 'UserState> =
        let test    = CharTest.Adapt <| fun ch _ ->
            match ch with
            | _ when ch >= '0' && ch <= '9' -> true
            | _ -> false
        let ems     = [Expected "Digit"]
        fun ps -> satisfyImpl test ems ps

    let hex : Parser<char, 'UserState> =
        let test    = CharTest.Adapt <| fun ch _ ->
            match ch with
            | _ when ch >= '0' && ch <= '9' -> true
            | _ when ch >= 'a' && ch <= 'f' -> true
            | _ when ch >= 'A' && ch <= 'F' -> true
            | _ -> false
        let ems = [Expected "HexDigit"]
        fun ps -> satisfyImpl test ems ps

    let pipe3
            (p0 : Parser<'T0, 'UserState>)
            (p1 : Parser<'T1, 'UserState>)
            (p2 : Parser<'T2, 'UserState>)
            (m  : 'T0->'T1->'T2->'T)
            : Parser<'T, 'UserState> =
        let fm = FSharpFunc<_,_,_,_>.Adapt m
        fun ps ->
            let stateTag0 = ps.StateTag
            let r0 = p0 ps
            let mutable ems = r0.ErrorMessages
            if r0.Error then failure ems
            else
                let stateTag1 = ps.StateTag
                let r1 = p1 ps
                ems <- mergeErrors ps.NoErrorMessages stateTag0 stateTag1 ems r1.ErrorMessages
                if r1.Error then failure ems
                else
                    let stateTag2 = ps.StateTag
                    let r2 = p2 ps
                    ems <- mergeErrors ps.NoErrorMessages stateTag1 stateTag2 ems r2.ErrorMessages
                    if r2.Error then failure ems
                    else
                        success (fm.Invoke(r0.Result, r1.Result, r2.Result)) ems

    let pipe4
            (p0 : Parser<'T0, 'UserState>)
            (p1 : Parser<'T1, 'UserState>)
            (p2 : Parser<'T2, 'UserState>)
            (p3 : Parser<'T3, 'UserState>)
            (m  : 'T0->'T1->'T2->'T3->'T)
            : Parser<'T, 'UserState> =
        let fm = FSharpFunc<_,_,_,_,_>.Adapt m
        fun ps ->
            let stateTag0 = ps.StateTag
            let r0 = p0 ps
            let mutable ems = r0.ErrorMessages
            if r0.Error then failure ems
            else
                let stateTag1 = ps.StateTag
                let r1 = p1 ps
                ems <- mergeErrors ps.NoErrorMessages stateTag0 stateTag1 ems r1.ErrorMessages
                if r1.Error then failure ems
                else
                    let stateTag2 = ps.StateTag
                    let r2 = p2 ps
                    ems <- mergeErrors ps.NoErrorMessages stateTag1 stateTag2 ems r2.ErrorMessages
                    if r2.Error then failure ems
                    else
                        let stateTag3 = ps.StateTag
                        let r3 = p3 ps
                        ems <- mergeErrors ps.NoErrorMessages stateTag2 stateTag3 ems r3.ErrorMessages
                        if r3.Error then failure ems
                        else
                            success (fm.Invoke(r0.Result, r1.Result, r2.Result, r3.Result)) ems

    let choice (parsers : Parser<'T, 'UserState> list) : Parser<'T, 'UserState> =
        fun ps ->
            let mutable stateTag    = ps.StateTag
            let mutable ems         = []
            let mutable result      = None
            let mutable remaining   = parsers
            while result.IsNone && remaining.Length > 0 do
                let p = remaining.Head
                remaining <- remaining.Tail
                let newStateTag     = ps.StateTag
                let r               = p ps
                ems                 <- mergeErrors ps.NoErrorMessages stateTag newStateTag ems r.ErrorMessages
                stateTag            <- newStateTag
                if r.Ok then result <- Some r.Result
            
            if result.IsSome then
                success result.Value ems
            else
                failure ems

    let between
            (b : Parser<_, 'UserState>)
            (e : Parser<_, 'UserState>)
            (p : Parser<'T, 'UserState>)
            : Parser<'T, 'UserState> =
            pipe3 b p e <| fun _ v _ -> v

    let charReturn (c : char) (v : 'T) : Parser<'T, 'UserState> =
        let ems     = [expectedChar c]
        let test    = CharTest.Adapt <| fun ch _ -> ch = c
        fun ps ->
            let ch = ps.MatchChar test
            if ch <> eosChar then success v ems
            else failure ems

    let stringReturn (s : string) (v : 'T) : Parser<'T, 'UserState> =
        let length  = s.Length
        let ems     = [expectedString s]
        let test    = CharTest.Adapt <| fun ch p -> s.[p] = ch
        fun ps ->
            let ss = ps.Match length length test
            if not ss.IsEmpty then success v ems
            else failure ems

    let many (p : Parser<'T, 'UserState>) : Parser<'T list, 'UserState> =
        fun ps ->
            let mutable stateTag    = ps.StateTag
            let mutable ems         = []
            let result  = List<'T>(initialCapacity)
            while
                let newStateTag = ps.StateTag in 
                let r           = p ps in 
                let _           = ems <- mergeErrors ps.NoErrorMessages stateTag newStateTag ems r.ErrorMessages in
                let _           = stateTag <- newStateTag in
                if r.Error then false
                else
                    result.Add r.Result
                    true
                do
                ()

            success (result |> Seq.toList) ems

    let manyChars (p : Parser<char, 'UserState>) : Parser<string, 'UserState> =
        fun ps ->
            let mutable stateTag    = ps.StateTag
            let mutable ems         = []
            let result  = StringBuilder()
            while
                let newStateTag = ps.StateTag in // TODO: Why is this in required?
                let r           = p ps in 
                let _           = ems <- mergeErrors ps.NoErrorMessages stateTag newStateTag ems r.ErrorMessages in
                let _           = stateTag <- newStateTag in
                if r.Error then false
                else
                    ignore <| result.Append r.Result
                    true
                do
                ()

            success (result.ToString()) ems

    let sepBy (p : Parser<'T, 'UserState>) (sep : Parser<_, 'UserState>) : Parser<'T list, 'UserState> =
        fun ps ->
            let result  = List<'T>(initialCapacity)

            let mutable stateTag    = ps.StateTag
            let ri                  = p ps
            let mutable ems         = ri.ErrorMessages

            if ri.Error then success [] ems
            else
                let mutable failed = false

                result.Add ri.Result
                while
                    let sepStateTag = ps.StateTag in // TODO: Why is this in required?
                    let rs          = sep ps in  
                    let _           = ems <- mergeErrors ps.NoErrorMessages stateTag sepStateTag ems rs.ErrorMessages in
                    let _           = stateTag <- sepStateTag in
                    if rs.Error then false
                    else
                        let newStateTag = ps.StateTag 
                        let r           = p ps
                        ems             <- mergeErrors ps.NoErrorMessages stateTag newStateTag ems r.ErrorMessages
                        stateTag        <- newStateTag
                        if r.Error then
                            failed <- true
                            false
                        else
                            result.Add r.Result
                            true
                    do
                    ()
                if failed then failure ems
                else
                    success (result |> Seq.toList) ems

    let createParserForwardedToRef () =
        let dummyParser = fun stream -> failwith "a parser created with createParserForwardedToRef was not initialized"
        let r = ref dummyParser
        (fun stream -> !r stream), r : Parser<_,'u> * Parser<_,'u> ref

    let fastChoice (parsers : (string*string option*Parser<'T, 'UserState>) list) : Parser<'T, 'UserState> =
        let choices = parsers |> List.map (fun (anyOf,_,v) -> (anyOf, Some v))
        let fm = fastMap choices None
        let buildString (ss : string list) = 
            let sb = StringBuilder()
            for s in ss do
                ignore <| sb.Append s
            sb.ToString ()
        let ems =
            parsers
            |> List.map (fun (anyOf,l,_) -> match l with | Some label -> expectedString label | _ -> expectedAnyOf anyOf)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toList
        fun ps ->
            if ps.IsEndOfStream then failure ems
            else
                let ch  = ps.Peek ()
                let f   = fm.Invoke(ch, 0)
                if f.IsSome then
                    let p           = f.Value
                    let r           = p ps
                    if r.Ok then success r.Result r.ErrorMessages
                    else failure r.ErrorMessages
                else
                    failure ems

    // JSON parsers specifics (should be FParsec compatible)

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
    let p_wstoken token : Parser<unit, unit>        = p_token token .>> p_ws

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
                noneOf """"\"""
                p_token '\\' >>. (p_escape <|> p_unicodeEscape)
            ]
    let p_stringLiteral : Parser<string, unit>      =
        between (p_token '"') (p_token '"') (manyChars p_char)

    let p_digit1To9     : Parser<char, unit>        = anyOf "123456789"
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

    let rec p_member        : Parser<string*JsonValue, unit> =
        p_stringLiteral .>> p_ws .>> (p_wstoken ':') .>>. p_value
    and p_object        : Parser<JsonValue, unit>        =
        between (p_wstoken '{') (p_token '}') (sepBy p_member (p_wstoken ',') |>> (List.toArray >> JsonValue.Record))
    and p_array         : Parser<JsonValue, unit>        =
        between (p_wstoken '[') (p_token ']') (sepBy p_value (p_wstoken ',') |>> (List.toArray >> JsonValue.Array))
    and p_value     : Parser<JsonValue, unit>    =
        let p =
            lazy
                fastChoice
                    [
                        "n"             , Some "null"   , p_null
                        "t"             , Some "true"   , p_true
                        "f"             , Some "false"  , p_false
                        "\""            , None          , p_string
                        "-0123456789"   , None          , p_number
                        "{"             , None          , p_object
                        "["             , None          , p_array
                    ]
                .>> p_ws
        fun ps -> p.Value ps


    let p_root          : Parser<JsonValue, unit>        = 
        fastChoice
            [
                "{"             , None          , p_object
                "["             , None          , p_array
            ]
        .>> p_ws

    let p_json  = p_ws >>. p_root .>> eof
    let p_jsons = p_ws >>. (many p_root) .>> eof

    type JsonParserNew(jsonText:string, cultureInfo : CultureInfo option, tolerateErrors : bool) =
        member x.Parse () =
            match run p_json jsonText with
            | Success (json, _, pos) -> json
            | Failure (msg, pos, _)  -> failwith msg

        member x.ParseMultiple () =
            match run p_jsons jsonText with
            | Success (json, _, pos) -> json :> seq<JsonValue>
            | Failure (msg, pos, _)  -> failwith msg

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
