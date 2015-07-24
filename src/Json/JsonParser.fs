// --------------------------------------------------------------------------------------
// Copyright (c) Microsoft Corporation 2005-2012.
// This sample code is provided "as is" without warranty of any kind.
// We disclaim all warranties, either express or implied, including the
// warranties of merchantability and fitness for a particular purpose.
//
// A portable & conforming F# parser for JSON data
// --------------------------------------------------------------------------------------

namespace FSharp.Data

module JsonConformingParser =
  open System
  open System.Collections.Generic
  open System.Diagnostics
  open System.Text

  exception JsonParseFailure of string*int

  module internal Tokens =
    [<Literal>]
    let Null      = "null"

    [<Literal>]
    let True      = "true"

    [<Literal>]
    let False     = "false"

    [<Literal>]
    let Digit     = "digit"

    [<Literal>]
    let HexDigit  = "hexdigit"

    [<Literal>]
    let EOS       = "EOS"

    [<Literal>]
    let NewLine   = "NEWLINE"

  module internal Details =

    [<Literal>]
    let DefaultSize = 16

    [<Literal>]
    let ErrorPrelude = "Failed to parse input as JSON"

    let inline clamp v min max =
      if v < min then min
      elif v > max then max
      else v

    // Min & Max Exponent of float (double)
    //  https://en.wikipedia.org/wiki/Double-precision_floating-point_format

    [<Literal>]
    let MinimumPow10  = -1022

    [<Literal>]
    let MaximumPow10  = 1023

    let Pow10Table =
      [|
        for i in MinimumPow10..MaximumPow10 -> pown 10. i
      |]

    let inline pow10 n = Pow10Table.[clamp (n - MinimumPow10) 0 (Pow10Table.Length - 1)]

    let inline isWhiteSpace (c : char) : bool =
      c = ' ' || c = '\t' || c = '\n' || c = '\r'

    let inline isDigit (c : char) : bool =
      c >= '0' && c <= '9'

    let rec charsContains (i : int) (v : char) (vs : char []) : bool =
      if i < vs.Length then
        vs.[i] = v || charsContains (i + 1) v vs
      else
        false

    let emptyString           = ""
    let nullValue             = JsonValue.Null
    let trueValue             = JsonValue.Boolean true
    let falseValue            = JsonValue.Boolean false
    let inline boolValue b    = if b then trueValue else falseValue
    let inline numberValue v  = JsonValue.Float v
    let inline stringValue s  = JsonValue.String s
    let inline arrayValue vs  = JsonValue.Array vs
    let inline objectValue ms = JsonValue.Record ms

    type JsonParser(s : string, epos : int) =
      let sb                = StringBuilder DefaultSize
      let mutable pos       = 0
      let discardedValues   = Stack<ResizeArray<JsonValue>>         DefaultSize
      let discardedMembers  = Stack<ResizeArray<string*JsonValue>>  DefaultSize

      let filter f      = f |> Seq.sort |> Seq.distinct |> Seq.toArray

      let mutable expected      = []
      let mutable expectedChars = []
      let mutable unexpected    = []

      member x.allExpected      = filter expected
      member x.allExpectedChars = filter expectedChars
      member x.allUnexpected    = filter unexpected

      member x.position     = pos

      member inline x.getValues () : ResizeArray<JsonValue> =
        if discardedValues.Count > 0 then discardedValues.Pop ()
        else ResizeArray<JsonValue> DefaultSize

      member inline x.discardvalues (vs : ResizeArray<JsonValue>) : unit =
        vs.Clear ()
        discardedValues.Push vs

      member inline x.getMembers () : ResizeArray<string*JsonValue> =
        if discardedMembers.Count > 0 then discardedMembers.Pop ()
        else ResizeArray<string*JsonValue> DefaultSize

      member inline x.discardMembers (ms : ResizeArray<string*JsonValue>) : unit =
        ms.Clear ()
        discardedMembers.Push ms

      member inline x.neos        : bool = pos < s.Length
      member inline x.eos         : bool = pos >= s.Length
      member inline x.ch          : char = s.[pos]
      member inline x.adv ()      : unit = pos <- pos + 1

      member x.Unexpected     (pos : int, token : string) = if epos = pos then unexpected     <- token::unexpected
      member x.Expected       (pos : int, token : string) = if epos = pos then expected       <- token::expected
      member x.ExpectedChar   (pos : int, token : char  ) = if epos = pos then expectedChars  <- token::expectedChars
      member x.ExpectedChars  (pos : int, tokens: string) =
        let e = tokens.Length - 1
        for i = 0 to e do
          x.ExpectedChar (pos, tokens.[i])

      member x.raise_Eos ()       : bool =
        x.Unexpected (pos, Tokens.EOS)
        false

      member x.raise_Value ()     : bool =
        x.Expected      (pos, Tokens.Null )
        x.Expected      (pos, Tokens.True )
        x.Expected      (pos, Tokens.False)
        x.Expected      (pos, Tokens.Digit)
        x.ExpectedChars (pos, "\"{[-")
        false

      member x.raise_RootValue () : bool =
        x.ExpectedChars (pos, "{[")
        false

      member inline x.consume_WhiteSpace () : bool =
        let l = s.Length
        while pos < l && (isWhiteSpace s.[pos]) do
          x.adv ()
        true

      member inline x.test_Char (c : char) : bool =
        x.neos && x.ch  = c

      member inline x.tryConsume_Char (c : char) : bool =
        if x.eos then x.raise_Eos ()
        elif x.ch = c then
          x.adv ()
          true
        else
          x.ExpectedChar (pos, c)
          false

  // inline causes DEBUG mode to crash (because F# creates tuples of pointers
  #if DEBUG
      member x.tryParse_AnyOf2 (first : char, second : char, r : char byref) : bool =
  #else
      member inline x.tryParse_AnyOf2 (first : char, second : char, r : char byref) : bool =
  #endif
        if x.eos then x.raise_Eos ()
        else
          let c = x.ch
          if c = first || c = second then
            r <- c
            x.adv ()
            true
          else
            x.ExpectedChar (pos, first)
            x.ExpectedChar (pos, second)
            false

      member inline x.tryConsume_Token (tk : string) : bool =
        let tkl           = tk.Length
        let spos          = pos
        let mutable tpos  = 0

        while tpos < tkl && tk.[tpos] = s.[pos] do
          tpos <- tpos + 1
          x.adv ()

        if tpos = tkl then true
        else
          // To support error reporting, move back on failure
          pos <- spos
          false

      member x.tryParse_Null (v : JsonValue byref) : bool =
        if x.tryConsume_Token Tokens.Null then
          v <- nullValue
          true
        else
          x.raise_Value ()

      member x.tryParse_True (v : JsonValue byref) : bool =
        if x.tryConsume_Token Tokens.True then
          v <- trueValue
          true
        else
          x.raise_Value ()

      member x.tryParse_False (v : JsonValue byref) : bool =
        if x.tryConsume_Token Tokens.False then
          v <- falseValue
          true
        else
          x.raise_Value ()

      member x.tryParse_UInt (first : bool, r : float byref) : bool =
        let z = float '0'
        if x.eos then ignore <| x.raise_Eos (); not first
        else
          let c = x.ch
          if c >= '0' && c <= '9' then
            x.adv ()
            r <- 10.0*r + (float c - z)
            x.tryParse_UInt (false, &r)
          else
            x.Expected (pos, Tokens.Digit)
            not first

      member x.tryParse_UInt0 (r : float byref) : bool =
        // tryParse_UInt0 only consumes 0 if input is 0123, this in order to be conformant with spec
        let zero = x.tryConsume_Char '0'

        if zero then
          r <- 0.0
          true
        else
          x.tryParse_UInt (true, &r)

      member x.tryParse_Fraction (r : float byref) : bool =
        if x.tryConsume_Char '.' then
          let spos        = pos
          let mutable uf  = 0.0
          if x.tryParse_UInt (true, &uf) then
            r <- (float uf) * (pow10 (spos - pos))
            true
          else
            false
        else
          true  // Fraction is optional

      member x.tryParse_Exponent (r : int byref) : bool =
        let mutable exp = ' '
        if x.tryParse_AnyOf2 ('e', 'E', &exp) then
          let mutable sign = '+'
          // Ignore as sign is optional
          ignore <| x.tryParse_AnyOf2 ('+', '-', &sign)
          // TODO: Parsing exponent as float seems unnecessary
          let mutable ue = 0.0
          if x.tryParse_UInt (true, &ue) then
            let inline sign v = if sign = '-' then -v else v
            r <- sign (int ue)
            true
          else
            false
        else
          true  // Fraction is optional

      member x.tryParse_Number (v : JsonValue byref) : bool =
        let hasSign       = x.tryConsume_Char '-'
        let inline sign v = if hasSign then -v else v

        let mutable i = 0.0
        let mutable f = 0.0
        let mutable e = 0

        let result =
          x.tryParse_UInt0        (&i)
          && x.tryParse_Fraction  (&f)
          && x.tryParse_Exponent  (&e)

        if result then
          v <- numberValue (sign ((i + f) * (pow10 e)))
          true
        else
          false

      member x.tryParse_UnicodeChar (n : int, r : int) : bool =
        if n = 0 then
          ignore <| sb.Append (char r)
          true
        elif x.eos then x.raise_Eos ()
        else
          let sr  = r <<< 4
          let   c = x.ch
          if    c >= '0' && c <= '9'  then x.adv () ; x.tryParse_UnicodeChar (n - 1, sr + (int c - int '0'))
          elif  c >= 'A' && c <= 'F'  then x.adv () ; x.tryParse_UnicodeChar (n - 1, sr + (int c - int 'A' + 10))
          elif  c >= 'a' && c <= 'f'  then x.adv () ; x.tryParse_UnicodeChar (n - 1, sr + (int c - int 'a' + 10))
          else
            x.Expected (pos, Tokens.HexDigit)
            false

      member x.tryParse_Chars (b : int) : bool =
        let inline app (c : char) = ignore <| sb.Append c
        let inline seq e          = ignore <| sb.Append (s, b, e - b)

        if x.eos then x.raise_Eos ()
        else
          let c = x.ch
          match c with
          | '"'         -> seq pos; true
          | '\r' | '\n' -> x.Unexpected (pos, Tokens.NewLine); false
          | '\\'        ->
            seq pos
            x.adv ()
            if x.eos then x.raise_Eos ()
            else
              let e = x.ch
              let result =
                match e with
                | '"'
                | '\\'
                | '/' -> app e    ; x.adv (); true
                | 'b' -> app '\b' ; x.adv (); true
                | 'f' -> app '\f' ; x.adv (); true
                | 'n' -> app '\n' ; x.adv (); true
                | 'r' -> app '\r' ; x.adv (); true
                | 't' -> app '\t' ; x.adv (); true
                | 'u' ->
                  x.adv ()
                  x.tryParse_UnicodeChar (4, 0)
                | _ ->
                  x.ExpectedChars (pos, "\"\\/bfnrtu")
                  false
              result && x.tryParse_Chars pos
          | _           ->
            x.adv ()
            x.tryParse_Chars b

      member x.tryParse_ToStringBuilder () : bool =
        ignore <| sb.Clear ()
        x.tryConsume_Char     '"'
        && x.tryParse_Chars   pos
        && x.tryConsume_Char  '"'

      member x.tryParse_MemberKey (v : string byref) : bool =
        x.tryParse_ToStringBuilder ()
        && (v <- sb.ToString (); true)

      member x.tryParse_String (v : JsonValue byref) : bool =
        x.tryParse_ToStringBuilder ()
        && (v <- stringValue (sb.ToString ()); true)

      member inline x.tryConsume_Delimiter (first : bool) : bool =
        if first then true
        else
          x.tryConsume_Char       ','
          && x.consume_WhiteSpace ()

      member x.tryParse_ArrayValues (first : bool, vs : ResizeArray<JsonValue>) : bool =
        if x.test_Char ']' then
          true
        else
          let mutable v = nullValue
          x.tryConsume_Delimiter    first
          && x.tryParse_Value       (&v)
          && (vs.Add v; true)
          && x.tryParse_ArrayValues (false, vs)

      member x.tryParse_Array (v : JsonValue byref) : bool =
        let vs = x.getValues ()
        x.tryConsume_Char         '['
        && x.consume_WhiteSpace   ()
        && x.tryParse_ArrayValues (true, vs)
        && x.tryConsume_Char      ']'
        && (v <- arrayValue (vs.ToArray ()); x.discardvalues vs; true)

      member x.tryParse_ObjectMembers (first : bool, ms : ResizeArray<string*JsonValue>) : bool =
        if x.test_Char '}' then
          true
        else
          let mutable k = emptyString
          let mutable v = nullValue
          x.tryConsume_Delimiter      first
          && x.tryParse_MemberKey     (&k)
          && x.consume_WhiteSpace     ()
          && x.tryConsume_Char        ':'
          && x.consume_WhiteSpace     ()
          && x.tryParse_Value         (&v)
          && (ms.Add (k,v); true)
          && x.tryParse_ObjectMembers (false, ms)

      member x.tryParse_Object (v : JsonValue byref) : bool =
        let ms = x.getMembers ()
        x.tryConsume_Char           '{'
        && x.consume_WhiteSpace     ()
        && x.tryParse_ObjectMembers (true, ms)
        && x.tryConsume_Char        '}'
        && (v <- objectValue (ms.ToArray ()); x.discardMembers ms;true)

      member x.tryParse_Value (v : JsonValue byref): bool =
        if x.eos then x.raise_Eos ()
        else
          let result =
            match x.ch with
            | 'n'                 -> x.tryParse_Null    (&v)
            | 't'                 -> x.tryParse_True    (&v)
            | 'f'                 -> x.tryParse_False   (&v)
            | '['                 -> x.tryParse_Array   (&v)
            | '{'                 -> x.tryParse_Object  (&v)
            | '"'                 -> x.tryParse_String  (&v)
            | '-'                 -> x.tryParse_Number  (&v)
            | c when isDigit c    -> x.tryParse_Number  (&v)
            | _                   -> x.raise_Value      ()
          result && x.consume_WhiteSpace ()

      member x.tryParse_RootValue (v : JsonValue byref) : bool =
        if x.eos then x.raise_Eos ()
        else
          let result =
            match x.ch with
            | '['                 -> x.tryParse_Array  (&v)
            | '{'                 -> x.tryParse_Object (&v)
            | _                   -> x.raise_RootValue ()
          result && x.consume_WhiteSpace ()

      member x.tryParse_RootValues (vs : ResizeArray<JsonValue>) : bool =
        let mutable v = nullValue
        x.tryParse_RootValue (&v)
        && (vs.Add v; true)
        && x.tryParse_RootValues vs

      member x.tryParse_Eos () : bool =
        if x.eos then
          true
        else
          x.Expected (pos, Tokens.EOS)
          false

    let tryParse (jp : JsonParser) (v : JsonValue byref) : bool =
      jp.consume_WhiteSpace     ()
      && jp.tryParse_RootValue  (&v)
      && jp.tryParse_Eos        ()

    let tryParseMultiple (jp : JsonParser) (vs : ResizeArray<JsonValue>) : bool =
      jp.consume_WhiteSpace     ()
      && jp.tryParse_RootValues vs
      && jp.tryParse_Eos        ()

    let raiseParseFailure (pos : int) : 'T =
      raise (JsonParseFailure (ErrorPrelude, pos))

    let raiseExtendedParseFailure (input : string) (pos : int) (ejp : JsonParser) : 'T =
      let sb = StringBuilder ()
      let inline str  (s : string)  = ignore <| sb.Append s
      let inline strl (s : string)  = ignore <| sb.AppendLine s
      let inline ch   (c : char)    = ignore <| sb.Append c
      let inline line ()            = ignore <| sb.AppendLine ()

      let e =
        Seq.concat
          [|
            ejp.allExpectedChars  |> Seq.map (fun c -> "'" + (c.ToString ()) + "'")
            upcast ejp.allExpected
          |]
        |> Seq.toArray
      let ue = ejp.allUnexpected

      let values prefix (vs : string []) =
        if vs.Length = 0 then ()
        else
          line ()
          str prefix
          let e = vs.Length - 1
          for i = 0 to e do
            let v = vs.[i]
            if i = 0 then ()
            elif i = e then str " or "
            else str ", "
            str v

      let windowSize = 60
      let windowBegin,windowEnd,windowPos =
        if input.Length < windowSize then
          0, input.Length - 1, pos
        else
          let hs  = windowSize / 2
          let b   = pos - hs
          let e   = pos + hs
          let ab  = max 0 b
          let ae  = min (input.Length - 1) (e + ab - b)
          let ap  = pos - ab
          ab, ae, ap

      strl ErrorPrelude
      for i = windowBegin to windowEnd do
        let c =
          match input.[i] with
          | '\n'
          | '\r'  -> ' '
          | c     -> c
        ch c
      line ()
      ignore <| sb.Append ('-', windowPos)
      str "^ Pos: "
      ignore <| sb.Append pos
      values "Expected: " e
      values "Unexpected: " ue

      raise (JsonParseFailure (sb.ToString (), pos))

  open Details

  let parse (extendedErrorInfo : bool) (input : string) : JsonValue =
    let mutable v = nullValue
    let jp        = JsonParser (input, Int32.MaxValue)
    let result    = tryParse jp &v

    if result then v
    elif not extendedErrorInfo then raiseParseFailure jp.position
    else
      let ejp     = JsonParser (input, jp.position)
      let eresult = tryParse ejp &v
      Debug.Assert (not eresult)

      raiseExtendedParseFailure input jp.position ejp

  let parseMultiple (extendedErrorInfo : bool) (input : string) : JsonValue [] =
    let vs        = ResizeArray<JsonValue> DefaultSize
    let jp        = JsonParser (input, Int32.MaxValue)
    let result    = tryParseMultiple jp vs

    if result then vs.ToArray ()
    elif not extendedErrorInfo then raiseParseFailure jp.position
    else
      let ejp     = JsonParser (input, jp.position)
      let eresult = tryParseMultiple ejp vs
      Debug.Assert (not eresult)

      raiseExtendedParseFailure input jp.position ejp
