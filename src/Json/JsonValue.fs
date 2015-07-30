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
open System.ComponentModel
open System.Globalization
open System.Runtime.InteropServices
open System.Text
open FSharp.Data
open FSharp.Data.Runtime

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
[<StructuredFormatDisplay("{_Print}")>]
type JsonValue =
  | String of string
  | Number of decimal
  | Float of float
  | Record of properties:(string * JsonValue)[]
  | Array of elements:JsonValue[]
  | Boolean of bool
  | Null

  /// [omit]
  [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
  [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
  member x._Print = x.ToString()

  /// Serializes the JsonValue to the specified System.IO.TextWriter.
  member x.WriteTo (w:TextWriter, saveOptions) =

    let newLine =
      if saveOptions = JsonSaveOptions.None then
        fun indentation plus ->
          w.WriteLine()
          System.String(' ', indentation + plus) |> w.Write
      else
        fun _ _ -> ()

    let propSep =
      if saveOptions = JsonSaveOptions.None then "\": "
      else "\":"

    let rec serialize indentation = function
      | Null -> w.Write "null"
      | Boolean b -> w.Write(if b then "true" else "false")
      | Number number -> w.Write number
      | Float number -> w.Write number
      | String s ->
          w.Write "\""
          JsonValue.JsonStringEncodeTo w s
          w.Write "\""
      | Record properties ->
          w.Write "{"
          for i = 0 to properties.Length - 1 do
            let k,v = properties.[i]
            if i > 0 then w.Write ","
            newLine indentation 2
            w.Write "\""
            JsonValue.JsonStringEncodeTo w k
            w.Write propSep
            serialize (indentation + 2) v
          newLine indentation 0
          w.Write "}"
      | Array elements ->
          w.Write "["
          for i = 0 to elements.Length - 1 do
            if i > 0 then w.Write ","
            newLine indentation 2
            serialize (indentation + 2) elements.[i]
          if elements.Length > 0 then
            newLine indentation 0
          w.Write "]"

    serialize 0 x

  // Encode characters that are not valid in JS string. The implementation is based
  // on https://github.com/mono/mono/blob/master/mcs/class/System.Web/System.Web/HttpUtility.cs
  static member internal JsonStringEncodeTo (w:TextWriter) (value:string) =
    if String.IsNullOrEmpty value then ()
    else
      for i = 0 to value.Length - 1 do
        let c = value.[i]
        let ci = int c
        if ci >= 0 && ci <= 7 || ci = 11 || ci >= 14 && ci <= 31 then
          w.Write("\\u{0:x4}", ci) |> ignore
        else
          match c with
          | '\b' -> w.Write "\\b"
          | '\t' -> w.Write "\\t"
          | '\n' -> w.Write "\\n"
          | '\f' -> w.Write "\\f"
          | '\r' -> w.Write "\\r"
          | '"'  -> w.Write "\\\""
          | '\\' -> w.Write "\\\\"
          | _    -> w.Write c

  member x.ToString saveOptions =
    let w = new StringWriter(CultureInfo.InvariantCulture)
    x.WriteTo(w, saveOptions)
    w.GetStringBuilder().ToString()

  override x.ToString() = x.ToString(JsonSaveOptions.None)

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

exception JsonParseFailure of string*int
  with
    override x.Message =
      match x :> exn with
      | JsonParseFailure (msg,_)  -> msg
      | _                         -> "JsonParseFailure" // In order to suppress warning

module JsonConformingParser =
  open System
  open System.Collections.Generic
  open System.Diagnostics
  open System.Text


  module internal Tokens =
    [<Literal>]
    let Null      = "null"

    [<Literal>]
    let True      = "true"

    [<Literal>]
    let False     = "false"

    [<Literal>]
    let Char              = "char"

    [<Literal>]
    let Digit     = "digit"

    [<Literal>]
    let HexDigit  = "hexdigit"

    [<Literal>]
    let EOS       = "EOS"

    [<Literal>]
    let NewLine   = "NEWLINE"

    [<Literal>]
    let Escapes           = "\"\\/bfnrtu"

    [<Literal>]
    let ValuePreludes     = "\"{[-"

    [<Literal>]
    let RootValuePreludes = "{["

    [<Literal>]
    let OutOfRange        = "'Number out of range'"

  module internal Details =

    [<Literal>]
    let DefaultSize = 16

    [<Literal>]
    let ErrorPrelude = "Failed to parse input as JSON"

    let inline clamp v min max =
      if v < min then min
      elif v > max then max
      else v

    // Min & Max Exponent of float (double) convert to decimal (double are in essence a binary number)
    //  https://en.wikipedia.org/wiki/Double-precision_floating-point_format

    [<Literal>]
    let MinimumPow10F = -307

    [<Literal>]
    let MaximumPow10F = 307

    let Pow10TableF =
      [|
        for i in MinimumPow10F..MaximumPow10F -> pown 10. i
      |]

    let inline pow10f n = Pow10TableF.[clamp (n - MinimumPow10F) 0 (Pow10TableF.Length - 1)]

    [<Literal>]
    let MinimumPow10D = 0

    [<Literal>]
    let MaximumPow10D = 28

    let Pow10TableD =
      [|
        for i in MinimumPow10D..MaximumPow10D -> if i = 0 then 1M else pown 10M i
      |]

    let inline pow10d n = Pow10TableD.[clamp (n - MinimumPow10D) 0 (Pow10TableD.Length - 1)]

    let inline isWhiteSpace (c : char) : bool =
      c = ' ' || c = '\t' || c = '\n' || c = '\r'

    let inline isDigit (c : char) : bool =
      c >= '0' && c <= '9'

    let rec charsContains (i : int) (v : char) (vs : char []) : bool =
      if i < vs.Length then
        vs.[i] = v || charsContains (i + 1) v vs
      else
        false

    module UInt96 =
      let inline trim (ui : uint64) : uint32 = uint32 ui

      let inline addc 
        (cc : uint32 byref) 
        (l  : uint32 byref) 
        (r  : uint32      )  : unit = 
        let s = uint64 l + uint64 r

        l   <- trim s
        cc  <- cc + trim (s >>> 32)

      let add 
        (l0 : uint32 byref)
        (l1 : uint32 byref)
        (l2 : uint32 byref)
        (r  : uint32      ) : bool = 
        let mutable c0 = 0u
        let mutable c1 = 0u
        let mutable c2 = 0u

        addc &c0 &l0 r 
        addc &c1 &l1 c0
        addc &c2 &l2 c1

        c2 = 0u

      let inline mul10c 
        (cc : uint32 byref)
        (l  : uint32 byref) : unit = 
        let s = uint64 l * 10UL

        l   <- trim s
        cc  <- cc + trim (s >>> 32)

      let mul10 
        (l0 : uint32 byref) 
        (l1 : uint32 byref)
        (l2 : uint32 byref) : bool = 
        let mutable c0 = 0u
        let mutable c1 = 0u
        let mutable c2 = 0u

        mul10c &c0 &l0
        mul10c &c1 &l1
        mul10c &c2 &l2

        addc &c1 &l1 c0
        addc &c2 &l2 c1

        c2 = 0u

    let emptyString           = ""
    let nullValue             = JsonValue.Null
    let trueValue             = JsonValue.Boolean true
    let falseValue            = JsonValue.Boolean false
    let inline boolValue b    = if b then trueValue else falseValue
    let inline floatValue v   = JsonValue.Float v
    let inline decimalValue v = JsonValue.Number v
    let inline stringValue s  = JsonValue.String s
    let inline arrayValue vs  = JsonValue.Array vs
    let inline objectValue ms = JsonValue.Record ms

    // TODO: mrange - Lift in latest error message updates from MiniJson

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

      member x.raise_OutOfRange (): bool =
        x.Unexpected (pos, Tokens.OutOfRange)
        false

      member x.raise_Value ()     : bool =
        x.Expected      (pos, Tokens.Null )
        x.Expected      (pos, Tokens.True )
        x.Expected      (pos, Tokens.False)
        x.Expected      (pos, Tokens.Digit)
        x.ExpectedChars (pos, Tokens.ValuePreludes)
        false

      member x.raise_RootValue () : bool =
        x.ExpectedChars (pos, Tokens.RootValuePreludes)
        false

      member x.raise_Char () : bool =
        x.Expected (pos, Tokens.Char)
        false

      member x.raise_Digit () : bool =
        x.Expected (pos, Tokens.Digit)
        false

      member x.raise_HexDigit () : bool =
        x.Expected (pos, Tokens.HexDigit)
        false

      member x.raise_Escapes () : bool =
        x.ExpectedChars (pos, Tokens.Escapes)
        false

      member inline x.consume_WhiteSpace () : bool =
        let l = s.Length
        while pos < l && (isWhiteSpace s.[pos]) do
          x.adv ()
        true

      member inline x.test_Char (c : char) : bool =
        x.neos && x.ch  = c

      member inline x.tryConsume_Char (c : char) : bool =
        if x.eos then 
          x.ExpectedChar (pos, c)
          x.raise_Eos ()
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
        if x.eos then 
          x.ExpectedChar (pos, first)
          x.ExpectedChar (pos, second)
          x.raise_Eos ()
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

      member x.tryParse_UInt32 (first : bool, r : uint32 byref) : bool =
        let z = uint64 '0'
        if x.eos then x.raise_Digit () || x.raise_Eos () || not first
        else
          let c = x.ch
          if c >= '0' && c <= '9' then
            x.adv ()
            let nr = 10UL*uint64 r + (uint64 c - z)
            if (nr >>> 32) = 0UL then
              r <- uint32 nr
              x.tryParse_UInt32 (false, &r)
            else
              x.raise_OutOfRange ()
          else
            x.raise_Digit () || not first

      member x.tryParse_UInt96 (first : bool, r0 : uint32 byref, r1 : uint32 byref, r2 : uint32 byref) : bool =
        let z = uint32 '0'
        if x.eos then x.raise_Digit () || x.raise_Eos () || not first
        else
          let c = x.ch
          if c >= '0' && c <= '9' then
            x.adv ()
            let a = uint32 c - z
            if (UInt96.mul10 &r0 &r1 &r2) && (UInt96.add &r0 &r1 &r2 a) then
              x.tryParse_UInt96 (false, &r0, &r1, &r2)
            else
              x.raise_OutOfRange ()
          else
            x.raise_Digit () || not first

      member x.tryParse_UInt96_0 (r0 : uint32 byref, r1 : uint32 byref, r2 : uint32 byref) : bool =
        // tryParse_UInt0 only consumes 0 if input is 0123, this in order to be conformant with spec
        let zero = x.tryConsume_Char '0'

        if zero then
          true
        else
          x.tryParse_UInt96 (true, &r0, &r1, &r2)

      member x.tryParse_Fraction96 (r0 : uint32 byref, r1 : uint32 byref, r2 : uint32 byref, scale : int byref) : bool =
        if x.tryConsume_Char '.' then
          let spos        = pos
          if x.tryParse_UInt96 (true, &r0, &r1, &r2) then
            scale <- pos - spos
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
          let mutable ue = 0u
          if x.tryParse_UInt32 (true, &ue) then
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

        let mutable r0    = 0u
        let mutable r1    = 0u
        let mutable r2    = 0u
        let mutable scale = 0
        let mutable exp   = 0

        let result =
          x.tryParse_UInt96_0       (&r0, &r1, &r2)
          && x.tryParse_Fraction96  (&r0, &r1, &r2, &scale)
          && x.tryParse_Exponent    (&exp)

        if result then
          let effectiveExp = exp - scale

          if r0 = 0u && r1 = 0u && r2 = 0u then
            v <- decimalValue (sign 0M)
            true
          elif effectiveExp >= 0 then
            // TODO: This can throw
            let d = pow10d effectiveExp * Decimal (int r0, int r1, int r2, hasSign, 0uy)
            v <- decimalValue d
            true
          elif effectiveExp >= -28 then
            let d = Decimal (int r0, int r1, int r2, hasSign, byte -effectiveExp)
            v <- decimalValue d
            true
          else
            let d = Decimal (int r0, int r1, int r2, hasSign, 0uy)
            let f = float d
            v <- floatValue (f * (pow10f effectiveExp))
            true
(*
          else
            x.raise_OutOfRange ()
*)
        else
          false

      member x.tryParse_UnicodeChar (n : int, r : int) : bool =
        if n = 0 then
          ignore <| sb.Append (char r)
          true
        elif x.eos then x.raise_HexDigit () || x.raise_Eos ()
        else
          let sr  = r <<< 4
          let   c = x.ch
          if    c >= '0' && c <= '9'  then x.adv () ; x.tryParse_UnicodeChar (n - 1, sr + (int c - int '0'))
          elif  c >= 'A' && c <= 'F'  then x.adv () ; x.tryParse_UnicodeChar (n - 1, sr + (int c - int 'A' + 10))
          elif  c >= 'a' && c <= 'f'  then x.adv () ; x.tryParse_UnicodeChar (n - 1, sr + (int c - int 'a' + 10))
          else
            x.raise_HexDigit ()

      member x.tryParse_Chars (b : int) : bool =
        let inline app (c : char) = ignore <| sb.Append c
        let inline seq e          = ignore <| sb.Append (s, b, e - b)

        if x.eos then x.raise_Char () || x.raise_Eos ()
        else
          let c = x.ch
          match c with
          | '"'         -> seq pos; true
          | '\r' | '\n' -> x.Unexpected (pos, Tokens.NewLine); false
          | '\\'        ->
            seq pos
            x.adv ()
            if x.eos then x.raise_Escapes () || x.raise_Eos ()
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
                | _ -> x.raise_Escapes ()
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
        if x.eos then x.raise_Value () || x.raise_Eos ()
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
        if x.eos then x.raise_RootValue () || x.raise_Eos ()
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


// --------------------------------------------------------------------------------------
// JsonValue extensions
// --------------------------------------------------------------------------------------

type JsonValue with

  static member internal ParseImpl(text, extendedErrorInfo : bool option) =
    JsonConformingParser.parse (defaultArg extendedErrorInfo true) text

  static member internal ParseMultipleImpl(text, extendedErrorInfo : bool option) =
    JsonConformingParser.parseMultiple (defaultArg extendedErrorInfo true) text

  /// Parses the specified JSON string
  static member Parse(text, ?extendedErrorInfo) =
    JsonValue.ParseImpl(text, extendedErrorInfo)

  /// Loads JSON from the specified stream
  static member Load(stream:Stream, ?extendedErrorInfo) =
    use reader = new StreamReader(stream)
    let text = reader.ReadToEnd()
    JsonValue.ParseImpl(text, extendedErrorInfo)

  /// Loads JSON from the specified reader
  static member Load(reader:TextReader, ?extendedErrorInfo) =
    let text = reader.ReadToEnd()
    JsonValue.ParseImpl(text, extendedErrorInfo)

  /// Loads JSON from the specified uri asynchronously
  static member AsyncLoad(uri:string, ?extendedErrorInfo) = async {
    let! reader = IO.asyncReadTextAtRuntime false "" "" "JSON" "" uri
    let text = reader.ReadToEnd()
    return JsonValue.ParseImpl(text, extendedErrorInfo)
  }

  /// Loads JSON from the specified uri
  static member Load(uri:string, ?extendedErrorInfo) =
    JsonValue.AsyncLoad(uri, defaultArg extendedErrorInfo true)
    |> Async.RunSynchronously

  /// Parses the specified string into multiple JSON values
  static member ParseMultiple(text, ?extendedErrorInfo) =
    JsonValue.ParseMultipleImpl(text, extendedErrorInfo)

  /// Sends the JSON to the specified uri. Defaults to a POST request.
  member x.Request(uri:string, [<Optional>] ?httpMethod, [<Optional>] ?headers:seq<_>) =
    let httpMethod = defaultArg httpMethod HttpMethod.Post
    let headers = defaultArg (Option.map List.ofSeq headers) []
    let headers =
        if headers |> List.exists (fst >> (=) (fst (HttpRequestHeaders.UserAgent "")))
        then headers
        else HttpRequestHeaders.UserAgent "F# Data JSON Type Provider" :: headers
    let headers = HttpRequestHeaders.ContentType HttpContentTypes.Json :: headers
    Http.Request(
      uri,
      body = TextRequest (x.ToString(JsonSaveOptions.DisableFormatting)),
      headers = headers,
      httpMethod = httpMethod)

  /// Sends the JSON to the specified uri. Defaults to a POST request.
  member x.RequestAsync(uri:string, [<Optional>] ?httpMethod, [<Optional>] ?headers:seq<_>) =
    let httpMethod = defaultArg httpMethod HttpMethod.Post
    let headers = defaultArg (Option.map List.ofSeq headers) []
    let headers =
        if headers |> List.exists (fst >> (=) (fst (HttpRequestHeaders.UserAgent "")))
        then headers
        else HttpRequestHeaders.UserAgent "F# Data JSON Type Provider" :: headers
    let headers = HttpRequestHeaders.ContentType HttpContentTypes.Json :: headers
    Http.AsyncRequest(
      uri,
      body = TextRequest (x.ToString(JsonSaveOptions.DisableFormatting)),
      headers = headers,
      httpMethod = httpMethod)

  [<Obsolete("Please use JsonValue.Request instead")>]
  member x.Post(uri:string, [<Optional>] ?headers) =
    x.Request(uri, ?headers = headers)
