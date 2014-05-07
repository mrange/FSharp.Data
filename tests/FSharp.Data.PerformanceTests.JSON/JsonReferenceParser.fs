module JsonReferenceParser
    open FSharp.Data
    open FParsec

    // JSON reference parser, uses FParsec
    // ===================================

    // The intention is to be strict and match the spec http://json.org as closely as possible
    // The reference parser is used as a yardstick to compare the performance and correctness
    // of the actual FSharp.Data JSON parser

    // Why isn't the reference parser used as default parser for FSharp.Data?
    // ----------------------------------------------------------------------
    // 1. FSharp.Data wants to have minimal dependencies to simplify deployment.
    //    There is a bit of irony here and hopefully with source code project in VS it will
    //    be easier to embed dependencies.
    // 2. The reference parser is slower than the FSharp.Data parser
    //    The reference parser does more though, it's stricter and provides superior 
    //    error messages (thanks to FParsec) but in the end developers asks for 
    //    performance for the happy path.

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

    let rec p_member    : Parser<string*JsonValue, unit> =
        p_stringLiteral .>> p_ws .>> (p_wstoken ':') .>>. p_value
    and p_object        : Parser<JsonValue, unit>        =
        between (p_wstoken '{') (p_token '}') (sepBy p_member (p_wstoken ',') |>> (List.toArray >> JsonValue.Record))
    and p_array         : Parser<JsonValue, unit>        =
        between (p_wstoken '[') (p_token ']') (sepBy p_value (p_wstoken ',') |>> (List.toArray >> JsonValue.Array))
    and p_value         : Parser<JsonValue, unit>    =
        let p =
            lazy
                choice
                    [
                        p_null
                        p_true
                        p_false
                        p_string
                        p_number
                        p_object
                        p_array
                    ]
                .>> p_ws
        fun ps -> p.Value ps


    let p_root          : Parser<JsonValue, unit>        = 
        choice
            [
                p_object
                p_array
            ]
        .>> p_ws

    let p_json  = p_ws >>. p_root .>> eof
    let p_jsons = p_ws >>. (many p_root) .>> eof

    let parse jsonText =
        match run p_json jsonText with
        | Success (json, _, pos) -> json
        | Failure (msg, pos, _)  -> failwith msg

    let parseMultiple jsonText =
        match run p_jsons jsonText with
        | Success (json, _, pos) -> json :> seq<JsonValue>
        | Failure (msg, pos, _)  -> failwith msg

