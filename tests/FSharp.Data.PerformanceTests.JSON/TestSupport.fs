module TestSupport
    open FSharp.Data
    open System

    // Checks if two floats are "near" eachother
    let isNear (l : float) (r : float) = 
        if r < 16. * Double.Epsilon then
            l < 16. * Double.Epsilon
        else
            let d = 1E-10
            let ratio = l / r
            ratio < (1. + d) && ratio > (1. - d)

    // Checks if two JsonValue are equal.
    // The built-in structural equal is too strict on equalness on double to work
    // Rant coming:
    // The reason ultimately is that double is a binary number and we humans uses 
    // decimal notation. This means that strings like: "0.20" can't be exactly 
    // represented as a double. Decimal doesn't have that problems but as JSON 
    // is often used as interop with JavaScript (it stands for JavaScript Object 
    // Notation after all) and JavaScript uses double as numbers (sadly) a JSON 
    // parser should use double as well.
    // 
    // This is also the primary reason decimals should always be used for 
    // financial amounts, the increased precision is a bonus.
    //
    // This was well understood in the 1970s and still JavaScript in the 2000s 
    // picked double as number representation.
    let rec isEqual (l : JsonValue) (r : JsonValue) =
        match l,r with
        | JsonValue.Null        , JsonValue.Null                                    -> true
        | JsonValue.Boolean lb  , JsonValue.Boolean rb when lb = rb                 -> true
        | JsonValue.String  ls  , JsonValue.String  rs when ls = rs                 -> true
        | JsonValue.Float   lf  , JsonValue.Float   rf when isNear lf rf            -> true // The reasons we do a custom isEqual
        | JsonValue.Float   lf  , JsonValue.Number  rd when isNear lf (float rd)    -> true // The reasons we do a custom isEqual
        | JsonValue.Number  ld  , JsonValue.Float   rf when isNear (float ld) rf    -> true // The reasons we do a custom isEqual
        // As .Number is decimal is decimal no isNear is needed as parsing should be exact for these
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

    let isEquals (l : JsonValue list) (r : JsonValue list) =
        if l.Length <> r.Length then
            false
        else
            let z = l |> List.zip r 
            let result = z |> List.forall (fun (ll,rr) -> isEqual ll rr)
            result
