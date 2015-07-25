// --------------------------------------------------------------------------------------
// JSON type provider - methods that are called from the generated erased code
// --------------------------------------------------------------------------------------
namespace FSharp.Data.Runtime.BaseTypes

open System.ComponentModel
open System.IO
open FSharp.Data
open FSharp.Data.Runtime

#nowarn "10001"

/// [omit]
type IJsonDocument =
    abstract JsonValue : JsonValue
    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    abstract Path : unit -> string
    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    abstract CreateNew : value:JsonValue * pathIncrement:string -> IJsonDocument

/// Underlying representation of types generated by JsonProvider
[<StructuredFormatDisplay("{JsonValue}")>]
type JsonDocument = 

  private { /// [omit]
            Json : JsonValue
            /// [omit]
            Path : string }

  interface IJsonDocument with 
    member x.JsonValue = x.Json
    member x.Path() = x.Path
    member x.CreateNew(value, pathIncrement) = 
        JsonDocument.Create(value, x.Path + pathIncrement)

  /// The underlying JsonValue
  member x.JsonValue = x.Json

  /// [omit]
  [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
  [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
  override x.ToString() = x.JsonValue.ToString()

  /// [omit]
  [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
  [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
  static member Create(value, path) = 
    { Json = value
      Path = path } :> IJsonDocument

  /// [omit]
  [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
  [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
  static member Create(reader:TextReader, extendedErrorInfo) =
    use reader = reader
    let text = reader.ReadToEnd()
    let value = JsonValue.Parse(text, extendedErrorInfo)
    JsonDocument.Create(value, "")

  /// [omit]
  [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
  [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
  static member CreateList(reader:TextReader, extendedErrorInfo) =
    use reader = reader
    let text = reader.ReadToEnd()
    match JsonValue.ParseMultiple(text, extendedErrorInfo) |> Seq.toArray with
    | [| JsonValue.Array array |] -> array
    | array -> array
    |> Array.mapi (fun i value -> JsonDocument.Create(value, "[" + (string i) + "]"))

// --------------------------------------------------------------------------------------

namespace FSharp.Data.Runtime

open System
open System.Globalization
open FSharp.Data
open FSharp.Data.JsonExtensions
open FSharp.Data.Runtime
open FSharp.Data.Runtime.BaseTypes
open FSharp.Data.Runtime.StructuralTypes

/// [omit]
type JsonValueOptionAndPath = 
  { JsonOpt : JsonValue option
    Path : string }

/// Static helper methods called from the generated code for working with JSON
type JsonRuntime = 

  // --------------------------------------------------------------------------------------
  // json option -> type

  static member ConvertString(cultureStr, json) = 
    json |> Option.bind (JsonConversions.AsString (*useNoneForNullOrEmpty*)true (TextRuntime.GetCulture cultureStr))
  
  static member ConvertInteger(cultureStr, json) = 
    json |> Option.bind (JsonConversions.AsInteger (TextRuntime.GetCulture cultureStr))
  
  static member ConvertInteger64(cultureStr, json) = 
    json |> Option.bind (JsonConversions.AsInteger64 (TextRuntime.GetCulture cultureStr))

  static member ConvertDecimal(cultureStr, json) =
    json |> Option.bind (JsonConversions.AsDecimal (TextRuntime.GetCulture cultureStr))

  static member ConvertFloat(cultureStr, missingValuesStr, json) = 
    json |> Option.bind (JsonConversions.AsFloat (TextRuntime.GetMissingValues missingValuesStr) 
                                                 (*useNoneForMissingValues*)true
                                                 (TextRuntime.GetCulture cultureStr))

  static member ConvertBoolean(json) = 
    json |> Option.bind JsonConversions.AsBoolean

  static member ConvertDateTime(cultureStr, json) = 
    json |> Option.bind (JsonConversions.AsDateTime (TextRuntime.GetCulture cultureStr))

  static member ConvertGuid(json) = 
    json |> Option.bind JsonConversions.AsGuid

  /// Operation that extracts the value from an option and reports a meaningful error message when the value is not there
  /// If the originalValue is a scalar, for missing strings we return "", and for missing doubles we return NaN
  /// For other types an error is thrown
  static member GetNonOptionalValue<'T>(path:string, opt:option<'T>, originalValue) : 'T = 
    let getTypeName() = 
        let name = typeof<'T>.Name
        if name.StartsWith "int" 
        then "an " + name
        else "a " + name
    match opt, originalValue with 
    | Some value, _ -> value
    | None, Some ((JsonValue.Array _ | JsonValue.Record _) as x) -> failwithf "Expecting %s at '%s', got %s" (getTypeName()) path <| x.ToString(JsonSaveOptions.DisableFormatting)
    | None, _ when typeof<'T> = typeof<string> -> "" |> unbox
    | None, _ when typeof<'T> = typeof<float> -> Double.NaN |> unbox
    | None, None -> failwithf "'%s' is missing" path
    | None, Some x -> failwithf "Expecting %s at '%s', got %s" (getTypeName()) path <| x.ToString(JsonSaveOptions.DisableFormatting)

  /// Converts JSON array to array of target types
  static member ConvertArray<'T>(doc:IJsonDocument, mapping:Func<IJsonDocument,'T>) = 
    match doc.JsonValue with     
    | JsonValue.Array elements ->
        elements
        |> Array.filter (function JsonValue.Null -> false 
                                | JsonValue.String s when s |> TextConversions.AsString |> Option.isNone -> false
                                | _ -> true)
        |> Array.mapi (fun i value -> doc.CreateNew(value, "[" + (string i) + "]") |> mapping.Invoke)
    | JsonValue.Null -> [| |]
    | x -> failwithf "Expecting an array at '%s', got %s" (doc.Path()) <| x.ToString(JsonSaveOptions.DisableFormatting)

  /// Get optional json property
  static member TryGetPropertyUnpacked(doc:IJsonDocument, name) =
    doc.JsonValue.TryGetProperty(name)
    |> Option.bind (function JsonValue.Null -> None | x -> Some x) 

  /// Get optional json property and wrap it together with path
  static member TryGetPropertyUnpackedWithPath(doc:IJsonDocument, name) =
    { JsonOpt = JsonRuntime.TryGetPropertyUnpacked(doc, name)
      Path = doc.Path() + "/" + name }

  /// Get optional json property wrapped in json document
  static member TryGetPropertyPacked(doc:IJsonDocument, name) =
    JsonRuntime.TryGetPropertyUnpacked(doc, name)
    |> Option.map (fun value -> doc.CreateNew(value, "/" + name))

  /// Get json property and wrap in json document
  static member GetPropertyPacked(doc:IJsonDocument, name) =
    match JsonRuntime.TryGetPropertyPacked(doc, name) with
    | Some doc -> doc
    | None -> failwithf "Property '%s' not found at '%s': %s" name (doc.Path()) <| doc.JsonValue.ToString(JsonSaveOptions.DisableFormatting)

  /// Get json property and wrap in json document, and return null if not found
  static member GetPropertyPackedOrNull(doc:IJsonDocument, name) =
    match JsonRuntime.TryGetPropertyPacked(doc, name) with
    | Some doc -> doc
    | None -> doc.CreateNew(JsonValue.Null, "/" + name)

  /// Get optional json property and convert to a specified type
  static member ConvertOptionalProperty<'T>(doc:IJsonDocument, name, mapping:Func<IJsonDocument,'T>) =
    JsonRuntime.TryGetPropertyPacked(doc, name)
    |> Option.map mapping.Invoke

  static member private Matches cultureStr tag = 
    match tag with
    | InferedTypeTag.Number -> 
        let cultureInfo = TextRuntime.GetCulture cultureStr
        fun json -> (JsonConversions.AsDecimal cultureInfo json).IsSome ||
                    (JsonConversions.AsFloat [| |] (*useNoneForMissingValues*)true cultureInfo json).IsSome
    | InferedTypeTag.Boolean -> 
        JsonConversions.AsBoolean >> Option.isSome
    | InferedTypeTag.String -> 
        JsonConversions.AsString (*useNoneForNullOrEmpty*)true (TextRuntime.GetCulture cultureStr)
        >> Option.isSome
    | InferedTypeTag.DateTime -> 
        JsonConversions.AsDateTime (TextRuntime.GetCulture cultureStr)
        >> Option.isSome
    | InferedTypeTag.Guid -> 
        JsonConversions.AsGuid >> Option.isSome
    | InferedTypeTag.Collection -> 
        function JsonValue.Array _ -> true | _ -> false
    | InferedTypeTag.Record _ -> 
        function JsonValue.Record _ -> true | _ -> false
    | InferedTypeTag.Json -> 
        failwith "Json type not supported"
    | InferedTypeTag.Null -> 
        failwith "Null type not supported"
    | InferedTypeTag.Heterogeneous -> 
        failwith "Heterogeneous type not supported"

  /// Returns all array values that match the specified tag
  static member GetArrayChildrenByTypeTag<'T>(doc:IJsonDocument, cultureStr, tagCode, mapping:Func<IJsonDocument,'T>) =     
    match doc.JsonValue with
    | JsonValue.Array elements ->
        elements
        |> Array.filter (JsonRuntime.Matches cultureStr (InferedTypeTag.ParseCode tagCode))
        |> Array.mapi (fun i value -> doc.CreateNew(value, "[" + (string i) + "]") |> mapping.Invoke)
    | JsonValue.Null -> [| |]
    | x -> failwithf "Expecting an array at '%s', got %s" (doc.Path()) <| x.ToString(JsonSaveOptions.DisableFormatting)

  /// Returns single or no value from an array matching the specified tag
  static member TryGetArrayChildByTypeTag<'T>(doc, cultureStr, tagCode, mapping:Func<IJsonDocument,'T>) = 
    match JsonRuntime.GetArrayChildrenByTypeTag(doc, cultureStr, tagCode, mapping) with
    | [| child |] -> Some child
    | [| |] -> None
    | _ -> failwithf "Expecting an array with single or no elements at '%s', got %s" (doc.Path()) <| doc.JsonValue.ToString(JsonSaveOptions.DisableFormatting)

  /// Returns a single array children that matches the specified tag
  static member GetArrayChildByTypeTag(doc, cultureStr, tagCode) = 
    match JsonRuntime.GetArrayChildrenByTypeTag(doc, cultureStr, tagCode, Func<_,_>(id)) with
    | [| child |] -> child
    | _ -> failwithf "Expecting an array with single element at '%s', got %s" (doc.Path()) <| doc.JsonValue.ToString(JsonSaveOptions.DisableFormatting)

  /// Returns a single or no value by tag type
  static member TryGetValueByTypeTag<'T>(doc:IJsonDocument, cultureStr, tagCode, mapping:Func<IJsonDocument,'T>) = 
    if JsonRuntime.Matches cultureStr (InferedTypeTag.ParseCode tagCode) doc.JsonValue
    then Some (mapping.Invoke doc)
    else None

  static member private ToJsonValue (cultureInfo:CultureInfo) (value:obj) = 
    let inline optionToJson f = function None -> JsonValue.Null | Some v -> f v
    match value with
    | null -> JsonValue.Null
    | :? Array                   as v -> JsonValue.Array [| for elem in v -> JsonRuntime.ToJsonValue cultureInfo elem |]

    | :? string                  as v -> JsonValue.String v
    | :? DateTime                as v -> v.ToString("O", cultureInfo) |> JsonValue.String
    | :? int                     as v -> JsonValue.Number(decimal v)
    | :? int64                   as v -> JsonValue.Number(decimal v)
    | :? float                   as v -> JsonValue.Number(decimal v)
    | :? decimal                 as v -> JsonValue.Number v
    | :? bool                    as v -> JsonValue.Boolean v
    | :? Guid                    as v -> v.ToString() |> JsonValue.String
    | :? IJsonDocument           as v -> v.JsonValue
    | :? JsonValue               as v -> v

    | :? option<string>          as v -> optionToJson JsonValue.String v
    | :? option<DateTime>        as v -> optionToJson (fun (dt:DateTime) -> dt.ToString(cultureInfo) |> JsonValue.String) v
    | :? option<int>             as v -> optionToJson (decimal >> JsonValue.Number) v
    | :? option<int64>           as v -> optionToJson (decimal >> JsonValue.Number) v
    | :? option<float>           as v -> optionToJson (decimal >> JsonValue.Number) v
    | :? option<decimal>         as v -> optionToJson JsonValue.Number v
    | :? option<bool>            as v -> optionToJson JsonValue.Boolean v
    | :? option<Guid>            as v -> optionToJson (fun (g:Guid) -> g.ToString() |> JsonValue.String) v
    | :? option<IJsonDocument>   as v -> optionToJson (fun (v:IJsonDocument) -> v.JsonValue) v
    | :? option<JsonValue>       as v -> optionToJson id v

    | _ -> failwithf "Can't create JsonValue from %A" value

  /// Creates a scalar JsonValue and wraps it in a json document
  static member CreateValue(value:obj, cultureStr) = 
    let cultureInfo = TextRuntime.GetCulture cultureStr
    let json = JsonRuntime.ToJsonValue cultureInfo value
    JsonDocument.Create(json, "")

  // Creates a JsonValue.Record and wraps it in a json document
  static member CreateRecord(properties, cultureStr) =
    let cultureInfo = TextRuntime.GetCulture cultureStr
    let json = 
      properties 
      |> Array.map (fun (k, v:obj) -> k, JsonRuntime.ToJsonValue cultureInfo v)
      |> JsonValue.Record
    JsonDocument.Create(json, "")

  /// Creates a scalar JsonValue.Array and wraps it in a json document
  static member CreateArray(elements:obj[], cultureStr) =
    let cultureInfo = TextRuntime.GetCulture cultureStr
    let json = 
      elements 
      |> Array.collect (JsonRuntime.ToJsonValue cultureInfo
                        >>
                        function JsonValue.Array elements -> elements | JsonValue.Null -> [| |] | element -> [| element |])
      |> JsonValue.Array
    JsonDocument.Create(json, "")
