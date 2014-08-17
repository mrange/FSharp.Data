﻿namespace ProviderImplementation

open System
open System.IO
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProviderHelpers
open FSharp.Data
open FSharp.Data.Runtime
open FSharp.Data.Runtime.StructuralTypes

// ----------------------------------------------------------------------------------------------

#nowarn "10001"

[<TypeProvider>]
type public JsonProvider(cfg:TypeProviderConfig) as this =
  inherit DisposableTypeProviderForNamespaces()

  // Generate namespace and type 'FSharp.Data.JsonProvider'
  let asm, version, replacer = AssemblyResolver.init cfg
  let ns = "FSharp.Data"
  let jsonProvTy = ProvidedTypeDefinition(asm, ns, "JsonProvider", Some typeof<obj>)

  let buildTypes (typeName:string) (args:obj[]) =

    // Generate the required type
    let tpType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)

    let sample = args.[0] :?> string
    let sampleIsList = args.[1] :?> bool
    let rootName = args.[2] :?> string
    let rootName = if String.IsNullOrWhiteSpace rootName then "Root" else NameUtils.singularize rootName
    let cultureStr = args.[3] :?> string
    let encodingStr = args.[4] :?> string
    let resolutionFolder = args.[5] :?> string
    let resource = args.[6] :?> string
    let noTypeInference = args.[7] :?> bool

    let cultureInfo = TextRuntime.GetCulture cultureStr
    let parseSingle _ value = JsonValue.Parse(value, cultureInfo)
    let parseList _ value = 
        JsonDocument.CreateList(new StringReader(value), cultureStr)
        |> Array.map (fun doc -> doc.JsonValue)
    
    let getSpecFromSamples samples = 

      let inferedType = using (IO.logTime "Inference" sample) <| fun _ ->
        [ for sampleJson in samples -> JsonInference.inferType noTypeInference cultureInfo "" sampleJson ]
        |> Seq.fold (StructuralInference.subtypeInfered (*allowEmptyValues*)false) StructuralTypes.Top

      using (IO.logTime "TypeGeneration" sample) <| fun _ ->

      let ctx = JsonGenerationContext.Create(cultureStr, tpType, replacer)
      let result = JsonTypeBuilder.generateJsonType ctx (*canPassAllConversionCallingTypes*)false (*optionalityHandledByParent*)false rootName inferedType

      { GeneratedType = tpType
        RepresentationType = result.ConvertedType
        CreateFromTextReader = fun reader -> 
          result.GetConverter ctx <@@ JsonDocument.Create(%reader, cultureStr) @@>
        CreateFromTextReaderForSampleList = fun reader -> 
          result.GetConverter ctx <@@ JsonDocument.CreateList(%reader, cultureStr) @@> }

    generateType "JSON" sample sampleIsList parseSingle parseList getSpecFromSamples 
                 version this cfg replacer encodingStr resolutionFolder resource typeName

  // Add static parameter that specifies the API we want to get (compile-time) 
  let parameters = 
    [ ProvidedStaticParameter("Sample", typeof<string>)
      ProvidedStaticParameter("SampleIsList", typeof<bool>, parameterDefaultValue = false) 
      ProvidedStaticParameter("RootName", typeof<string>, parameterDefaultValue = "Root") 
      ProvidedStaticParameter("Culture", typeof<string>, parameterDefaultValue = "") 
      ProvidedStaticParameter("Encoding", typeof<string>, parameterDefaultValue = "") 
      ProvidedStaticParameter("ResolutionFolder", typeof<string>, parameterDefaultValue = "")
      ProvidedStaticParameter("EmbeddedResource", typeof<string>, parameterDefaultValue = "")
      ProvidedStaticParameter("NoTypeInference", typeof<bool>, parameterDefaultValue = false) ]

  let helpText = 
    """<summary>Typed representation of a JSON document.</summary>
       <param name='Sample'>Location of a JSON sample file or a string containing a sample JSON document.</param>
       <param name='SampleIsList'>If true, sample should be a list of individual samples for the inference.</param>
       <param name='RootName'>The name to be used to the root type. Defaults to `Root`.</param>
       <param name='Culture'>The culture used for parsing numbers and dates. Defaults to the invariant culture.</param>
       <param name='Encoding'>The encoding used to read the sample. You can specify either the character set name or the codepage number. Defaults to UTF8 for files, and to ISO-8859-1 the for HTTP requests, unless `charset` is specified in the `Content-Type` response header.</param>
       <param name='ResolutionFolder'>A directory that is used when resolving relative file references (at design time and in hosted execution).</param>
       <param name='EmbeddedResource'>When specified, the type provider first attempts to load the sample from the specified resource 
          (e.g. 'MyCompany.MyAssembly, resource_name.json'). This is useful when exposing types generated by the type provider.</param>
       <param name='NoTypeInference'>If true, turns off type inference.</param>"""

  do jsonProvTy.AddXmlDoc helpText
  do jsonProvTy.DefineStaticParameters(parameters, buildTypes)

  // Register the main type with F# compiler
  do this.AddNamespace(ns, [ jsonProvTy ])
