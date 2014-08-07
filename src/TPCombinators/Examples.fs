module Examples


open FSharp.ProvidedTypes.GeneralCombinators
open Microsoft.FSharp.Core.CompilerServices

//let ctxt = FSharp.Data.FreebaseData.GetDataContext()
(*

type FB = FSharp.Data.FreebaseDataProvider<Key="AIzaSyBkXwW0htekE3hiXjBselWN8pCIOAzeGRI" >

let ctxt = FB.GetDataContext()
ctxt.``Science and Technology``.Biology.Genes.Individuals.``2,3-bisphosphoglycerate mutase``.``NCBI ID``


//let ctxt = FSharp.Data.FreebaseData.GetDataContext()

ctxt.``Science and Technology``.Biology.Genes.Individuals.``2,3-bisphosphoglycerate mutase``.``NCBI ID``
ctxt.``Science and Technology``.Biology.Genes.Individuals.``ATP synthase, H+ transporting, mitochondrial F0 complex, subunit B1``.``NCBI ID``
ctxt.``Science and Technology``.Chemistry.``Chemical Compounds``.Individuals.Carbendazim.``PubChem ID``
ctxt.``Science and Technology``.Medicine.Drugs.Individuals.``2-Aminoisobutyric acid``.``PubChem ID``

ctxt.``Arts and Entertainment``.Film.Films.Individuals.``A Star Is Born``.``Netflix ID``
ctxt.``Arts and Entertainment``.Film.Films.Individuals.``A Star Is Born``.``Rotten Tomatoes ID``
let m = ctxt.``Arts and Entertainment``.Film.Films |> Seq.head

m.``Apple movie trailer ID``
m.``Fandango ID``
m.``Metacritic film ID``
m.``NY Times ID``
m.``Netflix ID``
m.``Trailer Addict ID``

let r = ctxt.``Arts and Entertainment``.Music.``Musical Recordings`` |> Seq.head
type FS = FSharp.Management.FileSystem //  <".">

type RP = FSharp.Management.RelativePath<"data">

// Use as data
RP.``AirQuality.csv``.Rows

// Use as schema + sample
RP.``AirQuality.csv``.GetSample().Rows
RP.``AirQuality.csv``.Load("AirQuality.csv").Rows
RP.``AirQuality.csv``.Load("AirQuality2.csv").Rows

*)

// In this example, we use a provided type space where a copy+rename has happened to namespaces:
//
//     FSharp.Data --> MySpace
//
// for all namespaces and types provided by the FSharp.Data CsvProvider.
let CsvProvider config = 
    let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
    new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))



(*
module SimplifyUnsimplify =
    
    open FSharp.ProvidedTypes.SimplifiedAlgebra

    let Example1 config = 

        Simplify (CsvProvider config)

[<TypeProvider>]
type SimUnsimExampleProvider(config) = inherit TypeProviderExpression(SimplifyUnsimplify.Example1(config) |> FSharp.ProvidedTypes.SimplifiedAlgebra.Desimplify)
*)


module Unsimplified = 

    open FSharp.ProvidedTypes.CloneCombinator

    let Example1 config = 

        Clone("FSharp.Data", "UnsimplifiedExample1", CsvProvider config)

    let Example2 config = 

        Clone("UnsimplifiedExample1", "UnsimplifiedExample2", Example1 config)


module Simplified = 

    open FSharp.ProvidedTypes.SimplifiedAlgebra
    open FSharp.ProvidedTypes.CloneCombinatorOverSimplifiedAlgebra

    let Example1 config = Clone("FSharp.Data", "SimplifiedExample1", Simplify (CsvProvider config))

    let Example2 config = Clone("SimplifiedExample1", "SimplifiedExample2", Example1 config) 


[<TypeProvider>]
type UnsimplifiedExample1Provider(config) = inherit TypeProviderExpression(Unsimplified.Example1(config))

[<TypeProvider>]
type UnsimplifiedExample2Provider(config) = inherit TypeProviderExpression(Unsimplified.Example2(config))


[<TypeProvider>]
type SimplifiedExample1Provider(config) = inherit TypeProviderExpression(Simplified.Example1(config) |> FSharp.ProvidedTypes.SimplifiedAlgebra.Desimplify)

[<TypeProvider>]
type SimplifiedExample2Provider(config) = inherit TypeProviderExpression(Simplified.Example2(config) |> FSharp.ProvidedTypes.SimplifiedAlgebra.Desimplify)

[<assembly:TypeProviderAssembly>] 
do()