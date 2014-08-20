module CombinatorExamples

open FSharp.ProvidedTypes.GeneralCombinators
open FSharp.ProvidedTypes.CloneCombinatorOverSimplifiedAlgebra
open FSharp.ProvidedTypes.RegexHideCombinator
open FSharp.ProvidedTypes.SimplifiedAlgebra
open FSharp.ProvidedTypes.AddStaticCombinator
open FSharp.ProvidedTypes.CachingCombinator

open Microsoft.FSharp.Core.CompilerServices
open FSharp.Data
open FSharp.Management
open Hive.HiveRuntime

let HiveProvider config =
    let HiveAssembly = typeof<Hive.HiveRuntime.HiveDataContext>.Assembly
    new Hive.HiveRuntime.HiveTypeProviderImplementation(ConfigForOtherTypeProvider(config, HiveAssembly.Location)) |> Simplify

let CsvProvider config = 
    let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
    new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location)) |> Simplify

let FreebaseProvider config = 
    let FreebaseAssembly = typeof<FSharp.Data.CsvFile>.Assembly
    new ProviderImplementation.FreebaseTypeProvider(ConfigForOtherTypeProvider(config, FreebaseAssembly.Location)) |> Simplify

let XmlProvider config =
    let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
    new ProviderImplementation.XmlProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location)) |> Simplify

let JsonHideProvider config = 
    let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
    new ProviderImplementation.JsonProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location)) |> Simplify

let WorldBank config =
    let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
    new ProviderImplementation.WorldBankProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location)) |> Simplify

let FileSystem config = 
    let FileSysAssembly = typeof<FSharp.Management.Registry>.Assembly
    new FSharp.Management.NamespaceProvider.FileSystemProvider(ConfigForOtherTypeProvider(config, FileSysAssembly.Location)) |> Simplify

//Compose the AddStaticCombinator and the RegexHideCombinator in order to allow the hiding configuration to be passed
//as static parameters in the script where the transformed TP is used, rather than having to be defined in the compiled assembly.
let HideStaticCombinator (tp: ISimpleTypeProvider) =
    tp
    |> AddStaticParam("Regex", typeof<string>, Some("" :> obj))
    |> AddStaticParam("Show", typeof<bool>, Some(true :> obj))
    |> AddStaticParam("Restriction", typeof<string>, Some("" :> obj))
    |> HideStatic

// CHALLENGE: Make a general purpose "Hide" type provider transformer
//[<TypeProvider>]
//type HideProvider(config) = 
//   ...

//Clone the CSV provider into a new namespace
let CsvCloneExample config = 
    CsvProvider config 
    |> Clone("FSharp.Data", "MySpace")


let CsvHideExample config = 
    CsvCloneExample(config) 
    |> HideProperties("D.te")
    |> Clone("MySpace", "HideSpace") 

let CsvShowExample config = 
    CsvCloneExample(config) 
    |> ShowProperties("D.te")
    |> Clone("MySpace", "ShowSpace") 

let FreebaseHide config = 
    FreebaseProvider config 
    |> HideProperties("Argon")
    |> Clone("FSharp.Data", "Free")

//Hide the geometry properties from a GeoJSON file.
let JsonHide config =
    JsonHideProvider config
    |> HideProperties(".*[gG]eometry.*")
    |> Clone("FSharp.Data", "Geo")

//Hide all WB indicators except those with CO2 data
let WorldBankHide config = 
    WorldBank config
    |> ShowPropertiesInType(".*CO2.*", ".*[iI]ndicator.*")
    |> Clone("FSharp.Data", "BankSpace")

//Use the RegexHide combinator with static parameters
let HiveHide config =
    HiveProvider config
    |> HideStaticCombinator
    |> Clone("Hive", "HideHive")

//Add a static parameter to the CSV provider (unused)
let CsvAddStatic config = 
    CsvProvider config
    |> HideStaticCombinator
    |> Clone("FSharp.Data", "StaticSpace")

//Use metadata caching with FSharp.Management
let FileSysCache config =
    FileSystem config
    |> Cache
    |> Clone("FSharp.Management", "CachedFileSys")

[<TypeProvider>]
type CsvCloneProvider(config) = inherit TypeProviderExpression(CsvCloneExample(config) |> Desimplify)

[<TypeProvider>]
type CsvHideProvider(config) = inherit TypeProviderExpression(CsvHideExample(config) |> Desimplify)

[<TypeProvider>]
type CsvShowProvider(config) = inherit TypeProviderExpression(CsvShowExample(config) |> Desimplify)

[<TypeProvider>]
type HideFreebaseProvider(config) = inherit TypeProviderExpression(FreebaseHide(config) |> Desimplify)

[<TypeProvider>]
type GeoJsonHide(config) = inherit TypeProviderExpression(JsonHide(config) |> Desimplify)

[<TypeProvider>]
type WBHide(config) = inherit TypeProviderExpression(WorldBankHide(config) |> Desimplify)

[<TypeProvider>]
type CsvStatic(config) = inherit TypeProviderExpression(CsvAddStatic(config) |> Desimplify)

[<TypeProvider>]
type FileSysCached(config) = inherit TypeProviderExpression(FileSysCache(config) |> Desimplify)

[<TypeProvider>]
type HideHive(config) = inherit TypeProviderExpression(HiveHide(config) |> Desimplify)

[<assembly:TypeProviderAssembly>] 
do()
