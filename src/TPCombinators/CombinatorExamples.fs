module CombinatorExamples

open FSharp.ProvidedTypes.GeneralCombinators
open FSharp.ProvidedTypes.CloneCombinatorOverSimplifiedAlgebra
open FSharp.ProvidedTypes.RegexHideCombinator
open FSharp.ProvidedTypes.ChainCombinator
open FSharp.ProvidedTypes.SimplifiedAlgebra
open FSharp.ProvidedTypes.AddStaticCombinator
open FSharp.ProvidedTypes.CachingCombinator
open Microsoft.FSharp.Core.CompilerServices
open FSharp.Data
open FSharp.Management

let CsvProvider config = 
    let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
    new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location)) |> Simplify

//let FreebaseProvider config = 
//    let FreebaseAssembly = typeof<FSharp.Data.CsvFile>.Assembly
//    new ProviderImplementation.FreebaseTypeProvider(ConfigForOtherTypeProvider(config, FreebaseAssembly.Location)) |> Simplify
//
//let XmlProvider config =
//    let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
//    new ProviderImplementation.XmlProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location)) |> Simplify
//
//let JsonHideProvider config = 
//    let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
//    new ProviderImplementation.JsonProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location)) |> Simplify

//let WorldBank config =
//    let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
//    new ProviderImplementation.WorldBankProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location)) |> Simplify

let FileSystem config = 
    let FileSysAssembly = typeof<FSharp.Management.Registry>.Assembly
    new FSharp.Management.NamespaceProvider.FileSystemProvider(ConfigForOtherTypeProvider(config, FileSysAssembly.Location)) |> Simplify
//
//let CloneExample config = 
//    CsvProvider config 
//    |> Clone("FSharp.Data", "MySpace")
//
//let HideCloneExample config = 
//
//    CloneExample(config) 
//    |> HideProperties("D.te")
//    |> Clone("MySpace", "HideSpace") 
//
//let ShowExample config = 
//
//    CloneExample(config) 
//    |> ShowProperties("D.te")
//    |> Clone("MySpace", "ShowSpace") 
//
//let FreebaseHide config = 
//    FreebaseProvider config 
//    |> HideProperties("Argon")
//    |> Clone("FSharp.Data", "Free")
//
//
//let XmlHide config = 
//    XmlProvider config 
//    |> Clone("FSharp.Data", "Corp")
//
//let JsonHide config =
//    
//    //Hide any geometry properties from the GeoJSON file.
//    JsonHideProvider config
//    |> HideProperties(".*[gG]eometry.*")
//    |> Clone("FSharp.Data", "Geo")
//
//let WorldBankHide config = 
//
//    //Hide all indicators except those with CO2 data
//    WorldBank config
//    |> ShowPropertiesInType(".*CO2.*", ".*[iI]ndicator.*")
//    |> Clone("FSharp.Data", "BankSpace")

let CsvAddStatic config = 

    CsvProvider config
    |> AddStaticParam("Regex", typeof<string>, Some("default value" :> obj))
    |> AddStaticParam("Show", typeof<bool>, Some(true :> obj))
    |> HideStatic
    |> Clone("FSharp.Data", "StaticSpace")

let FileSysCache config =
    
    FileSystem config
    |> Cache
    |> Clone("FSharp.Management", "CachedFileSys")

//[<TypeProvider>]
//type CloneProvider(config) = inherit TypeProviderExpression(CloneExample(config) |> Desimplify)
//
//[<TypeProvider>]
//type HideCloneProvider(config) = inherit TypeProviderExpression(HideCloneExample(config) |> Desimplify)
//
//[<TypeProvider>]
//type ShowProvider(config) = inherit TypeProviderExpression(ShowExample(config) |> Desimplify)
//
//[<TypeProvider>]
//type HideFreebaseProvider(config) = inherit TypeProviderExpression(FreebaseHide(config) |> Desimplify)
//
//[<TypeProvider>]
//type XmlHideProvider(config) = inherit TypeProviderExpression(XmlHide(config) |> Desimplify)
//
//[<TypeProvider>]
//type GeoJsonHide(config) = inherit TypeProviderExpression(JsonHide(config) |> Desimplify)
//
//[<TypeProvider>]
//type WBHide(config) = inherit TypeProviderExpression(WorldBankHide(config) |> Desimplify)
//
[<TypeProvider>]
type CsvStatic(config) = inherit TypeProviderExpression(CsvAddStatic(config) |> Desimplify)

[<TypeProvider>]
type FileSysCached(config) = inherit TypeProviderExpression(FileSysCache(config) |> Desimplify)

// CHALLENGE: Make a general purpose "Hide" type provider transformer
//[<TypeProvider>]
//type HideProvider(config) = 
//   ...


[<assembly:TypeProviderAssembly>] 
do()
