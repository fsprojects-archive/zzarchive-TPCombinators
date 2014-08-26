module ExtendExamples

open System.Reflection
open FSharp.ProvidedTypes.CloneCombinatorOverSimplifiedAlgebra
open FSharp.ProvidedTypes.ExtendCombinator
open FSharp.ProvidedTypes.SimplifiedAlgebra
open FSharp.ProvidedTypes.GeneralCombinators
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open FSharp.Management.NamespaceProvider

// instantiate type providers used in chaining
let FileSystemProvider config =
    let FSharpManagementAssembly = typeof<FSharp.Management.Registry>.Assembly
    new FileSystemProvider(ConfigForOtherTypeProvider(config, FSharpManagementAssembly.Location))

let CsvProvider config =
    let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
    new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))
(*
// Chain FileSystem --> CSV
let FileSystemToCsv config =

    let resolver (_, inp: ISimpleTypeDefinition) = 
        [| for field in inp.Fields do
            if not (field.Name.EndsWith(".csv")) then None else

            let rty, csvSample = 
                let tp = CsvProvider config :> ITypeProvider
                let n1 = tp.GetNamespaces() |> Array.find (fun n -> n.NamespaceName = "FSharp.Data") 
                let td = n1.GetTypes() |> Array.find (fun n -> n.Name = "CsvProvider") 
                let sparams = [| for sp in tp.GetStaticParameters(td) do 
                                        if sp.Name = "Sample" then 
                                            yield field.LiteralValue
                                        else 
                                            assert sp.IsOptional 
                                            yield sp.RawDefaultValue |]
                let csvPath = (field.LiteralValue :?> string).Replace(@"\", @"\\")
                let csvFile = tp.ApplyStaticArguments(td, [| "FSharp"; "Data"; "CsvProvider,Sample=\"" + csvPath + "\""|], sparams)
                let bindingFlags = BindingFlags.DeclaredOnly ||| BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.Public
                let getSampleMeth = csvFile.GetMethods(bindingFlags) |> Array.find (fun n -> n.Name = "GetSample")
                let q = tp.GetInvokerExpression(getSampleMeth, [| |]) // GetSample expects no arguments
                let csvSample = Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation(q)
                TyApp(OtherTyDef(getSampleMeth.ReturnType), [||]), csvSample

            { new ISimpleProperty with
                override __.Name                = field.Name + " contents"
                override __.IsStatic            = true
                override __.PropertyType        = rty
                override __.CustomAttributes    = Seq.empty
                override __.IndexParameters     = [| |]
                override __.GetMethod           = Some { new ISimpleAssociatedMethod with member __.GetImplementation(parameters) = getImpl(parameters) }
            } |> Some

    let csvResolver = { defaultExtendResolver with MethodResolver = resolver }
    
    FileSystemProvider config
    |> Simplify
    |> Extend csvResolver
    |> Clone ("FSharp.Management", "FileSystemToCsv")
    |> Desimplify

[<TypeProvider>]
type FileSystemCsvProvider(config) = inherit TypeProviderExpression(FileSystemToCsv(config))

[<assembly:TypeProviderAssembly>] 
do()

*)