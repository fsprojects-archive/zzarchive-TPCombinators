module ChainExamples

open System.Reflection
open FSharp.ProvidedTypes.CloneCombinatorOverSimplifiedAlgebra
open FSharp.ProvidedTypes.ChainCombinator
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
    
let FreebaseProvider config =
    let FSharpDataAssembly = typeof<FSharp.Data.Runtime.Freebase.FreebaseObject>.Assembly
    new ProviderImplementation.FreebaseTypeProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))
    
let DbPediaProvider config =
    let FSharpDataDbPediaAssembly = typeof<FSharp.Data.DbPedia>.Assembly
    new FSharp.Data.DbPediaTypeProvider.DbPediaTypeProvider(ConfigForOtherTypeProvider(config, FSharpDataDbPediaAssembly.Location))

// Chain DbPedia --> Freebase
let DbPediaToFreebase config = 
    let contextCreator (staticArgumentValues:obj[], inpApplied : ISimpleTypeDefinition) =
        let dataContextMethodOpt =
            inpApplied.Methods
            |> Array.tryFind (fun m -> m.IsStatic && (m.Name = "GetSample" || m.Name = "GetDataContext"))

        dataContextMethodOpt |> Option.map (fun dataContextMethod ->
            let expr = dataContextMethod.GetImplementation [| |]
            let dataContextObj = Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation(expr)
            let dataContext = dataContextObj :?> DbPediaAccess.DbPediaDataContextBase
            dataContext.Connection)

    let resolver (connObjOpt:DbPediaAccess.DbPediaConnection option,  inp: ISimpleProperty) = 

        // If no GetDataContext/LoadSample, then no transform
        match connObjOpt with 
        | None -> None
        | Some connObj ->

        // If property is not string, then no transform
        if not (inp.PropertyType.IsPrimitive typeof<string>) then None else

        let isTarget = (inp.Name = "abstract") // staticValueOfProp.StartsWith("http://freebase.com/")
        if not isTarget then None else

        let staticValueOfProp = 
            let thisv = Var("this", typeof<DbPediaAccess.DbPediaIndividualBase>)
            let q =    inp.GetMethod.Value.GetImplementation [| thisv |]
            let q2 = 
                match q with 
                // Translate the implementations, which only depened on the connection. 
                | Patterns.Let(_var0,_thisVar, Patterns.Call(Some _connExpr, m, args)) -> Expr.Call(Expr.Value connObj, m, args)
                //| Call(None, m, args) -> Expr.Call(m, args)
                | _ -> failwith "unexpected implementation of target method"
                
            let res = Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation(q2)
            let res2 = res :?> string
            res2

        let ty, getImpl = 

            let tp = FreebaseProvider config :> ITypeProvider
            let n1 = tp.GetNamespaces() |> Array.find (fun n -> n.NamespaceName = "FSharp.Data") 
            let td = n1.GetTypes() |> Array.find (fun n -> n.Name = "FreebaseData") 
            let bindingFlags = BindingFlags.DeclaredOnly ||| BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.Public
            let getSampleMeth = td.GetMethods(bindingFlags) |> Array.find (fun n -> n.Name = "GetDataContext")
            let getImpl = (fun _ -> tp.GetInvokerExpression(getSampleMeth, [| |])) 
            TyApp (OtherTyDef(getSampleMeth.ReturnType), [| |]), getImpl


        { new ISimpleProperty with 
            override __.Name              = inp.Name
            override __.IsStatic          = inp.IsStatic  
            override __.PropertyType          = ty 
            override __.IndexParameters        = [| |]
            override __.CustomAttributes = Seq.empty
            override __.GetMethod = Some { new ISimpleAssociatedMethod with member __.GetImplementation(parameters) = getImpl(parameters) }
            override __.SetMethod = None 
        } |> Some

    let freebaseResolver = { defaultChainResolver with ContextCreator = contextCreator; PropertyResolver = resolver }
        
    DbPediaProvider config
    |> Simplify
    |> Chain freebaseResolver 
    |> Clone ("FSharp.Data", "DbPediaToFreebase")
    |> Desimplify

[<TypeProvider>]
type DbPediaFreebaseProvider(config) = inherit TypeProviderExpression(DbPediaToFreebase(config))

// Chain FileSystem --> CSV
let FileSystemToCsv config =

    let resolver (_, inp: ISimpleLiteralField) = 
        if not (inp.Name.EndsWith(".csv")) then None else

        let rty, csvSample = 
            let tp = CsvProvider config :> ITypeProvider
            let n1 = tp.GetNamespaces() |> Array.find (fun n -> n.NamespaceName = "FSharp.Data") 
            let td = n1.GetTypes() |> Array.find (fun n -> n.Name = "CsvProvider") 
            let sparams = [| for sp in tp.GetStaticParameters(td) do 
                                    if sp.Name = "Sample" then 
                                        yield inp.LiteralValue
                                    else 
                                        assert sp.IsOptional 
                                        yield sp.RawDefaultValue |]
            let csvPath = (inp.LiteralValue :?> string).Replace(@"\", @"\\")
            let csvFile = tp.ApplyStaticArguments(td, [| "FSharp"; "Data"; "CsvProvider,Sample=\"" + csvPath + "\""|], sparams)
            let bindingFlags = BindingFlags.DeclaredOnly ||| BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.Public
            let getSampleMeth = csvFile.GetMethods(bindingFlags) |> Array.find (fun n -> n.Name = "GetSample")
            let q = tp.GetInvokerExpression(getSampleMeth, [| |]) // GetSample expects no arguments
            let csvSample = Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation(q)
            TyApp(OtherTyDef(getSampleMeth.ReturnType), [||]), csvSample

        { new ISimpleLiteralField with
            override __.Name                = inp.Name
            override __.FieldType           = rty
            override __.CustomAttributes    = Seq.empty
            override __.LiteralValue        = csvSample
        } |> Some

    let csvResolver = { defaultChainResolver with FieldResolver = resolver }
    
    FileSystemProvider config
    |> Simplify
    |> Chain csvResolver
    |> Clone ("FSharp.Management", "FileSystemToCsv")
    |> Desimplify

[<TypeProvider>]
type FileSystemCsvProvider(config) = inherit TypeProviderExpression(FileSystemToCsv(config))

[<assembly:TypeProviderAssembly>] 
do()
