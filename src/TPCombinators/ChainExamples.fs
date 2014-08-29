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
    new FileSystemProvider(ConfigForOtherTypeProvider(config, FSharpManagementAssembly.Location)) :> ITypeProvider

let CsvProvider config =
    let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
    new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location)) :> ITypeProvider

let XmlProvider config =
    let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
    new ProviderImplementation.XmlProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location)) :> ITypeProvider

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

    let resolver (connObjOpt:DbPediaAccess.DbPediaConnection option,  memberInp: ISimpleMember) = 

        // If no GetDataContext/LoadSample, then no transform
        match connObjOpt with 
        | None -> None
        | Some connObj ->

        match memberInp with
        | Property inp ->
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
            } |> Property |> Some

        | _ -> None     // if member is not a property then no transform

    let freebaseResolver = { defaultChainResolver with ContextCreator = contextCreator; MemberResolver = resolver }
        
    DbPediaProvider config
    |> Simplify
    |> Chain freebaseResolver 
    |> Clone ("FSharp.Data", "DbPediaToFreebase")
    |> Desimplify

[<TypeProvider>]
type DbPediaFreebaseProvider(config) = inherit TypeProviderExpression(DbPediaToFreebase(config))

// Chain FileSystem --> CSV & XML
let FileSystemToCsvAndXml config =

    let resolver (_, memberInp: ISimpleMember) = 
        match memberInp with
        | Field inp ->
            if not (inp.Name.EndsWith(".csv")) && not (inp.Name.EndsWith(".xml")) then None else

            let tp, providerName = 
                if inp.Name.EndsWith(".csv") then 
                    CsvProvider config, "CsvProvider"
                else 
                    XmlProvider config, "XmlProvider"

            let rty, getImpl = 
                let n1 = tp.GetNamespaces() |> Array.find (fun n -> n.NamespaceName = "FSharp.Data") 
                let td = n1.GetTypes() |> Array.find (fun n -> n.Name = providerName) 
                let sparams = [| for sp in tp.GetStaticParameters(td) do 
                                        if sp.Name = "Sample" then 
                                            yield inp.LiteralValue
                                        else 
                                            assert sp.IsOptional 
                                            yield sp.RawDefaultValue |]
                let csvPath = (inp.LiteralValue :?> string).Replace(@"\", @"\\")
                let csvFile = tp.ApplyStaticArguments(td, [| "FSharp"; "Data"; sprintf "%s,Sample=\"%s\"" providerName csvPath |], sparams)
                let bindingFlags = BindingFlags.DeclaredOnly ||| BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.Public
                let getSampleMeth = csvFile.GetMethods(bindingFlags) |> Array.find (fun n -> n.Name = "GetSample")
                let getImpl = (fun _ -> tp.GetInvokerExpression(getSampleMeth, [| |])) 
                TyApp (OtherTyDef(getSampleMeth.ReturnType), [| |]), getImpl

            { new ISimpleProperty with 
                override __.Name              = inp.Name
                override __.IsStatic          = true
                override __.PropertyType      = rty 
                override __.IndexParameters   = [| |]
                override __.CustomAttributes = Seq.empty
                override __.GetMethod = Some { new ISimpleAssociatedMethod with member __.GetImplementation(parameters) = getImpl(parameters) }
                override __.SetMethod = None 
            } |> Property |> Some

        | _ -> None

    let csvResolver = { defaultChainResolver with MemberResolver = resolver }
    
    FileSystemProvider config
    |> Simplify
    |> Chain csvResolver
    |> Clone ("FSharp.Management", "FileSystemToCsv")
    |> Desimplify

//[<TypeProvider>]
//type FileSystemCsvProvider(config) = inherit TypeProviderExpression(FileSystemToCsvAndXml(config))

let ExpandStaticData (recognizer: 'T -> (ITypeProvider * string) option) (sourceTPInstance: ISimpleTypeProvider) =
    let resolver (_, memberInp: ISimpleMember) = 
        // ignore constructors, events, methods, and non-static properties
        match memberInp with
        | Constructor _ | Event _ | Method _ -> None
        | Property inp when not inp.IsStatic -> None
        | _ ->
        
        let inpName, inpValue =
            match memberInp with
            | Field inp -> inp.Name, inp.LiteralValue :?> 'T
            | Property inp -> 
                let value =
                    let getterExpr = inp.GetMethod.Value.GetImplementation([||])
                    Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation(getterExpr) :?> 'T
                inp.Name, value
            | _ -> failwith "unreachable"
                
        match recognizer inpValue with
        | None -> None      // only continue if recognizer condition passes
        | Some(targetTPInstance, staticArg) ->
        
        let rty, getImpl = 
            let ns = targetTPInstance.GetNamespaces() |> Seq.head
            let tdWithSampleParam = 
                List.head <|
                [ for td in ns.GetTypes() do
                    for sp in targetTPInstance.GetStaticParameters(td) do
                        if sp.Name = "Sample" then yield td ]
            let sparams = 
                [| for sp in targetTPInstance.GetStaticParameters(tdWithSampleParam) do 
                    if sp.Name = "Sample" then 
                        yield staticArg :> obj
                    else 
                        assert sp.IsOptional 
                        yield sp.RawDefaultValue |]
            let tdWithAppliedParams = 
                targetTPInstance.ApplyStaticArguments(
                    tdWithSampleParam, 
                    Array.append (ns.NamespaceName.Split([|'.'|])) [| sprintf "%s,Sample=\"%s\"" tdWithSampleParam.Name staticArg |], 
                    sparams)
            let bindingFlags = BindingFlags.DeclaredOnly ||| BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.Public
            let getSampleMeth = tdWithAppliedParams.GetMethods(bindingFlags) |> Array.find (fun n -> n.Name = "GetSample")
            let getImpl = (fun _ -> targetTPInstance.GetInvokerExpression(getSampleMeth, [| |])) 
            TyApp (OtherTyDef(getSampleMeth.ReturnType), [| |]), getImpl

        { new ISimpleProperty with 
            override __.Name              = inpName
            override __.IsStatic          = true
            override __.PropertyType      = rty 
            override __.IndexParameters   = [| |]
            override __.CustomAttributes = Seq.empty
            override __.GetMethod = Some { new ISimpleAssociatedMethod with member __.GetImplementation(parameters) = getImpl(parameters) }
            override __.SetMethod = None 
        } |> Property |> Some

    let targetResolver = { defaultChainResolver with MemberResolver = resolver }

    sourceTPInstance
    |> Chain targetResolver

let (+++) r1 r2 = (fun s -> match r1 s with Some tp -> Some tp | None -> r2 s)

let fs2csv config = 
    let filesystemTP = FileSystemProvider config
    let csvTP = CsvProvider config
    
    filesystemTP
    |> Simplify
    |> ExpandStaticData ((fun (s:string) -> if s.EndsWith(".csv") then Some (csvTP, s.Replace(@"\",@"\\")) else None))
  //                       (fun (s:string) -> if s.EndsWith(".xml") then Some (XmlProvider, s.Replace(@"\",@"\\")) else None))
    |> Clone ("FSharp.Management", "FileSystemToCsv")
    |> Desimplify

[<TypeProvider>]
type MyChainedProvider(config) = inherit TypeProviderExpression(fs2csv config)

[<assembly:TypeProviderAssembly>] 
do()
