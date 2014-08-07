module ChainExamples

open System.Reflection
open FSharp.ProvidedTypes.CloneCombinatorOverSimplifiedAlgebra
open FSharp.ProvidedTypes.ChainCombinator
open FSharp.ProvidedTypes.SimplifiedAlgebra
open FSharp.ProvidedTypes.GeneralCombinators
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations

//FSharp.Data.DbPedia.GetDataContext().Ontology.Activity.Game.Individuals.``7 Wonders (board game)``.
//let r = FSharp.Data.DbPediaSearch<"Person","Antoine Bauza">.SearchResults()

let CsvProvider config =
    let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
    new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location)) |> Simplify


let FreebaseProvider config =
    let FSharpDataAssembly = typeof<FSharp.Data.Runtime.Freebase.FreebaseObject>.Assembly
    new ProviderImplementation.FreebaseTypeProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))  :> ITypeProvider
    

let DbPediaProvider config =
    let FSharpDataDbPediaAssembly = typeof<FSharp.Data.DbPedia>.Assembly
    new FSharp.Data.DbPediaTypeProvider.DbPediaTypeProvider(ConfigForOtherTypeProvider(config, FSharpDataDbPediaAssembly.Location)) |> Simplify

(*
// In this example, we chain two type providers:
//
//     CsvProvider --> DbPediaProvider
//
// such that any string value in the CSV file containing a DBpedia resource link will be replaced with a provided type
let CsvDbPedia config = 

    let dbPediaProvider = DbPediaProvider config
    //let RenamedCsvProvider = Clone("FSharp.Data", "Combined", CsvProvider)
    //Chain(RenamedCsvProvider, DbPediaProvider)
    let resolver = dbPediaProvider.FreebaseToDbPedia >> (function None -> None | Some uri -> DbPediaProvider.GetTypeByUri uri)
    
    Chain(CsvProvider, resolver)

[<TypeProvider>]
type CsvDbPediaProvider(config) = inherit TypeProviderExpression(CsvDbPedia(config))
*)

let c = FSharp.Data.DbPedia.GetDataContext()

(*
c.Ontology.Holiday.Individuals.``Anzac Day``.``abstract``

<@ c.Ontology @>

_c = FSharp.Data.DbPedia.GetDataContext()
_cOntology = _c.Ontology
_cHoliday = _cOntology.Holiday
_cIndividuals = _cHoliday.Individuals
_c``Anzac Day`` = _cIndividuals.``Anzac Day``
*)

let v = c.Ontology.Holiday.Individuals.``Anzac Day``.``abstract``

//open Microsoft.FSharp.Quotations
//let ffff (h: Expr<FSharp.Data.DbPedia.ServiceTypes.Anzac_DayType>) = <@@ (%h).``abstract`` @@>
//
//ffff <@ failwith "a" @>

//type Foo = FSharp.Data.CsvProvider<
let Lengthify config = 
    let dbPediaProvider = DbPediaProvider config 

    
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



(*
        let ty, getImpl = 

            let tp = CsvProvider config
            let staticValueOfProp = "data/Titanic.csv"
            let n1 = tp.Namespaces |> Array.find (fun n -> n.NamespaceName = "FSharp.Data") 
            let std = n1.TypeDefinitions |> Array.find (fun n -> n.Name = "CsvProvider") 
            let sparams = [| for sp in std.StaticParameters do 
                                    if sp.Name = "Sample" then 
                                        yield (box staticValueOfProp) 
                                    else 
                                        assert sp.OptionalValue.IsSome 
                                        yield sp.OptionalValue.Value |]

            let td = std.ApplyStaticArguments( [| "FSharp"; "Data"; "random_" + staticValueOfProp |], sparams)
            let getSampleMeth = td.Methods |> Array.find (fun n -> n.Name = "GetSample")
            let getImpl = (fun _ -> getSampleMeth.GetImplementation [| |]) // GetSample expects no arguments
            TyApp (SimpleTyDef(td), [| |]), getImpl
*)

        let ty, getImpl = 

            let tp = FreebaseProvider config
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

    let chainResolver = { (defaultResolver contextCreator) with PropertyResolver = resolver }
        
    Chain(dbPediaProvider, chainResolver) |> Clone ("FSharp.Data", "Chained")

[<TypeProvider>]
type CsvDbPediaProvider(config) = inherit TypeProviderExpression(Lengthify(config) |> Desimplify)


[<assembly:TypeProviderAssembly>] 
do()