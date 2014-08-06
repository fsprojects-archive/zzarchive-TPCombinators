module ChainExamples

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

let Lengthify config = 
    let dbPediaProvider = DbPediaProvider config 
    let contextCreator (staticArgumentValues:obj[], inpApplied : ISimpleTypeDefinition) =
        try
                let dataContextMethod =
                    inpApplied.Methods
                    |> Array.find (fun m -> m.IsStatic && (m.Name = "GetSample" || m.Name = "GetDataContext"))
                let expr = dataContextMethod.GetImplementation [| |]
                let dataContextObj = Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation(expr)
                let dataContext = dataContextObj :?> DbPediaAccess.DbPediaDataContextBase
                dataContext.Connection
        with
        | :? System.Collections.Generic.KeyNotFoundException -> 
            raise (DataContextMethodNotFound(sprintf "The provided type '%s' requires the static method GetDataContext() or GetSample()" inpApplied.Name))
              

    let resolver (connObj:DbPediaAccess.DbPediaConnection,  inp: ISimpleProperty) = 

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

        let n = staticValueOfProp.Length
        let rty = typeof<int>
        let getImpl = (fun _ -> <@@ n @@>) 
        { new ISimpleProperty with 
            override __.Name              = inp.Name  + "_look_ma_i_know_it_has_length_" + string n
            override __.IsStatic          = inp.IsStatic  
            override __.PropertyType          = TyApp(OtherTyDef rty, [| |])
            override __.IndexParameters        = inp.IndexParameters
            override __.CustomAttributes = inp.CustomAttributes 
            override __.GetMethod = Some { new ISimpleAssociatedMethod with member __.GetImplementation(parameters) = getImpl(parameters) }
            override __.SetMethod = None 
        }
        
    
    Clone("FSharp.Data", "Chained", Chain(dbPediaProvider, "abstract", contextCreator, resolver))

[<TypeProvider>]
type CsvDbPediaProvider(config) = inherit TypeProviderExpression(Lengthify(config) |> Desimplify)


[<assembly:TypeProviderAssembly>] 
do()

