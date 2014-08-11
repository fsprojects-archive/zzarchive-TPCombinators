module FSharp.ProvidedTypes.ChainCombinator

open System
open System.Text
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Linq.Expressions
open System.Collections.Generic
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open FSharp.ProvidedTypes.GeneralCombinators
open FSharp.ProvidedTypes.SimplifiedAlgebra


// PropertyInfo = name + static type + how to evaluate = (string * Type * how-to-evaluate)



//let Chain(tp1: ITypeProvider, resolver: (PropertyInfo -> (string * Type) list), ?propNameFilter:string) = 
//let Chain(tp1: ITypeProvider, resolver: (PropertyInfo -> PropertyInfo list)) = 
//let Chain(tp1: ITypeProvider, resolver: (string -> Type option)) = 

// Chain(FS, (fun obj -> [ "freebase", FBType ]))
// Chain(DBP, "sameAs", "freebase", (fun stringArray -> [ "freebase", FBType ]))

// MINIMUM NECESSARY FOR FS 
//
//    P : string where P ~ "*.csv" --> CSV
//
//let Chain(tp1: ITypeProvider, propNameRegEx:string, resolver: (valueOfProperty:string -> (Type * (Expr -> Expr) )) = 




// MINIMUM NECESSARY FOR DB "sameAs" --> FB
//
//let Chain(tp1: ISimpleTypeProvider, origPropName:string, producedPropName:string, resolver: (staticValueOfProp:obj -> producedPropStaticType:Type * (Expr -> Expr)))) = 

// For a type provider tp1 that (optionally?) uses the GetDataContext or GetSample pattern,
// transform all (instance) properties named 'origPropName' to produce a new property 'producedPropName' by
//   1. evaluating the property statically in a default data context to give staticValueOfProp
//   2. calling resolver with this value to give producedPropStaticType
//   3.  of type 'SimplifiedType' whose implementation is given by 'getImpl'
//
//let Chain(tp1: ITypeProvider, origPropName:string, producedPropName:string, staticPart: (string[] -> SimplifiedType * getImpl:(Expr[] -> Expr)))) = 
//
//    GetProperties() -> 
//        [ for p in tp1.GetProperties() do 
//             yield p
//             if p = origPropName r then 
//                yield makeProperty (producedName, resolver (eval p))
//        ]
// 
//    GetProperty(reqPropName) -> 
//        if reqPropName = producedName then 
//            makeProperty (producedName, resolver (eval p), 
//                          getImplementation=(fun args -> getImpl(args)   ))
//        else
//            tp1.GetProperty(reqPropName)
//
//    GetInvokerExpression(methodBase,args) -> 
//        match getInvoker(methodBase,args) with 
//        | Some res -> res 
//        | None -> tp1.GetInvokerExpresion(methodBase,args)


type ChainResolver<'T> =
  { ContextCreator : obj[] * ISimpleTypeDefinition -> 'T option
    FieldResolver : 'T option * ISimpleLiteralField -> ISimpleLiteralField option
    PropertyResolver : 'T option * ISimpleProperty -> ISimpleProperty option
    EventResolver : 'T option * ISimpleEvent -> ISimpleEvent option }

let defaultChainResolver =
  { ContextCreator      = fun _ -> None
    FieldResolver       = fun _ -> None
    PropertyResolver    = fun _ -> None
    EventResolver       = fun _ -> None }

/// Chains two or more type providers together by replacing fields or properties of types in 'tp' by other types, controlled by the implementation of 'resolver'
let Chain<'T> (resolver : ChainResolver<'T>) (tp: ISimpleTypeProvider) =

    let thisAssembly = typedefof<Utils.IWraps<_>>.Assembly

    // A table tracking how wrapped type definition objects are translated to cloned objects.
    let txTable = TxTable<ISimpleTypeDefinition, ISimpleTypeDefinition>()

    let TxAssembly (a:Assembly) = thisAssembly 

    let TxStaticParameter(inp: ISimpleStaticParameter) = inp

    let rec TxParameter(inp : ISimpleParameter) = 
        { new ISimpleParameter with 

            override __.Name = inp.Name 
            override __.ParameterType = inp.ParameterType |> TxType
            override __.OptionalValue = inp.OptionalValue 
            override __.IsOut = inp.IsOut 
            override __.CustomAttributes = inp.CustomAttributes  
        }
 
    and TxConstructor(inp: ISimpleConstructor) = 
        { new ISimpleConstructor with

            override __.Parameters = inp.Parameters |> Array.map TxParameter
            override __.CustomAttributes = inp.CustomAttributes 
            override __.GetImplementation(parameters) = inp.GetImplementation(parameters)
        }

    and FindGoverningTransObj declTy  =
            let rec loop (ty:ISimpleTypeDefinition) = 
                match ty.DeclaringType with 
                | Some t -> loop t
                | None -> ty
            match loop declTy with 
            | :? IWraps<'T option> as state ->  state.Value
            | _ -> None

    and TxMethod declTy (inp: ISimpleMethod) =

        { new ISimpleMethod with 
            override __.Name              = inp.Name  
            override __.IsStatic          = inp.IsStatic  
            override __.Parameters        = inp.Parameters |> Array.map TxParameter
            override __.ReturnType        = inp.ReturnType  |> TxType
            override __.CustomAttributes = inp.CustomAttributes 
            override __.GetImplementation(parameters) = inp.GetImplementation(parameters)
        }

    and TxAssociatedMethod (inp: ISimpleAssociatedMethod) =
        { new ISimpleAssociatedMethod with 
            override __.GetImplementation(parameters) = inp.GetImplementation(parameters)
        }


    and TxProperty declTy (inp: ISimpleProperty) = 

      let trans = resolver.PropertyResolver (FindGoverningTransObj declTy, inp) 
      match trans with
      | Some res -> res     // this result should not be transformed
      | None ->

        { new ISimpleProperty with 

            override __.Name = inp.Name 
            override __.IsStatic = inp.IsStatic
            override __.PropertyType = inp.PropertyType |> TxType
            override __.GetMethod = inp.GetMethod |> Option.map TxAssociatedMethod
            override __.SetMethod = inp.SetMethod |> Option.map TxAssociatedMethod
            override __.IndexParameters = inp.IndexParameters |> Array.map TxParameter
            override __.CustomAttributes = inp.CustomAttributes 

        }

    and TxEventDefinition declTy (inp: ISimpleEvent) = 
        { new ISimpleEvent with 

            override __.Name = inp.Name 
            override __.IsStatic = inp.IsStatic
            override __.EventHandlerType = inp.EventHandlerType |> TxType
            override __.AddMethod = inp.AddMethod |> TxAssociatedMethod 
            override __.RemoveMethod = inp.RemoveMethod |> TxAssociatedMethod 
            override __.CustomAttributes = inp.CustomAttributes 
        }

    and TxField declTy (inp: ISimpleLiteralField) = 
        let trans = resolver.FieldResolver (FindGoverningTransObj declTy, inp) 
        match trans with
        | Some res -> res     // this result should not be transformed
        | None ->

        { new ISimpleLiteralField with 

            override __.Name = inp.Name 
            override __.FieldType = inp.FieldType |> TxType
            override __.LiteralValue  = inp.LiteralValue 
            override __.CustomAttributes = inp.CustomAttributes 
        }

    and TxMember this (inp: ISimpleMember) =
        match inp with
        | Constructor ctor -> Constructor(TxConstructor ctor)
        | Method meth -> Method(TxMethod this meth)
        | Field fld -> Field(TxField this fld)
        | Property prop -> Property(TxProperty this prop)
        | Event evt -> Event(TxEventDefinition this evt)

    and TxType(inp: ISimpleType) = 
        match inp with 
        | TyApp(td, args) -> TyApp(TxTypeDefinitionReference td, Array.map TxType args)
        | TyArray(n, arg) -> TyArray(n, TxType arg)
        | TyPointer(arg) -> TyPointer(TxType arg)
        | TyByRef(arg) -> TyByRef(TxType arg)

    and TxTypeDefinitionReference inp = 
        match inp with
        | ISimpleTypeDefinitionReference.OtherTyDef _ -> inp
        | ISimpleTypeDefinitionReference.SimpleTyDef x -> SimpleTyDef (TxTypeDefinition None x)

    and TxTypeDefinition transState (inp: ISimpleTypeDefinition) =
      txTable.Get inp <| fun () ->

        { new ISimpleTypeDefinition with 
            override __.Name = inp.Name 
            override __.Assembly = inp.Assembly |> TxAssembly 
            override __.Namespace = inp.Namespace
            override __.DeclaringType = inp.DeclaringType |> Option.map (TxTypeDefinition None)

            override __.BaseType = inp.BaseType |> Option.map TxType
            override __.Interfaces = inp.Interfaces |> Array.map TxType

            override this.Members = inp.Members |> Array.map (TxMember this)
            override __.NestedTypes = inp.NestedTypes |> Array.map (TxTypeDefinition None)
            override __.GetNestedType(name, declaredOnly) = inp.GetNestedType(name, declaredOnly) |> Option.map (TxTypeDefinition None)

            override __.CustomAttributes = inp.CustomAttributes 
            override __.ApplyStaticArguments(typePathWithArguments, objs) = 
                let inpApplied = inp.ApplyStaticArguments(typePathWithArguments, objs) 
                let dataContextObjOpt = resolver.ContextCreator (objs, inpApplied)
                let inpWrapped = inpApplied |> TxTypeDefinition dataContextObjOpt
                inpWrapped

            override __.StaticParameters = inp.StaticParameters |> Array.map TxStaticParameter
          interface IWraps<'T option> with 
              member __.Value = transState
        }

    /// Transform a provided namespace definition
    let rec TxNamespaceDefinition (inp: ISimpleNamespace) = 
        { new ISimpleNamespace with
            override __.NestedNamespaces = inp.NestedNamespaces |> Array.map TxNamespaceDefinition
            override __.NamespaceName = inp.NamespaceName 
            override __.TypeDefinitions = 
                inp.TypeDefinitions 
                |> Array.map (fun ty -> 
                    let ctxtObjOpt = resolver.ContextCreator ([||], ty)
                    ty |> TxTypeDefinition ctxtObjOpt )
            override __.GetTypeDefinition(name) =  inp.GetTypeDefinition(name) |> Option.map (TxTypeDefinition None)
         }

    /// Transform an input ITypeProvider
    let TxTypeProviderDefinition (inp: ISimpleTypeProvider) = 
        { new ISimpleTypeProvider with 
            override __.Namespaces = inp.Namespaces |> Array.map TxNamespaceDefinition

            [<CLIEvent>]
            override __.Invalidate = inp.Invalidate 

          interface System.IDisposable with 
            override x.Dispose() = inp.Dispose()
        }
    
    TxTypeProviderDefinition(tp)



