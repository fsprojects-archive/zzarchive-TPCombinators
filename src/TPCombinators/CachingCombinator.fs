module FSharp.ProvidedTypes.CachingCombinator

open System
open System.Text
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Linq.Expressions
open System.Collections.Generic
open Microsoft.FSharp.Core.CompilerServices
open FSharp.ProvidedTypes.GeneralCombinators
open FSharp.ProvidedTypes.SimplifiedAlgebra

type System.String with 
    member s.ReplacePrefix (s1:string, s2:string) =  
        if s.StartsWith(s1) then s2 + s.[s1.Length..] else s

type CachedField =
    {
        Name: string
        FieldType: ISimpleType
        LiteralValue: obj
    }

type CachedTypeDefiniton = 
    {
        Name: string
        DeclaringType: CachedTypeDefiniton option
        Fields: seq<CachedField>
        NestedTypes: seq<CachedTypeDefiniton>
    }

type CachedThing =
    | CField of CachedField
    | CTypeDef of CachedTypeDefiniton

let Cache (tp: ISimpleTypeProvider) = 

    let thisAssembly = typedefof<Utils.IWraps<_>>.Assembly

    // A table tracking how wrapped type definition objects are translated to cloned objects.
    let txTable = TxTable<ISimpleTypeDefinition, ISimpleTypeDefinition>()

    // The transformation we perform on the assembly. isTarget indicates if this is a provided object we should transform. 
    let TxAssembly (a:Assembly) = thisAssembly 

    let TxCustomAttributes (inp: seq<CustomAttributeData>) = inp

    let rec TxStaticParameter(inp: ISimpleStaticParameter) = 

        { new ISimpleStaticParameter with 

            override __.Name = inp.Name
            override __.ParameterType = inp.ParameterType
            override __.OptionalValue = inp.OptionalValue
            override __.CustomAttributes = inp.CustomAttributes  |> TxCustomAttributes
        }


    and TxParameter(inp : ISimpleParameter) = 
        { new ISimpleParameter with 

            override __.Name = inp.Name 
            override __.ParameterType = inp.ParameterType |> TxType
            override __.OptionalValue = inp.OptionalValue 
            override __.IsOut = inp.IsOut 
            override __.CustomAttributes = inp.CustomAttributes  |> TxCustomAttributes
        }
 
    and TxConstructor(inp: ISimpleConstructor) = 
        { new ISimpleConstructor with

            override __.Parameters = inp.Parameters |> Array.map TxParameter
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes
            override __.GetImplementation(parameters) = inp.GetImplementation(parameters)
        }

    and TxMethod(inp: ISimpleMethod) =
        { new ISimpleMethod with 

            override __.Name              = inp.Name  
            override __.IsStatic          = inp.IsStatic  
            override __.Parameters        = inp.Parameters |> Array.map TxParameter
            override __.ReturnType        = inp.ReturnType  |> TxType
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes
            override __.GetImplementation(parameters) = inp.GetImplementation(parameters)
        }

    and TxAssociatedMethod(inp: ISimpleAssociatedMethod) =
        { new ISimpleAssociatedMethod with 
            override __.GetImplementation(parameters) = inp.GetImplementation(parameters)
        }


    and TxProperty(inp: ISimpleProperty) = 
        { new ISimpleProperty with 

            override __.Name = inp.Name 
            override __.IsStatic = inp.IsStatic 
            override __.PropertyType = inp.PropertyType |> TxType
            override __.GetMethod = inp.GetMethod |> Option.map TxAssociatedMethod
            override __.SetMethod = inp.SetMethod |> Option.map TxAssociatedMethod
            override __.IndexParameters = inp.IndexParameters |> Array.map TxParameter
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes

        }

    and TxEventDefinition(inp: ISimpleEvent) = 
        { new ISimpleEvent with 

            override __.Name = inp.Name 
            override __.IsStatic = inp.IsStatic 
            override __.EventHandlerType = inp.EventHandlerType |> TxType
            override __.AddMethod = inp.AddMethod |> TxAssociatedMethod
            override __.RemoveMethod = inp.RemoveMethod |> TxAssociatedMethod
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes
        }

    and TxField(inp: ISimpleLiteralField) = 
        { new ISimpleLiteralField with 

            override __.Name = inp.Name 
            override __.FieldType = inp.FieldType |> TxType
            override __.LiteralValue  = inp.LiteralValue 
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes
        }

    and TxMember(inp: ISimpleMember) =
        match inp with
        | Constructor ctor -> Constructor(TxConstructor ctor)
        | Method meth -> Method(TxMethod meth)
        | Field fld -> Field(TxField fld)
        | Property prop -> Property(TxProperty prop)
        | Event evt -> Event(TxEventDefinition evt)

    and TxType(inp: ISimpleType) = 
        match inp with 
        | TyApp(td, args) -> TyApp(TxTypeDefinitionReference td, Array.map TxType args)
        | TyArray(n, arg) -> TyArray(n, TxType arg)
        | TyPointer(arg) -> TyPointer(TxType arg)
        | TyByRef(arg) -> TyByRef(TxType arg)

    and TxTypeDefinitionReference inp = 
        match inp with
        | ISimpleTypeDefinitionReference.OtherTyDef _ -> inp
        | ISimpleTypeDefinitionReference.SimpleTyDef x -> SimpleTyDef (TxTypeDefinition x)

    and TxTypeDefinition(inp: ISimpleTypeDefinition) =
      txTable.Get inp <| fun () ->

        { new ISimpleTypeDefinition with 
            override __.Name = inp.Name 
            override __.Assembly = inp.Assembly |> TxAssembly 
            override __.Namespace = inp.Namespace
            override __.DeclaringType = inp.DeclaringType |> Option.map TxTypeDefinition

            override __.BaseType = inp.BaseType |> Option.map TxType
            override __.Interfaces = inp.Interfaces |> Array.map TxType

            override __.Members = inp.Members |> Array.map TxMember
            override __.NestedTypes = inp.NestedTypes |> Array.map TxTypeDefinition
            override __.GetNestedType(name, declaredOnly) = inp.GetNestedType(name, declaredOnly) |> Option.map TxTypeDefinition

            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes
            override __.ApplyStaticArguments(typePathWithArguments, objs) = inp.ApplyStaticArguments(typePathWithArguments, objs) |> TxTypeDefinition
            override __.StaticParameters = inp.StaticParameters |> Array.map TxStaticParameter
        }

    /// Transform a provided namespace definition
    let rec TxNamespaceDefinition (inp: ISimpleNamespace) = 
        { new ISimpleNamespace with
            override __.NestedNamespaces = inp.NestedNamespaces |> Array.map TxNamespaceDefinition
            override __.NamespaceName = inp.NamespaceName
            override __.TypeDefinitions = inp.TypeDefinitions |> Array.map TxTypeDefinition
            override __.GetTypeDefinition(name) =  inp.GetTypeDefinition(name) |> Option.map TxTypeDefinition
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
  
    let CacheField (inp: ISimpleLiteralField) =
        {
            Name = inp.Name;
            FieldType = inp.FieldType;
            LiteralValue = inp.LiteralValue
        }

    let rec CacheTypeDefinition (cache: CachedTypeDefiniton) (inp: ISimpleTypeDefinition) =
        {
            Name = inp.Name;
            DeclaringType = cache.DeclaringType;
            Fields = inp.Fields |> Seq.map CacheField
            NestedTypes = inp.NestedTypes |> Seq.map (CacheTypeDefinition cache)
            
        }
    
    let CacheNamespaceDefinition (cache: CachedTypeDefiniton) (inp: ISimpleNamespace) =
        {
            Name = inp.NamespaceName;
            DeclaringType = Some cache;
            Fields = Seq.empty
            NestedTypes = inp.TypeDefinitions |> Seq.map (CacheTypeDefinition cache)
        }

    let CacheTypeProviderDefinition (cache: CachedTypeDefiniton) (inp: ISimpleTypeProvider) =
        {
            Name = inp.ToString();
            DeclaringType = cache.DeclaringType;
            Fields = Seq.empty;
            NestedTypes = inp.Namespaces |> Seq.map (CacheNamespaceDefinition cache)
        }
    
    
    let a = TxTypeProviderDefinition(tp)
    
    let emptyCache = 
        {Name="Sysobj"; DeclaringType=None; Fields=Seq.empty; NestedTypes=Seq.empty}
    let cache = a |> (CacheTypeProviderDefinition emptyCache)

    printfn "%A" cache

    a